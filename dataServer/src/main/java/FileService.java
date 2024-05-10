import com.google.protobuf.ByteString;
import file.DownloadMessage;
import file.FileMessage;
import file.Rx3FileGrpc;
import file.UploadMessage;
import file.joinMessage;
import io.reactivex.rxjava3.core.*;
import io.reactivex.rxjava3.schedulers.Schedulers;
import java.io.File;
import java.io.*;
import java.net.Socket;
import java.nio.ByteBuffer;

public class FileService extends Rx3FileGrpc.FileImplBase {

    private String folder;


    public FileService(int port, String central_Ip, int central_port) throws IOException {
        this.folder = "localhost" + String.valueOf(port);
        File directory = new File(folder);

        if(!directory.exists()){
            directory.mkdir();
        }
        /*
        try(Socket s = new Socket(central_Ip, central_port)){
            // Send join Msg to Central Server
            var joinMsg = joinMessage.newBuilder().setIp("localhost").setPort(port).build();
            var outputStream = s.getOutputStream();
            outputStream.write(joinMsg.toByteArray());
            outputStream.flush();

            // Wait join response
            var inputStream = s.getInputStream();
            byte[] prefix = new byte[4];
            inputStream.read(prefix);
            int messageSize = ByteBuffer.wrap(prefix).getInt();
            byte[] messageBytes = new byte[messageSize];
            inputStream.read(messageBytes);


        }
        */
    }

	/**
     * Opens the file and create the stream.
     * @param request Request message for download.
     * @return Stream.
     */
    private Flowable<String> openFileToStream(DownloadMessage request) {
        String filePath = request.getHashKey().toStringUtf8();

        if (!new java.io.File(this.folder, filePath).exists()) {
            System.out.println("Error in opening file");
            return Flowable.error(io.grpc.Status.NOT_FOUND
                    .withDescription("File not found")
                    .asRuntimeException());
        }


        int chunkSize = 2097152; // Also Defined in GO

        // Flowable using does 3 things:
        // 1: request a resource (BufferedReader)
        // 2: read lines from that resource
        // 3: when done, free resource by closing
        return Flowable.using(
                () -> new FileInputStream(new File(this.folder, filePath)),
                inputStream -> Flowable.generate(
                        emitter -> {
                            byte[] buffer = new byte[chunkSize];
                            int read = inputStream.read(buffer);
                            if(read != -1){
                                emitter.onNext(new String(buffer));
                            } else {
                                emitter.onComplete();
                            }
                        }
                ),
                InputStream::close
        );
    }

	/**
     * Implements the service of file download.
     * @param request Request message for download.
     * @return Message Stream.
     */
    @Override
    public Flowable<FileMessage> download(DownloadMessage request) {
        return openFileToStream(request)
                .observeOn(Schedulers.io())
                .map(n -> FileMessage.newBuilder().setData(ByteString.copyFromUtf8(n)).build());
    }

	/**
     * Implements the service of file upload.
     * @param request Message Stream.
     * @return Single with confirmation message of the upload.
     */
    public Single<UploadMessage> upload(Flowable<FileMessage> request) {
        var uploadResult = request
                .observeOn(Schedulers.io())
                .flatMap(message -> {
                    String filePath = this.folder + File.separator + message.getHashKey().toStringUtf8();
                    try (FileOutputStream writer = new FileOutputStream(filePath)) {
                        writer.write(message.getData().toByteArray());
						writer.flush();
                        return Flowable.just(UploadMessage.newBuilder().build());
                    } catch (IOException e) {
						return Flowable.error(io.grpc.Status.NOT_FOUND
                                .asRuntimeException());
                    }
                });

        return uploadResult.firstOrError();
    }

}
