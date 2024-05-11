import com.google.protobuf.ByteString;
import file.DownloadMessage;
import file.FileMessage;
import file.Rx3FileGrpc;
import file.UploadMessage;
import file.joinMessage;
import io.grpc.Status;
import io.reactivex.rxjava3.core.*;
import io.reactivex.rxjava3.processors.PublishProcessor;
import io.reactivex.rxjava3.schedulers.Schedulers;
import java.io.File;
import java.io.*;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.concurrent.Flow;

public class FileService extends Rx3FileGrpc.FileImplBase {

    private String folder;

    private int chunkSize = 4096; // Also Defined in GO


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
    private Flowable<byte[]> openFileToStream(DownloadMessage request) {
        String filePath = request.getHashKey().toStringUtf8();

        if (!new java.io.File(this.folder, filePath).exists()) {
            System.out.println("Error in opening file");
            return Flowable.error(io.grpc.Status.NOT_FOUND
                    .withDescription("File not found")
                    .asRuntimeException());
        }

        return Flowable.create(sub -> {
            try(var reader = new FileInputStream(new File(folder, filePath))) {
                byte[] buffer = new byte[chunkSize];
                int read;
                while ((read = reader.read(buffer)) != -1) {
                    byte[] readBuff = new byte[read];
                    System.arraycopy(buffer, 0, readBuff, 0, read);
                    sub.onNext(readBuff);
                }
                sub.onComplete();
            } catch (IOException e){
                sub.onError(Status.ABORTED.asRuntimeException());
            }
        }, BackpressureStrategy.DROP);
    }

	/**
     * Implements the service of file download.
     * @param request Request message for download.
     * @return Message Stream.
     */
    @Override
    public Flowable<FileMessage> download(DownloadMessage request) {
        return openFileToStream(request)
                .subscribeOn(Schedulers.io())
                .map(n -> FileMessage.newBuilder().setData(ByteString.copyFrom(n)).build());
    }

	/**
     * Implements the service of file upload.
     * @param request Message Stream.
     * @return Single with confirmation message of the upload.
     */
    public Flowable<UploadMessage> upload(Flowable<FileMessage> request) {

        PublishProcessor<FileMessage> processor = PublishProcessor.create();

        request.subscribe(processor);

        var f = processor
                .observeOn(Schedulers.io())
                .flatMap(message -> {
                    String filePath = this.folder + File.separator + message.getHashKey().toStringUtf8();
                    byte[] data = message.getData().toByteArray();

                    try (FileOutputStream writer = new FileOutputStream(filePath, true)) {
                        writer.write(data);
                        writer.flush();
                        return Flowable.just(UploadMessage.newBuilder().build());
                    } catch (IOException e) {
                        return Flowable.error(Status.ABORTED.asRuntimeException());
                    }
                });

        return processor.take(1).observeOn(Schedulers.io()).flatMap(message -> {
            if(!new java.io.File(this.folder, message.getHashKey().toStringUtf8()).exists()){
                return f;
            } else{
                return Flowable.error(Status.ALREADY_EXISTS.asRuntimeException());
            }
        });

    }

}
