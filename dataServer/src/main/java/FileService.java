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
import java.util.Objects;
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

        return Flowable.generate(
                () -> {
                    FileInputStream reader = new FileInputStream(new File(folder, filePath));
                    return reader;
                },
                (reader, emitter) -> {
                    try {
                        byte[] buffer = new byte[chunkSize];
                        int read = reader.read(buffer);
                        if (read == -1) {
                            reader.close();
                            emitter.onComplete();
                        } else {
                            byte[] readBuff = new byte[read];
                            System.arraycopy(buffer, 0, readBuff, 0, read);
                            emitter.onNext(readBuff);
                        }
                    } catch (IOException e) {
                        reader.close();
                        emitter.onError(e);
                    }
                    return reader;
                },
                reader -> {
                    try {
                        reader.close();
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }
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
                .subscribeOn(Schedulers.io())
                .map(n -> FileMessage.newBuilder().setData(ByteString.copyFrom(n)).build());
    }

    class FileWriter {
        private FileOutputStream writer = null;
        private File file = null;

        public String createIfDontExist(String fileName){
            if(this.writer==null) {
                this.file = new java.io.File(fileName);
                if(file.exists())
                    return "E"; // already exists
                try {
                    this.writer = new FileOutputStream(fileName, true);
                } catch (IOException e){
                    file.delete();
                    return "A";
                }
            }
            return "";
        }

        public boolean writeData(byte[] data){
            try {
                this.writer.write(data);
                this.writer.flush();
            } catch (IOException e){
                this.file.delete();
                return false;
            }

            return true;
        }
    }

	/**
     * Implements the service of file upload.
     * @param request Message Stream.
     * @return Single with confirmation message of the upload.
     */
    public Flowable<UploadMessage> upload(Flowable<FileMessage> request) {

        final FileWriter writer = new FileWriter();

        return request
                .flatMap(message -> {
                    String filePath = this.folder + File.separator + message.getHashKey().toStringUtf8();
                    String status = writer.createIfDontExist(filePath);
                    if(Objects.equals(status, "E")){
                        return Flowable.error(Status.ALREADY_EXISTS.asRuntimeException());
                    } else if(Objects.equals(status, "A")){
                        return Flowable.error(Status.ABORTED.asRuntimeException());
                    }

                    byte[] data = message.getData().toByteArray();

                    if(writer.writeData(data)){
                        return Flowable.just(UploadMessage.newBuilder().build());
                    } else {
                        return Flowable.error(Status.ABORTED.asRuntimeException());
                    }
                });
    }

}

/*

 */
