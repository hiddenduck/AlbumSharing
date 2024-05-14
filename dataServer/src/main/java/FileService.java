import com.google.protobuf.ByteString;
import file.DownloadMessage;
import file.FileMessage;
import file.Rx3FileGrpc;
import file.UploadMessage;
import io.grpc.ManagedChannelBuilder;
import io.grpc.Status;
import io.reactivex.rxjava3.core.*;
import io.reactivex.rxjava3.schedulers.Schedulers;
import java.io.File;
import java.io.*;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.util.Objects;

public class FileService extends Rx3FileGrpc.FileImplBase {

    private String folder;

    private int chunkSize = 4096; // Also Defined in GO


    public FileService(Boolean isInitial, int port, String central_Ip, int central_port) throws IOException, InvalidTargetServerException {
        this.folder = String.valueOf(port);
        File directory = new File(folder);

        if(!directory.exists()){
            directory.mkdir();
        }

        if(!isInitial) {
            file.ServerInfo serverInfo = null;
            try (Socket s = new Socket(central_Ip, central_port)) {
                // Waiting join response...
                var inputStream = s.getInputStream();
                byte[] prefix = new byte[8];
                inputStream.read(prefix);
                int messageSize = ByteBuffer.wrap(prefix).getInt();
                byte[] messageBytes = new byte[messageSize];
                inputStream.read(messageBytes);
                serverInfo = file.ServerInfo.parseFrom(messageBytes);
            }
            if(serverInfo== null) throw new InvalidTargetServerException("Target Server values are null");

            var connection = ManagedChannelBuilder.forAddress(serverInfo.getIp(), serverInfo.getPort())
                    .usePlaintext()
                    .build();

            var stub = Rx3FileGrpc.newRxStub(connection);

            DownloadMessage request = DownloadMessage.newBuilder()
                    .setHashKey(serverInfo.getHash())
                    .build();

            stub.transfer(request).blockingSubscribe(
                    fileMessage -> {
                        byte[] data = fileMessage.getData().toByteArray();
                        String hash = fileMessage.getHashKey().toStringUtf8();

                        try (FileOutputStream writer = new FileOutputStream(hash, true)) {
                            writer.write(data);
                            writer.flush();
                        } catch (IOException e) {
                            new File(hash).delete();
                            throw new ErrorInTransferException("Error while transfering file with hash: " + hash);
                        }
                    },
                    throwable -> {
                        throw new ErrorInTransferException(throwable.getMessage());
                    },
                    () -> System.out.println("File transfer completed! :)")
            );

        }

    }

	/**
     * Opens the file and create the stream.
     * @param filePath Path for the file to be downloaded.
     * @return Stream of data chunks of the file.
     */
    private Flowable<byte[]> openFileToStream(String filePath) {

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
        String filePath = request.getHashKey().toStringUtf8();
        return openFileToStream(filePath)
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

    public Flowable<FileMessage> transfer(DownloadMessage request){
        File directory = new File(this.folder);
        String[] filesNames = directory.list();
        if(filesNames==null) filesNames = new String[]{};

        return Flowable.fromArray(filesNames).flatMap(fileName -> openFileToStream(fileName)
                .map(n -> FileMessage.newBuilder().setData(ByteString.copyFrom(n)).build())).subscribeOn(Schedulers.io());
    }

}