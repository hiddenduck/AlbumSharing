import io.grpc.ManagedChannelBuilder;
import io.grpc.ServerBuilder;
import file.ServerInfo;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.Socket;


public class Main {
    public static void main(String[] args) throws IOException, InterruptedException, InvalidTargetServerException {
        if(args.length!=3){
            System.out.println("Wrong Arguments! [PORT] [Central Server IP] [Central Server Port]");
            return;
        }

        String port = args[0];
        String central_Ip = args[1];
        String central_port = args[2];


        var folder = String.valueOf(port);
        File directory = new File(folder);

        if(!directory.exists()){
            directory.mkdir();
        }

        ServerInfo serverInfo = null;
        try (Socket s = new Socket(central_Ip, Integer.parseInt(central_port), null, Integer.parseInt(port))) {
            // Waiting join response...
            var inputStream = s.getInputStream();
            byte[] messageBytes = new byte[1024];
            inputStream.read(messageBytes);
            serverInfo = ServerInfo.parseFrom(messageBytes);
        }
        if(serverInfo== null) throw new InvalidTargetServerException("Target Server values are null");

        byte[] my_hash = serverInfo.getMyHash().toByteArray();

        if(!serverInfo.getIp().equals("")) {
            var connection = ManagedChannelBuilder.forAddress(serverInfo.getIp(), serverInfo.getPort())
                    .usePlaintext()
                    .build();

            var stub = file.Rx3FileGrpc.newRxStub(connection);

            file.TransferMessage request = file.TransferMessage.newBuilder()
                    .setHashKey(serverInfo.getMyHash())
                    .setInfHash(serverInfo.getInfHash())
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

        ServerBuilder.forPort(Integer.parseInt(args[0]))
                .addService(new FileService(folder, my_hash))
                .build()
                .start()
                .awaitTermination();
    }
}
