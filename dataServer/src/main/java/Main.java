import com.google.protobuf.ByteString;
import io.grpc.ManagedChannelBuilder;
import io.grpc.ServerBuilder;
import file.ServerInfo;

import java.io.*;
import java.net.Socket;
import java.util.Objects;


public class Main {
    private static byte[] hexToByteArray(String hex) {
        int len = hex.length();
        byte[] data = new byte[len / 2];
        for (int i = 0; i < len; i += 2) {
            data[i / 2] = (byte) ((Character.digit(hex.charAt(i), 16) << 4)
                    + Character.digit(hex.charAt(i+1), 16));
        }
        return data;
    }

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

        String message = "";
        try (Socket s = new Socket(central_Ip, Integer.parseInt(central_port));
             BufferedReader input = new BufferedReader(new InputStreamReader(s.getInputStream()));
             PrintWriter output = new PrintWriter(s.getOutputStream())) {
            output.println(args[0]);
            // Waiting join response...
            message = input.readLine();
            System.out.println(message);
        }

        byte[] my_hash;

        String[] messageParts = message.split(";", 2);

        if(!Objects.equals(messageParts[0], "")) {

            String IP = messageParts[0];
            String[] configs = messageParts[1].split(":", 4);
            int Port = Integer.parseInt(configs[0]);
            my_hash = hexToByteArray(configs[1]);
            byte[] infHash = hexToByteArray(configs[2]);

            var connection = ManagedChannelBuilder.forAddress(IP, Port)
                    .usePlaintext()
                    .build();

            var stub = file.Rx3FileGrpc.newRxStub(connection);

            file.TransferMessage request = file.TransferMessage.newBuilder()
                    .setHashKey(ByteString.copyFrom(my_hash))
                    .setInfHash(ByteString.copyFrom(infHash))
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
        } else {
            String[] configs = messageParts[1].split(":", 2);
            my_hash = hexToByteArray(configs[0]);
        }

        ServerBuilder.forPort(Integer.parseInt(args[0]))
                .addService(new FileService(folder, my_hash))
                .build()
                .start()
                .awaitTermination();
    }
}
