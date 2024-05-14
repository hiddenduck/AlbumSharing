import io.grpc.ServerBuilder;

import java.io.IOException;


public class Main {
    public static void main(String[] args) throws IOException, InterruptedException, InvalidTargetServerException {
        if(args.length!=4){
            System.out.println("Wrong Arguments! [is_Initial] [PORT] [Central Server IP] [Central Server Port]");
            return;
        }

        ServerBuilder.forPort(Integer.parseInt(args[1]))
                .addService(new FileService(Boolean.parseBoolean(args[0]), Integer.parseInt(args[1]), args[2], Integer.parseInt(args[3])))
                .build()
                .start()
                .awaitTermination();
    }
}
