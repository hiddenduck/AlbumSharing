import io.grpc.ServerBuilder;

import java.io.IOException;


public class Main {
    public static void main(String[] args) throws IOException, InterruptedException, InvalidTargetServerException {
        if(args.length!=3){
            System.out.println("Wrong Arguments! [PORT] [Central Server IP] [Central Server Port]");
            return;
        }

        ServerBuilder.forPort(Integer.parseInt(args[0]))
                .addService(new FileService(Integer.parseInt(args[0]), args[1], Integer.parseInt(args[2])))
                .build()
                .start()
                .awaitTermination();
    }
}
