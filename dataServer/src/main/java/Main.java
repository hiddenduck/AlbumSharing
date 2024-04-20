import io.grpc.ServerBuilder;

import java.io.IOException;


public class Main {
    public static void main(String[] args) throws IOException, InterruptedException {
        ServerBuilder.forPort(12345)
                .addService(new FileService())
                .build()
                .start()
                .awaitTermination();
    }
}
