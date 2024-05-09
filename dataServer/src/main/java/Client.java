import com.google.protobuf.ByteString;
import file.DownloadMessage;
import file.FileMessage;
import file.Rx3FileGrpc;
import io.grpc.ManagedChannelBuilder;
import io.reactivex.rxjava3.core.BackpressureStrategy;
import io.reactivex.rxjava3.core.Flowable;

import java.io.BufferedReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;

import static io.reactivex.rxjava3.schedulers.Schedulers.io;

public class Client { // For testing
    public static void main(String[] args) throws InterruptedException {
        var c = ManagedChannelBuilder.forAddress("localhost", 12345)
                .usePlaintext()
                .build();

        var s = Rx3FileGrpc.newRxStub(c);
        String content = "3r4528u5rrfuhjweuihrfhuiwhuireuhirweuhirhuiwe";

        String filePath = "output.txt";

        try {
            FileWriter writer = new FileWriter(filePath);

            writer.write(content);

            writer.close();

            System.out.println("Content has been written to " + filePath);

        } catch (IOException e) {
            System.out.println("An error occurred while writing to the file: " + e.getMessage());
            e.printStackTrace();
        }

        s.upload()

        //s.download(DownloadMessage.newBuilder().setHashKey(ByteString.copyFromUtf8("ola")).build()).blockingSubscribe();

        Thread.sleep(100);
    }
}
