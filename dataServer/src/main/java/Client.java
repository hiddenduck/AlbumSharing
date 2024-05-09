import com.google.protobuf.ByteString;
import file.DownloadMessage;
import file.FileMessage;
import file.Rx3FileGrpc;
import io.grpc.ManagedChannelBuilder;
import io.reactivex.rxjava3.core.BackpressureStrategy;
import io.reactivex.rxjava3.core.Flowable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.schedulers.Schedulers;

import java.io.BufferedReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;

import static io.reactivex.rxjava3.schedulers.Schedulers.io;

public class Client { // For testing
    public static void main(String[] args) throws Exception {
        var c = ManagedChannelBuilder.forAddress("localhost", 12345)
                .usePlaintext()
                .build();

        var s = Rx3FileGrpc.newRxStub(c);
        //String content = "3r4528u5rrfuhjweuihrfhuiwhuireuhirweuhirhuiwe";

        String filePath = "output.txt";

        //FileWriter writer = new FileWriter(filePath);

        //writer.write(content);

        //writer.close();

        //System.out.println("Content has been written to " + filePath);

        DownloadMessage request = DownloadMessage.newBuilder()
                .setHashKey(ByteString.copyFromUtf8(filePath))
                .build();

        s.download(request).observeOn(Schedulers.io()).blockingSubscribe(
                        fileMessage -> {
                            System.out.println("Received file message");
                            System.out.println(fileMessage.getData().toStringUtf8());
                        },
                        throwable -> {
                            System.err.println(throwable.getMessage());
                        },
                        () -> {
                            System.out.println("File download completed.");
                        }
                );
    }
}
