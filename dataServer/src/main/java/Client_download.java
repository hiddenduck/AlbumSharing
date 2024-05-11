import com.google.protobuf.ByteString;
import file.DownloadMessage;
import file.Rx3FileGrpc;
import io.grpc.ManagedChannelBuilder;
import io.grpc.Status;
import io.reactivex.rxjava3.core.Flowable;
import io.reactivex.rxjava3.schedulers.Schedulers;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

public class Client_download { // For testing
    public static void main(String[] args) throws Exception {
        var c = ManagedChannelBuilder.forAddress("localhost", 1234)
                .usePlaintext()
                .build();

        var s = Rx3FileGrpc.newRxStub(c);

        String filePath = "output2.txt";

        DownloadMessage request = DownloadMessage.newBuilder()
                .setHashKey(ByteString.copyFromUtf8(filePath))
                .build();

        s.download(request).blockingSubscribe(
                        fileMessage -> {
                            byte[] data = fileMessage.getData().toByteArray();

                            try (FileOutputStream writer = new FileOutputStream("output2.txt", true)) {
                                writer.write(data);
                                writer.flush();
                            } catch (IOException e) {
                            }
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
