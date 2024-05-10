import com.google.protobuf.ByteString;
import file.DownloadMessage;
import file.Rx3FileGrpc;
import io.grpc.ManagedChannelBuilder;
import io.reactivex.rxjava3.schedulers.Schedulers;

public class Client_download { // For testing
    public static void main(String[] args) throws Exception {
        var c = ManagedChannelBuilder.forAddress("localhost", 12345)
                .usePlaintext()
                .build();

        var s = Rx3FileGrpc.newRxStub(c);

        String filePath = "output.txt";

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
