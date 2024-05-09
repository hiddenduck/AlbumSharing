import com.google.protobuf.ByteString;
import io.grpc.ManagedChannelBuilder;
import io.reactivex.rxjava3.core.Flowable;
import io.reactivex.rxjava3.schedulers.Schedulers;

import java.io.BufferedReader;
import java.io.FileReader;

public class Client_upload {
    //FileWriter writer = new FileWriter(filePath);

    //writer.write(content);

    //writer.close();

    //System.out.println("Content has been written to " + filePath);
    public static void main(String[] args) throws Exception {
        var c = ManagedChannelBuilder.forAddress("localhost", 12345)
                .usePlaintext()
                .build();

        var s = file.Rx3FileGrpc.newRxStub(c);

        String filePath = "output.txt";

        String filePath2 = "output2.txt";

        var f = Flowable.using(
                () -> new BufferedReader(new FileReader(filePath)),
                reader -> Flowable.fromIterable(() -> reader.lines().iterator()),
                BufferedReader::close
        ).observeOn(Schedulers.io()).map(n -> file.FileMessage.newBuilder().setHashKey(ByteString.copyFromUtf8(filePath2)).setData(ByteString.copyFromUtf8(n)).build());

        s.upload(f).blockingSubscribe();
    }
}
