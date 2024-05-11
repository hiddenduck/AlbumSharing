import com.google.protobuf.ByteString;
import io.grpc.ManagedChannelBuilder;
import io.reactivex.rxjava3.core.BackpressureStrategy;
import io.reactivex.rxjava3.core.Flowable;
import io.reactivex.rxjava3.schedulers.Schedulers;

import java.io.*;

public class Client_upload {


    //System.out.println("Content has been written to " + filePath);
    public static void main(String[] args) throws Exception {
        var c = ManagedChannelBuilder.forAddress("localhost", 1234)
                .usePlaintext()
                .build();

        var s = file.Rx3FileGrpc.newRxStub(c);

        String filePath = "output.txt";

        //String content = "1234567";

        //FileWriter writer = new FileWriter(filePath);

        //writer.write(content);

        //writer.close();

        String filePath2 = "output2.txt";

        int chunkSize = 100;



        var f = Flowable.using(
                () -> new FileInputStream(filePath),
                inputStream -> Flowable.create(
                        emitter -> {
                            int count = 0;
                            byte[] buffer = new byte[chunkSize];
                            int read;
                            while ((read = inputStream.read(buffer)) != -1) {
                                byte[] readBuff = new byte[read];
                                System.arraycopy(buffer, 0, readBuff, 0, read);
                                emitter.onNext(readBuff);
                            }
                            emitter.onComplete();
                        }
                , BackpressureStrategy.DROP),
                InputStream::close
        ).observeOn(Schedulers.io()).map(n -> file.FileMessage.newBuilder().setHashKey(ByteString.copyFromUtf8(filePath2)).setData(ByteString.copyFrom((byte[])n)).build());

        s.upload(f).blockingSubscribe();
    }
}
