import com.google.protobuf.ByteString;
import io.grpc.ManagedChannelBuilder;
import io.reactivex.rxjava3.core.BackpressureStrategy;
import io.reactivex.rxjava3.core.Flowable;
import io.reactivex.rxjava3.schedulers.Schedulers;

import java.io.*;

public class Client_upload {


    public static void main(String[] args) throws Exception {

        String filePath = args[0];
        String Hash     = args[1];
        String IP       = args[2];
        String PORT     = args[3];

        var c = ManagedChannelBuilder.forAddress(IP, Integer.parseInt(PORT))
                .usePlaintext()
                .build();

        var s = file.Rx3FileGrpc.newRxStub(c);

        //String filePath = "output.txt";


        //String filePath2 = "output2.txt";

        int chunkSize = 4096;

        var f = Flowable.generate(
                () -> {
                    FileInputStream reader = new FileInputStream(filePath);
                    return reader;
                },
                (reader, emitter) -> {
                    try {
                        byte[] buffer = new byte[chunkSize];
                        int read = reader.read(buffer);
                        if (read == -1) {
                            reader.close();
                            emitter.onComplete();
                        } else {
                            byte[] readBuff = new byte[read];
                            System.arraycopy(buffer, 0, readBuff, 0, read);
                            emitter.onNext(readBuff);
                        }
                    } catch (IOException e) {
                        reader.close();
                        emitter.onError(e);
                    }
                    return reader;
                },
                reader -> {
                    try {
                        reader.close();
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }
        ).observeOn(Schedulers.io()).map(n -> file.FileMessage.newBuilder().setHashKey(ByteString.copyFromUtf8(Hash)).setData(ByteString.copyFrom((byte[])n)).build());

        s.upload(f).blockingSubscribe((m) -> System.out.println("received"),
                (e -> {
                    System.exit(1);
                }));
        System.exit(0);
    }
}
