import Storage.Store;
import com.google.common.primitives.Bytes;
import com.google.protobuf.ByteString;
import com.google.protobuf.InvalidProtocolBufferException;
import file.FileMessage;
import io.reactivex.rxjava3.core.Flowable;

import java.nio.charset.Charset;

public class Main {
    public static void main(String[] args) throws Exception{
        Store store = new Store();

        Flowable<FileMessage> f = Flowable.just(FileMessage.newBuilder().setData(
                FileMessage.Data.newBuilder().setBytes(ByteString.copyFromUtf8("ola")).build()
        ).build());
        store.storeMessages(f);

        Thread.sleep(1000000);
    }
}
