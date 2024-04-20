import com.google.protobuf.ByteString;
import file.DownloadMessage;
import file.FileMessage;
import file.Rx3FileGrpc;
import file.UploadMessage;
import io.reactivex.rxjava3.core.*;
import io.reactivex.rxjava3.schedulers.Schedulers;

import java.io.*;

public class FileService extends Rx3FileGrpc.FileImplBase {


    private Flowable<String> openFileToStream(DownloadMessage request) {
        // Flowable using does 3 things:
        // 1: request a resource (BufferedReader)
        // 2: read lines from that resource
        // 3: when done, free resource by closing
        return Flowable.using(
                () -> new BufferedReader(new FileReader(String.valueOf(request.getHashKey()))),
                reader -> Flowable.fromIterable(() -> reader.lines().iterator()),
                BufferedReader::close
        );
    }


    @Override
    public Flowable<FileMessage> download(DownloadMessage request) {
        return openFileToStream(request)
                .observeOn(Schedulers.io())
                .map(n -> FileMessage.newBuilder().setHashKey(request.getHashKey()).setData(ByteString.copyFromUtf8(n)).build());
    }

    public Single<UploadMessage> upload(Flowable<FileMessage> request) {
        var r = request
                .observeOn(Schedulers.io())
                .flatMap(message -> {
                    try (BufferedWriter writer = new BufferedWriter(new FileWriter(String.valueOf(message.getHashKey())))) {
                        writer.write(message.getData().toStringUtf8() + "\n");
                        return Flowable.just(UploadMessage.newBuilder().build());
                    } catch (IOException e) {
                        return Flowable.error(e);
                    }
                });

        return r.firstOrError();
    }

}
