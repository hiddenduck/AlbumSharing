import com.google.protobuf.ByteString;
import file.DownloadMessage;
import file.FileMessage;
import file.Rx3FileGrpc;
import file.UploadMessage;
import io.reactivex.rxjava3.core.*;
import io.reactivex.rxjava3.schedulers.Schedulers;

import java.io.*;

public class FileService extends Rx3FileGrpc.FileImplBase {

	/**
     * Opens the file and create the stream.
     * @param request Request message for download.
     * @return Stream.
     */
    private Flowable<String> openFileToStream(DownloadMessage request) {
        String filePath = request.getHashKey().toStringUtf8();

        if (!new java.io.File(filePath).exists()) {
            System.out.println("Error in opening file");
            return Flowable.error(io.grpc.Status.NOT_FOUND
                    .withDescription("File not found")
                    .asRuntimeException());
        }

        // Flowable using does 3 things:
        // 1: request a resource (BufferedReader)
        // 2: read lines from that resource
        // 3: when done, free resource by closing
        return Flowable.using(
                () -> new BufferedReader(new FileReader(filePath)),
                reader -> Flowable.fromIterable(() -> reader.lines().iterator()),
                BufferedReader::close
        );
    }

	/**
     * Implements the service of file download.
     * @param request Request message for download.
     * @return Message Stream.
     */
    @Override
    public Flowable<FileMessage> download(DownloadMessage request) {
        return openFileToStream(request)
                .observeOn(Schedulers.io())
                .map(n -> FileMessage.newBuilder().setHashKey(request.getHashKey()).setData(ByteString.copyFromUtf8(n)).build());
    }

	/**
     * Implements the service of file upload.
     * @param request Message Stream.
     * @return Single with confirmation message of the upload.
     */
    public Single<UploadMessage> upload(Flowable<FileMessage> request) {
        var uploadResult = request
                .observeOn(Schedulers.io())
                .flatMap(message -> {
                    try (BufferedWriter writer = new BufferedWriter(new FileWriter(String.valueOf(message.getHashKey())))) {
                        writer.write(message.getData().toStringUtf8() + "\n");
						writer.flush();
                        return Flowable.just(UploadMessage.newBuilder().build());
                    } catch (IOException e) {
                        System.out.println("Error in upload");
						return Flowable.error(new RuntimeException("Failed to upload file."));
                    }
                });

        return uploadResult.firstOrError();
    }

}
