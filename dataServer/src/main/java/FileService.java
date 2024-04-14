import file.DownloadMessage;
import file.FileMessage;
import file.Rx3FileGrpc;
import io.reactivex.rxjava3.core.BackpressureStrategy;
import io.reactivex.rxjava3.core.Flowable;
import io.reactivex.rxjava3.core.FlowableEmitter;
import io.reactivex.rxjava3.core.Single;

public class FileService extends Rx3FileGrpc.FileImplBase {

    private Store store;

    FlowableEmitter<Flowable<FileMessage>> sub;

    public FileService(){
        this.store = new Store();

    }

    /*
    @Override
    public Flowable<FileMessage> download(DownloadMessage request) {
        return this.store.transferFile(request.getHashKey());
    }

     */

    /*
    @Override
    public Flowable<FileMessage> download(DownloadMessage request) {

    }
    
     */


}
