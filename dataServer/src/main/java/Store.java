import com.google.protobuf.ByteString;
import file.FileMessage;
import io.r2dbc.spi.ConnectionFactories;
import io.r2dbc.spi.ConnectionFactory;
import io.r2dbc.spi.ConnectionFactoryOptions;
import io.reactivex.rxjava3.core.Flowable;
import org.mariadb.r2dbc.MariadbConnectionConfiguration;
import org.mariadb.r2dbc.MariadbConnectionFactory;

public class Store {

    private ConnectionFactory cf;

    private ConnectionFactory createConnectionFactory() {

        MariadbConnectionConfiguration conf = MariadbConnectionConfiguration.builder()
                .host("localhost")
                .port(3306)
                .username("carlos")
                .password("123456")
                .database("album")
                .build();

        return new MariadbConnectionFactory(conf);
    }

    public Store(){
        this.cf = createConnectionFactory();
    }

    public void query(){
        Flowable.just("ola").flatMap(s -> Flowable.fromPublisher(cf.create())
                .flatMap(connection -> connection
                        .createStatement("select * from file").execute())
        ).blockingSubscribe();
    }
    /*
    public Flowable<FileMessage> transferFile(ByteString hash_key){
        return Flowable.fromPublisher()
    }

     */
}
