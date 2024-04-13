
package Storage;

import file.Rx3FileGrpc;
import file.FileMessage;
import io.r2dbc.postgresql.PostgresqlConnectionConfiguration;
import io.r2dbc.postgresql.PostgresqlConnectionFactory;
import io.r2dbc.spi.ConnectionFactories;
import io.r2dbc.spi.Result;
import io.reactivex.rxjava3.annotations.NonNull;
import io.reactivex.rxjava3.core.Flowable;

public class Store extends Rx3FileGrpc.FileImplBase {

    public void storeMessages(Flowable<FileMessage> received) {
        var cf = new PostgresqlConnectionFactory(
                PostgresqlConnectionConfiguration.builder()
                        .host("localhost")
                        .port(5432)
                        .username("postgres")
                        .password("postgres")
                        .database("album")
                        .build());

        received.flatMap(message -> Flowable.fromPublisher(cf.create())
                        .flatMap(connection -> connection
                                .createStatement("INSERT INTO data (hash, data) VALUES ('1223', $1)")
                                .bind("$1", message.getData().getBytes())
                                .execute()
                        ))
                .subscribe(onNext -> {

                },onError -> {
                    System.out.println(onError.getMessage());
                });
    }
}