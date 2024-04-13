package Storage;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

public class Appconfig {
    private static final Properties properties = new Properties();

    static {
        try (InputStream input = new FileInputStream("application.properties")) {
            properties.load(input);
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }

    public static String getDBUrl() {
        return properties.getProperty("database.url");
    }

    public static String getUserDBName() {
        return properties.getProperty("database.username");
    }

    public static String getUserDBPassword() {
        return properties.getProperty("database.password");
    }
}
