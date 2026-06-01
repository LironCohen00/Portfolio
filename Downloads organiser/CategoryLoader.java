import org.yaml.snakeyaml.Yaml;
import java.io.InputStream;
import java.nio.file.*;
import java.util.*;

public class CategoryLoader {
    private Map<String, String> mimeMap;
    private Map<String, String> extMap;

    public CategoryLoader(Path yamlFile) {
        try (InputStream in = Files.newInputStream(yamlFile)) {
            Yaml yaml = new Yaml();
            Map<String, Object> data = yaml.load(in);
            mimeMap = (Map<String, String>) data.get("mime");
            extMap = (Map<String, String>) data.get("extensions");
        } catch (Exception e) {
            System.err.println("Failed to load categories.yaml, using defaults.");
        }
    }

    public String getCategory(String mimeType, String extension) {
        if (mimeType != null && mimeMap.containsKey(mimeType))
            return mimeMap.get(mimeType);
        if (extension != null && extMap.containsKey(extension))
            return extMap.get(extension);
        return null;
    }
}

