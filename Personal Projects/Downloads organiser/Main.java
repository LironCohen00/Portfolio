import java.nio.file.*;
import java.io.IOException;

public class Main {
    public static void main(String[] args) {
        Path dir = Paths.get(System.getProperty("user.home"), "Downloads");
        organise(dir);
    }


    public static void organise(Path dir) {
        Path configPath = Paths.get("categories.yaml");
        CategoryLoader loader = new CategoryLoader(configPath);

        try {
            Files.list(dir)
            .filter(Files::isRegularFile)
            .forEach(file -> {
             try {
                 String mime = Files.probeContentType(file);
                 String name = file.getFileName().toString().toLowerCase();
                 int dot = name.lastIndexOf('.');
                 String ext = (dot > 0) ? name.substring(dot + 1) : null;

                 String category = loader.getCategory(mime, ext);

                 if (category != null) {
                     Path targetDir = dir.resolve(category);
                     Files.createDirectories(targetDir);
                     Files.move(file, targetDir.resolve(file.getFileName()),
                                StandardCopyOption.REPLACE_EXISTING);
                 } else {
                     System.out.println("Skipping unknown file: " + file.getFileName());
                 }
             } catch (Exception e) {
                 e.printStackTrace();
             }
    });
        } catch (IOException e) {
            e.printStackTrace();
        }

}

}
