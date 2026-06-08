import java.net.ServerSocket;
import java.net.Socket;

public class Main {
    static int PORT = 9922;
    int UserCount = -1;
    public static void main(String[] args) {
        try (ServerSocket serverSocket = new ServerSocket(PORT)) {
            Socket ClientSocket = new Socket();
            HandleClient(ClientSocket);
        } catch (Exception e){
            System.out.println(e);
        }
    }

    public static void HandleClient(Socket ClientSocket) {
        
    }
}