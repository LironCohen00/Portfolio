import java.io.*;
import java.net.*;
import java.util.concurrent.CopyOnWriteArrayList;

public class Client implements Runnable {
        private Socket clientSocket;
        private PrintWriter out;
        private BufferedReader in;

        public Client(Socket socket) throws IOException {
            this.clientSocket = socket;
            this.out = new PrintWriter(socket.getOutputStream(), true);
            this.in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        }

        @Override
        public void run() {
            try {
                // Send welcome message to the new client
                out.println(String.format(WELCOME_MESSAGE, clients.size() - 1));

                // Notify other clients about the new client
                for (ClientHandler client : clients) {
                    if (client != this) {
                        client.out.println(String.format(JOIN_MESSAGE_FORMAT, clientSocket.getInetAddress()));
                    }
                }

                // Listen for client messages
                String message;
                while ((message = in.readLine()) != null) {
                    // Broadcast the message to other clients
                    for (ClientHandler client : clients) {
                        if (client != this) {
                            client.out.println(String.format(MESSAGE_FORMAT, clientSocket.getInetAddress(), clientSocket.getPort(), message));
                        }
                    }
                }
            } catch (IOException e) {
                // Client disconnected
                clients.remove(this);

                // Notify other clients about the disconnection
                for (ClientHandler client : clients) {
                    client.out.println(String.format(JOIN_MESSAGE_FORMAT, clientSocket.getInetAddress()));
                }
            }
        }
    }
