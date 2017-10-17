import java.io.*;
import java.net.*;

/**
 * @Author Tim Jonasson
 * @Version 1.0
 * Handles the connection to the backend server
 *
 */

public class Server_connection {
    private static Socket socket = null;
    private static PrintWriter outputStream = null;
    private static DataInputStream inputStream = null;

    public static void main(String[] args){
        Server_connection server = new Server_connection();
        try {
            socket = new Socket("10.0.151.42", 8040);
            outputStream =  new PrintWriter(socket.getOutputStream());
            inputStream = new DataInputStream(socket.getInputStream());
        } catch (UnknownHostException e) {
            System.err.println(e);
        } catch (IOException e) {
            System.err.println(e);
        }
        if (socket != null && outputStream != null && inputStream != null) {
            outputStream.print("{[a1, a2], [{a1, a2, hello}]}");
            outputStream.flush();

        }
        //server.SendMessage("{[a1, a2], [{a1, a2, hello}, {a2, a1, helloback}]}");

    }
    /**
     * Initializes the connection to the backend server
     */
    public void Init(){
       OpenConnection();
    }

    /**
     * Opens the connection to the port and ip
     */
    public void OpenConnection(){
        try {
            socket = new Socket("10.0.151.42", 8040);
            outputStream =  new PrintWriter(socket.getOutputStream());
            inputStream = new DataInputStream(socket.getInputStream());
        } catch (UnknownHostException e) {
            System.err.println(e);
        } catch (IOException e) {
            System.err.println(e);
        }
    }

    /**
     * Closes the connection to the server
     */
    public void CloseConnection(){
        try {
            outputStream.close();
            inputStream.close();
            socket.close();
        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    /**
     * sends a message to the server
     * @param Message The message sent to the server
     */
    public void SendMessage(String Message) {

        if (CheckConnection()) {
            outputStream.print(Message);
            outputStream.flush();

        }
    }

    /**
     * Reads a message from the server
     */
    public void ReceiveMessage() {
        if (CheckConnection()) {
            try {
                inputStream = new DataInputStream(socket.getInputStream());
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * Checks if the connection is open
     * @return True if the connection is open
     */
    private boolean CheckConnection(){
            return socket != null && outputStream != null && inputStream != null;
    }
}