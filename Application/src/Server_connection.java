
import java.io.*;
import java.net.*;


/**
 * @Author Tim Jonasson
 * @Version 1.0
 * Handles the connection to the backend server
 *
 */

public class Server_connection {
    private Socket socket = null;
    private PrintWriter outputStream = null;
    private BufferedReader inputStream = null;

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
            socket = new Socket("127.0.0.1", 8040);
            outputStream =  new PrintWriter(socket.getOutputStream());
            inputStream = new BufferedReader(new InputStreamReader(socket.getInputStream()));
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
    public String ReceiveMessage() {
        StringBuilder stringBuilder = new StringBuilder();
        int value;
        try {
            //The character ~ is used as the stop character so the client knows when to stop reading from the input stream
            while( (value = inputStream.read()) != '~') {
                stringBuilder.append((char)value);
            }
        } catch (IOException e1) {
            e1.printStackTrace();
        }
        return stringBuilder.toString();
    }

    /**
     * Checks if the connection is open
     * @return true if the connection is open
     */
    private boolean CheckConnection(){
            return socket != null && outputStream != null && inputStream != null;
    }
}