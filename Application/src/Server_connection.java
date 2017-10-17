import java.io.*;
import java.net.*;

public class Server_connection {
    private static Socket socket = null;
    private static PrintWriter outputStream = null;
    private static DataInputStream inputStream = null;

    public void Init(){
       OpenConnection();
    }

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

    public void CloseConnection(){
        try {
            outputStream.close();
            inputStream.close();
            socket.close();
        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    public void SendMessage(String Message) {

        if (CheckConnection()) {
            outputStream.print(Message);
            outputStream.flush();

        }
    }

    public void ReceiveMessage(){
        try {
            inputStream = new DataInputStream(socket.getInputStream());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    private boolean CheckConnection(){
            return socket != null && outputStream != null && inputStream != null;
    }


    public static Socket getSocket() {
        return socket;
    }

    public static void setSocket(Socket socket) {
        Server_connection.socket = socket;
    }

    public static PrintWriter getOutputStream() {
        return outputStream;
    }

    public static void setOutputStream(PrintWriter outputStream) {
        Server_connection.outputStream = outputStream;
    }

    public static DataInputStream getInputStream() {
        return inputStream;
    }

    public static void setInputStream(DataInputStream inputStream) {
        Server_connection.inputStream = inputStream;
    }

}