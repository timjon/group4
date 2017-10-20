package visuals.handlers;

import javafx.beans.value.ChangeListener;
import javafx.stage.Stage;
import visuals.DiagramView;

/**
 * The Resizer runs on a separate thread and there can only exist one at a time.
 * It's in charge of when the canvases are meant call resize after a certain time delay.
 * @version 0.5
 * @author Pontus Laestadius
 */
public class Resizer implements Runnable {
    private static Resizer th;
    private static int timer = 0;
    private int inc = 5;
    private int threashold = 45;

    /**
     * Runs when the thread starts.
     */
    public void run() {
        if (th == null) // Only allow one instance of the program to run at a time
            time();
    }

    /**
     * I bet tim wont read this, But just complains about no comments.
     */
    private void time(){
        th = this;
        while ((timer+=inc) < threashold) { // A 400ms delay in which the timer can be reset
            try {
                Thread.sleep(inc); // Sleep for the same time as timer increases
            } catch (InterruptedException e) {
                System.err.println(e.toString());
                System.out.println(e.toString());
            }
        }
        for (DiagramView d: DiagramView.list) // For all opened diagrams:
            d.resize(); // Resize them
        th = null; // Allows a new instance to start once this one is finished
    }

    /**
     * Adds a change listener for the width and height properties of the stage provided, to run the listener method.
     * @param primaryStage the stage to add the listener too.
     */
    public static void init(Stage primaryStage) {
        ChangeListener<Number> stageSizeListener = (obserable, oldVal, newVal) -> {
            timer = 0;
            if (th == null)
                (new Thread(new Resizer())).start();
        };
        primaryStage.widthProperty().addListener(stageSizeListener);
        primaryStage.heightProperty().addListener(stageSizeListener);
    }
}