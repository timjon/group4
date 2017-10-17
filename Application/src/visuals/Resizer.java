package visuals;

import javafx.beans.value.ChangeListener;
import javafx.stage.Stage;

/**
 * The Resizer runs on a seperate thread and there can only exist one at a time.
 * It's in charge of when the canvases are meant to be resized after a certain time delay.
 * @version 0.4
 * @author Pontus Laestadius
 */
public class Resizer implements Runnable {
    private static Resizer th;
    static int timer = 0;

    public void run() {
        if (th == null) {
            th = this;
            time();
        }
    }

    private void time(){ // lol how can somenoe void time? Timetraveled I guess.
        while ((timer+=250) < 749) {
            try {
                Thread.sleep(250);
            } catch (Exception e) {
                System.err.println(e.toString());
            }
        }
        for (DiagramView d: DiagramView.list)
            d.resize();
        th = null;
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