package visuals;

import javafx.beans.value.ChangeListener;
import javafx.stage.Stage;

/**
 * The Resizer runs on a seperate thread and there can only exist one at a time.
 * It's in charge of when the canvases are meant to be resized after a certain time delay.
 * @version 0.3
 * @author Pontus Laestadius
 */
public class Resizer implements Runnable {
    private static int timer = 0;
    private static Resizer th;
    private static long avgRenderTime = 0;
    private static int avgCount = 0;
    static int w = (int)DiagramView.tabPane.getWidth();
    static int h = (int)DiagramView.tabPane.getWidth();

    public void run() {
        if (th == null) {
            th = this;
            time();
        }
    }

    /**
     * Waits for the average render time and then resizes the canvas to the TabPane's size.
     */
    private void time(){ // lol how can somenoe void time? Timetraveled I guess.
        while (timer < avgRenderTime/++avgCount) {
            try {
                Thread.sleep(15);
            } catch (Exception e) {
                System.err.println(e.toString());
            }
            timer += 15;
        }
        long t1 = System.currentTimeMillis();
        int wi = (int)DiagramView.tabPane.getWidth();
        int he = (int)DiagramView.tabPane.getHeight();
        for (DiagramView d: DiagramView.list){
            d.resize(wi, he);
        }

        // I'm sad that this solution works.
        /*
        Due to an error in that the tabpane data is delayed, adding a delay here to see if the data has been changed
        from what was resized, it will attempt to resize it again.
         */
        try {
            Thread.sleep(30);
        } catch (Exception e) {
            System.err.println(e.toString());
        }
        // TEMP soultion.
        if (DiagramView.tabPane.getWidth() != w && DiagramView.tabPane.getHeight() != h){
            w = (int)DiagramView.tabPane.getWidth();
            h = (int)DiagramView.tabPane.getHeight();
            time();
        }

        long time = System.currentTimeMillis()-t1;
        avgRenderTime += time;
        System.out.println("Resizing {dim: " + wi + "x" + he + ", time: " + time + "ms, avg: " + (avgRenderTime/avgCount) + "ms}");
        th = null;
        w = 0;
        h = 0;
    }

    /**
     * Adds a change listener for the width and height properties of the stage provided, to run the listener method.
     * @param primaryStage the stage to add the listener too.
     */
    public static void init(Stage primaryStage) {
        ChangeListener<Number> stageSizeListener = (obserable, oldVal, newVal) -> {
            if (obserable.toString().contains("width")){
                w = newVal.intValue();
            } else {
                h = newVal.intValue();
            }

            (new Thread(new Resizer())).start();


        };
        primaryStage.widthProperty().addListener(stageSizeListener);
        primaryStage.heightProperty().addListener(stageSizeListener);
    }
}