package visuals.handlers;

import visuals.DiagramView;
import visuals.Draw;

/**
 * Handles refreshing and animating draw objects.7
 * @version 0.6
 * @author Pontus Laestadius
 */
public class Animation extends Thread {
    private Thread th;

    public Animation() {}

    @Override
    public void run() {
        if (th == null) {
            th = this;
            loop();
        }
    }

    private void loop() {
        while (true) {
            try {
                Thread.sleep(200);
            } catch (InterruptedException e) {
                System.err.println(e.toString());
            }

            DiagramView dv = DiagramView.getDiagramViewInView();
            if (dv == null) continue;
            Draw draw = dv.getDraw();
            draw.update();
            draw.redraw();
        }
    }
}
