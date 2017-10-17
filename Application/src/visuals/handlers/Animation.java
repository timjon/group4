package visuals.handlers;

import visuals.Draw;

public class Animation extends Thread {
    public static boolean run = false;
    Draw item;

    public Animation(Draw item) {
        this.item = item;
    }

    @Override
    public void run() {
        if (run) return;
        run = true;
        loop();
    }

    private void loop() {
        while (run) {
            try {
                Thread.sleep(1000/3);
            } catch (InterruptedException e) {
                System.err.println(e.toString());
            }
            item.update();
            item.redraw();
        }
    }
}
