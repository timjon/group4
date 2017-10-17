package visuals;

public class AnimationHandler extends Thread {
    public static boolean run = false;
    Draw item;

    AnimationHandler(Draw item) {
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
                Thread.sleep(1000/30);
            } catch (InterruptedException e) {
                System.err.println(e.toString());
            }
            item.redraw();
        }
    }
}
