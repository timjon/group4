package visuals.handlers;

public class UniqueCounter {
    private static int c;
    public static int get() {
        return c++;
    }

    public static String getString() {
        return c++ + "";
    }
}
