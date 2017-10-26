package visuals.handlers;

/**
 * Holds a static unique counter that increments on every call.
 */
public class UniqueCounter {
    private static int c;

    /**
     * @return a unique integer that can be used as an identifier.
     */
    public static int get() {
        return c++;
    }

    /**
     * @return a String version of an identifier.
     */
    public static String getString() {
        return get() + "";
    }
}
