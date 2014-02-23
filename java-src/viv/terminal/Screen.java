package viv.terminal;

public class Screen {

    public static void start() {
        System.loadLibrary("viv_terminal_Screen");
        nativeStart();
    }

    private static native void nativeStart();
    public static native void stop();
    public static native char getch();
    public static native void refresh(int cursorI, int cursorJ, int width, char[] chars, byte[] attrs);
    public static native int[] size();
}
