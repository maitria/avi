package viv.terminal;

public class Screen {

    public static void start() {
        System.loadLibrary("viv_terminal_Screen");
        nativeStart();
    }

    private static native void nativeStart();
    public static native void stop();
}
