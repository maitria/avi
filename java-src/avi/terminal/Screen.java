package avi.terminal;

public class Screen {

    public static void start() {
        System.loadLibrary("avi_jni");
        nativeStart();
    }

    private static native void nativeStart();
    public static native void stop();
    public static native String getKey();
    public static native void refresh(int cursorI, int cursorJ, int width, char[] chars, byte[] attrs);
    public static native int[] size();
    public static native void beep();
}
