#include <jni.h>
#include <curses.h>

JNIEXPORT void JNICALL
Java_viv_terminal_Screen_nativeStart(JNIEnv *env, jclass k)
{
	initscr();
	cbreak();
}

JNIEXPORT void JNICALL
Java_viv_terminal_Screen_stop(JNIEnv *env, jclass k)
{
	endwin();
}
