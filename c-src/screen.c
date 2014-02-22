#include <jni.h>
#include <curses.h>

JNIEXPORT void JNICALL
Java_viv_terminal_Screen_nativeStart(JNIEnv *env, jclass k)
{
	initscr();
	cbreak();
	noecho();
	keypad(stdscr, 1);
}

JNIEXPORT void JNICALL
Java_viv_terminal_Screen_stop(JNIEnv *env, jclass k)
{
	endwin();
}

JNIEXPORT jchar JNICALL
Java_viv_terminal_Screen_getch(JNIEnv *env, jclass k)
{
	return getch();
}

JNIEXPORT void JNICALL
Java_viv_terminal_Screen_refresh(JNIEnv *env, jclass k, jint width, jcharArray charsArray)
{
	jsize size = (*env)->GetArrayLength(env, charsArray);
	jchar *chars = (*env)->GetCharArrayElements(env, charsArray, NULL);

	for (jint i = 0, offset = 0; offset < size; ++i, offset += width)
		for (jint j = 0; j < width; ++j)
			mvaddch(i, j, chars[offset+j]);

	refresh();
	(*env)->ReleaseCharArrayElements(env, charsArray, chars, 0);
}
