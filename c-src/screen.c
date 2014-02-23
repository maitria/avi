#include <jni.h>
#include <curses.h>

static const int SCREEN_COLORS[] = {
	COLOR_BLACK,
	COLOR_RED,
	COLOR_GREEN,
	COLOR_YELLOW,
	COLOR_BLUE,
	COLOR_MAGENTA,
	COLOR_CYAN,
	COLOR_WHITE
};

const int SCREEN_COLOR_COUNT = sizeof(SCREEN_COLORS)/sizeof(SCREEN_COLORS[0]);

JNIEXPORT void JNICALL
Java_viv_terminal_Screen_nativeStart(JNIEnv *env, jclass k)
{
	initscr();
	start_color();

	for (int i = 0; i < SCREEN_COLOR_COUNT; ++i)
		for (int j = 0; j < SCREEN_COLOR_COUNT; ++j)
			init_pair(i*SCREEN_COLOR_COUNT+j, SCREEN_COLORS[i], SCREEN_COLORS[j]);
	
	cbreak();
	noecho();
	keypad(stdscr, 1);
}

JNIEXPORT void JNICALL
Java_viv_terminal_Screen_stop(JNIEnv *env, jclass k)
{
	endwin();
}

static jboolean is_enter_key(jchar character) 
{
	return character == KEY_ENTER || character == '\n';
}

JNIEXPORT jchar JNICALL
Java_viv_terminal_Screen_getch(JNIEnv *env, jclass k)
{
	jchar character = getch();
	if (is_enter_key(character)) 
		character = '\r';

	return character;
}

JNIEXPORT void JNICALL
Java_viv_terminal_Screen_refresh(JNIEnv *env, jclass k, jint cursorI, jint cursorJ, jint width, jcharArray charsArray, jbyteArray attrsArray)
{
	chtype ch;
	jsize size = (*env)->GetArrayLength(env, charsArray);
	jchar *chars = (*env)->GetCharArrayElements(env, charsArray, NULL);
	jbyte *attrs = (*env)->GetByteArrayElements(env, attrsArray, NULL);

	for (jint i = 0, offset = 0; offset < size; ++i, offset += width) {
		for (jint j = 0; j < width; ++j) {
			ch = chars[offset+j] | COLOR_PAIR(attrs[offset+j]);
			mvaddch(i, j, ch);
		}
	}

	move(cursorI, cursorJ);
	refresh();

	(*env)->ReleaseCharArrayElements(env, charsArray, chars, 0);
	(*env)->ReleaseByteArrayElements(env, attrsArray, attrs, 0);
}
