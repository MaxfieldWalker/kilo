/* Kilo -- A very simple editor in less than 1-kilo lines of code (as counted
 *         by "cloc"). Does not depend on libcurses, directly emits VT100
 *         escapes on the terminal.
 *
 * -----------------------------------------------------------------------
 *
 * Copyright (C) 2016 Salvatore Sanfilippo <antirez at gmail dot com>
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *  *  Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *  *  Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#define KILO_VERSION "0.0.1"

#define _BSD_SOURCE
#define _GNU_SOURCE

#include <termios.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <unistd.h>
#include <stdarg.h>
#include <fcntl.h>


/**
 * 編集中のファイル1行分を表す構造体
 */
typedef struct erow {
    int idx;            /* Row index in the file, zero-based. */
    int size;           /* Size of the row, excluding the null term. */
    int rsize;          /* Size of the rendered row. */
    char *chars;        /* Row content. */
    char *render;       /* Row content "rendered" for screen (for TABs). */
} erow;

struct editorConfig {
    int cx, cy;  /* Cursor x and y position in characters */
    int row_display_offset;     /* Offset of row displayed. */
    int col_display_offset;     /* Offset of column displayed. */
    int screen_rows; /* Number of rows that we can show */
    int screen_cols; /* Number of cols that we can show */
    int file_numrows;    /* Number of rows */
    int rawmode;    /* Is terminal raw mode enabled? */
    erow *row;      /* Rows */
    int dirty;      /* ファイルが未保存状態か */
    char *filename; /* Currently open filename */
    char statusmsg[80];
    time_t statusmsg_time;
};

static struct editorConfig E;

enum KEY_ACTION {
    KEY_NULL = 0,       /* NULL */
    CTRL_C = 3,         /* Ctrl-c */
    CTRL_D = 4,         /* Ctrl-d */
    CTRL_F = 6,         /* Ctrl-f */
    CTRL_H = 8,         /* Ctrl-h */
    TAB = 9,            /* Tab */
    CTRL_L = 12,        /* Ctrl+l */
    ENTER = 13,         /* Enter */
    CTRL_Q = 17,        /* Ctrl-q */
    CTRL_S = 19,        /* Ctrl-s */
    CTRL_U = 21,        /* Ctrl-u */
    ESC = 27,           /* Escape */
    BACKSPACE = 127,   /* Backspace */
    /* The following are just soft codes, not really reported by the
     * terminal directly. */
            ARROW_LEFT = 1000,
    ARROW_RIGHT,
    ARROW_UP,
    ARROW_DOWN,
    DEL_KEY,
    HOME_KEY,
    END_KEY,
    PAGE_UP,
    PAGE_DOWN
};

void editorSetStatusMessage(const char *fmt, ...);


/* ======================= Low level terminal handling ====================== */

static struct termios orig_termios; /* In order to restore at exit.*/

void disableRawMode(int fd) {
    /* Don't even check the return value as it's too late. */
    if (E.rawmode) {
        tcsetattr(fd, TCSAFLUSH, &orig_termios);
        E.rawmode = 0;
    }
}

/* Called at exit to avoid remaining in raw mode. */
void editorAtExit(void) {
    disableRawMode(STDIN_FILENO);
}

/* Raw mode: 1960 magic shit. */
int enableRawMode(int fd) {
    struct termios raw;

    if (E.rawmode) return 0; /* Already enabled. */
    if (!isatty(STDIN_FILENO)) goto fatal;
    atexit(editorAtExit);
    if (tcgetattr(fd, &orig_termios) == -1) goto fatal;

    raw = orig_termios;  /* modify the original mode */
    /* input modes: no break, no CR to NL, no parity check, no strip char,
     * no start/stop output control. */
    raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    /* output modes - disable post processing */
    raw.c_oflag &= ~(OPOST);
    /* control modes - set 8 bit chars */
    raw.c_cflag |= (CS8);
    /* local modes - choing off, canonical off, no extended functions,
     * no signal chars (^Z,^C) */
    raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
    /* control chars - set return condition: min number of bytes and timer. */
    raw.c_cc[VMIN] = 0; /* Return each byte, or zero for timeout. */
    raw.c_cc[VTIME] = 1; /* 100 ms timeout (unit is tens of second). */

    /* put terminal in raw mode after flushing */
    if (tcsetattr(fd, TCSAFLUSH, &raw) < 0) goto fatal;
    E.rawmode = 1;
    return 0;

    fatal:
    errno = ENOTTY;
    return -1;
}

/* Read a key from the terminal put in raw mode, trying to handle
 * escape sequences. */
int editorReadKey(int fd) {
    int nread;
    char c, seq[3];
    while ((nread = read(fd, &c, 1)) == 0);
    if (nread == -1) exit(1);

    while (1) {
        switch (c) {
            case ESC:    /* escape sequence */
                /* If this is just an ESC, we'll timeout here. */
                if (read(fd, seq, 1) == 0) return ESC;
                if (read(fd, seq + 1, 1) == 0) return ESC;

                /* ESC [ sequences. */
                if (seq[0] == '[') {
                    if (seq[1] >= '0' && seq[1] <= '9') {
                        /* Extended escape, read additional byte. */
                        if (read(fd, seq + 2, 1) == 0) return ESC;
                        if (seq[2] == '~') {
                            switch (seq[1]) {
                                case '3':
                                    return DEL_KEY;
                                case '5':
                                    return PAGE_UP;
                                case '6':
                                    return PAGE_DOWN;
                            }
                        }
                    } else {
                        switch (seq[1]) {
                            case 'A':
                                return ARROW_UP;
                            case 'B':
                                return ARROW_DOWN;
                            case 'C':
                                return ARROW_RIGHT;
                            case 'D':
                                return ARROW_LEFT;
                            case 'H':
                                return HOME_KEY;
                            case 'F':
                                return END_KEY;
                        }
                    }
                }
                    /* ESC O sequences. */
                else if (seq[0] == 'O') {
                    switch (seq[1]) {
                        case 'H':
                            return HOME_KEY;
                        case 'F':
                            return END_KEY;
                    }
                }
                break;
            default:
                return c;
        }
    }
}

/* Use the ESC [6n escape sequence to query the horizontal cursor position
 * and return it. On error -1 is returned, on success the position of the
 * cursor is stored at *rows and *cols and 0 is returned. */
int getCursorPosition(int ifd, int ofd, int *rows, int *cols) {
    char buf[32];
    unsigned int i = 0;

    /* Report cursor location */
    if (write(ofd, "\x1b[6n", 4) != 4) return -1;

    /* Read the response: ESC [ rows ; cols R */
    while (i < sizeof(buf) - 1) {
        if (read(ifd, buf + i, 1) != 1) break;
        if (buf[i] == 'R') break;
        i++;
    }
    buf[i] = '\0';

    /* Parse it. */
    if (buf[0] != ESC || buf[1] != '[') return -1;
    if (sscanf(buf + 2, "%d;%d", rows, cols) != 2) return -1;
    return 0;
}

/* Try to get the number of columns in the current terminal. If the ioctl()
 * call fails the function will try to query the terminal itself.
 * Returns 0 on success, -1 on error. */
int getWindowSize(int ifd, int ofd, int *rows, int *cols) {
    struct winsize ws;

    if (ioctl(1, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
        /* ioctl() failed. Try to query the terminal itself. */
        int orig_row, orig_col, retval;

        /* Get the initial position so we can restore it later. */
        retval = getCursorPosition(ifd, ofd, &orig_row, &orig_col);
        if (retval == -1) goto failed;

        /* Go to right/bottom margin and get position. */
        if (write(ofd, "\x1b[999C\x1b[999B", 12) != 12) goto failed;
        retval = getCursorPosition(ifd, ofd, rows, cols);
        if (retval == -1) goto failed;

        /* Restore position. */
        char seq[32];
        snprintf(seq, 32, "\x1b[%d;%dH", orig_row, orig_col);
        if (write(ofd, seq, strlen(seq)) == -1) {
            /* Can't recover... */
        }
        return 0;
    } else {
        *cols = ws.ws_col;
        *rows = ws.ws_row;
        return 0;
    }

    failed:
    return -1;
}

/* ======================= Editor rows implementation ======================= */

/* Update the rendered version and the syntax highlight of a row. */
void editorUpdateRow(erow *row) {
    int tabs = 0, nonprint = 0, j, idx;

    /* Create a version of the row we can directly print on the screen,
      * respecting tabs, substituting non printable characters with '?'. */
    free(row->render);
    for (j = 0; j < row->size; j++)
        if (row->chars[j] == TAB) tabs++;

    row->render = malloc(row->size + tabs * 8 + nonprint * 9 + 1);
    idx = 0;
    for (j = 0; j < row->size; j++) {
        if (row->chars[j] == TAB) {
            row->render[idx++] = ' ';
            while ((idx + 1) % 8 != 0) row->render[idx++] = ' ';
        } else {
            row->render[idx++] = row->chars[j];
        }
    }
    row->rsize = idx;
    row->render[idx] = '\0';
}

/* Insert a row at the specified position, shifting the other rows on the bottom
 * if required. */
void editorInsertRow(int at, char *s, size_t len) {
    if (at > E.file_numrows) return;
    E.row = realloc(E.row, sizeof(erow) * (E.file_numrows + 1));
    if (at != E.file_numrows) {
        memmove(E.row + at + 1, E.row + at, sizeof(E.row[0]) * (E.file_numrows - at));
        for (int j = at + 1; j <= E.file_numrows; j++) E.row[j].idx++;
    }
    E.row[at].size = len;
    E.row[at].chars = malloc(len + 1);
    memcpy(E.row[at].chars, s, len + 1);
    E.row[at].render = NULL;
    E.row[at].rsize = 0;
    E.row[at].idx = at;
    editorUpdateRow(E.row + at);
    E.file_numrows++;
    E.dirty++;
}

/* Free row's heap allocated stuff. */
void editorFreeRow(erow *row) {
    free(row->render);
    free(row->chars);
}

/* Remove the row at the specified position, shifting the remainign on the
 * top. */
void editorDelRow(int at) {
    erow *row;

    if (at >= E.file_numrows) return;
    row = E.row + at;
    editorFreeRow(row);
    memmove(E.row + at, E.row + at + 1, sizeof(E.row[0]) * (E.file_numrows - at - 1));
    for (int j = at; j < E.file_numrows - 1; j++) E.row[j].idx++;
    E.file_numrows--;
    E.dirty++;
}

/* Turn the editor rows into a single heap-allocated string.
 * Returns the pointer to the heap-allocated string and populate the
 * integer pointed by 'buflen' with the size of the string, escluding
 * the final nulterm. */
char *editorRowsToString(int *buflen) {
    char *buf = NULL, *p;
    int totlen = 0;
    int j;

    /* Compute count of bytes */
    for (j = 0; j < E.file_numrows; j++)
        totlen += E.row[j].size + 1; /* +1 is for "\n" at end of every row */
    *buflen = totlen;
    totlen++; /* Also make space for nulterm */

    p = buf = malloc(totlen);
    for (j = 0; j < E.file_numrows; j++) {
        memcpy(p, E.row[j].chars, E.row[j].size);
        p += E.row[j].size;
        *p = '\n';
        p++;
    }
    *p = '\0';
    return buf;
}

/* Insert a character at the specified position in a row, moving the remaining
 * chars on the right if needed. */
void editorRowInsertChar(erow *row, int at, int c) {
    if (at > row->size) {
        /* Pad the string with spaces if the insert location is outside the
         * current length by more than a single character. */
        int padlen = at - row->size;
        /* In the next line +2 means: new char and null term. */
        row->chars = realloc(row->chars, row->size + padlen + 2);
        memset(row->chars + row->size, ' ', padlen);
        row->chars[row->size + padlen + 1] = '\0';
        row->size += padlen + 1;
    } else {
        /* If we are in the middle of the string just make space for 1 new
         * char plus the (already existing) null term. */
        row->chars = realloc(row->chars, row->size + 2);
        memmove(row->chars + at + 1, row->chars + at, row->size - at + 1);
        row->size++;
    }
    row->chars[at] = c;
    editorUpdateRow(row);
    E.dirty++;
}

/* Append the string 's' at the end of a row */
void editorRowAppendString(erow *row, char *s, size_t len) {
    row->chars = realloc(row->chars, row->size + len + 1);
    memcpy(row->chars + row->size, s, len);
    row->size += len;
    row->chars[row->size] = '\0';
    editorUpdateRow(row);
    E.dirty++;
}

/* Delete the character at offset 'at' from the specified row. */
void editorRowDelChar(erow *row, int at) {
    if (row->size <= at) return;
    memmove(row->chars + at, row->chars + at + 1, row->size - at);
    editorUpdateRow(row);
    row->size--;
    E.dirty++;
}

/* Insert the specified char at the current prompt position. */
void editorInsertChar(int c) {
    int filerow = E.row_display_offset + E.cy;
    int filecol = E.col_display_offset + E.cx;
    erow *row = (filerow >= E.file_numrows) ? NULL : &E.row[filerow];

    /* If the row where the cursor is currently located does not exist in our
     * logical representaion of the file, add enough empty rows as needed. */
    if (!row) {
        while (E.file_numrows <= filerow)
            editorInsertRow(E.file_numrows, "", 0);
    }
    row = &E.row[filerow];
    editorRowInsertChar(row, filecol, c);
    if (E.cx == E.screen_cols - 1)
        E.col_display_offset++;
    else
        E.cx++;
    E.dirty++;
}

/* Inserting a newline is slightly complex as we have to handle inserting a
 * newline in the middle of a line, splitting the line as needed. */
void editorInsertNewline(void) {
    int filerow = E.row_display_offset + E.cy;
    int filecol = E.col_display_offset + E.cx;
    erow *row = (filerow >= E.file_numrows) ? NULL : &E.row[filerow];

    if (!row) {
        if (filerow == E.file_numrows) {
            editorInsertRow(filerow, "", 0);
            goto fixcursor;
        }
        return;
    }
    /* If the cursor is over the current line size, we want to conceptually
     * think it's just over the last character. */
    if (filecol >= row->size) filecol = row->size;
    if (filecol == 0) {
        editorInsertRow(filerow, "", 0);
    } else {
        /* We are in the middle of a line. Split it between two rows. */
        editorInsertRow(filerow + 1, row->chars + filecol, row->size - filecol);
        row = &E.row[filerow];
        row->chars[filecol] = '\0';
        row->size = filecol;
        editorUpdateRow(row);
    }
    fixcursor:
    if (E.cy == E.screen_rows - 1) {
        E.row_display_offset++;
    } else {
        E.cy++;
    }
    E.cx = 0;
    E.col_display_offset = 0;
}

/* Delete the char at the current prompt position. */
void editorDelChar() {
    int filerow = E.row_display_offset + E.cy;
    int filecol = E.col_display_offset + E.cx;
    erow *row = (filerow >= E.file_numrows) ? NULL : &E.row[filerow];

    if (!row || (filecol == 0 && filerow == 0)) return;
    if (filecol == 0) {
        /* Handle the case of column 0, we need to move the current line
         * on the right of the previous one. */
        filecol = E.row[filerow - 1].size;
        editorRowAppendString(&E.row[filerow - 1], row->chars, row->size);
        editorDelRow(filerow);
        row = NULL;
        if (E.cy == 0)
            E.row_display_offset--;
        else
            E.cy--;
        E.cx = filecol;
        if (E.cx >= E.screen_cols) {
            int shift = (E.screen_cols - E.cx) + 1;
            E.cx -= shift;
            E.col_display_offset += shift;
        }
    } else {
        editorRowDelChar(row, filecol - 1);
        if (E.cx == 0 && E.col_display_offset)
            E.col_display_offset--;
        else
            E.cx--;
    }
    if (row) editorUpdateRow(row);
    E.dirty++;
}

/**
 * ファイルをメモリ上に読み込みます。
 * 成功なら0、エラーなら1を返します。
 * @param filename 開くファイル名
 * @return 成功可否
 */
int editorOpen(char *filename) {
    FILE *fp;

    E.dirty = 0;
    free(E.filename);
    E.filename = strdup(filename);

    // リードモードでファイルを開く
    fp = fopen(filename, "r");
    if (!fp) {
        if (errno != ENOENT) {
            perror("Opening file");
            exit(1);
        }
        return 1;
    }

    char *line = NULL;
    size_t linecap = 0;
    ssize_t linelen;
    // 一行ずつ読み込む
    while ((linelen = getline(&line, &linecap, fp)) != -1) {
        if (linelen && (line[linelen - 1] == '\n' || line[linelen - 1] == '\r'))
            line[--linelen] = '\0';
        editorInsertRow(E.file_numrows, line, linelen);
    }
    free(line);
    fclose(fp);
    E.dirty = 0;
    return 0;
}

/* Save the current file on disk. Return 0 on success, 1 on error. */
int editorSave(void) {
    int len;
    char *buf = editorRowsToString(&len);
    int fd = open(E.filename, O_RDWR | O_CREAT, 0644);
    if (fd == -1) goto writeerr;

    /* Use truncate + a single write(2) call in order to make saving
     * a bit safer, under the limits of what we can do in a small editor. */
    if (ftruncate(fd, len) == -1) goto writeerr;
    if (write(fd, buf, len) != len) goto writeerr;

    close(fd);
    free(buf);
    E.dirty = 0;
    editorSetStatusMessage("%d bytes written on disk", len);
    return 0;

    writeerr:
    free(buf);
    if (fd != -1) close(fd);
    editorSetStatusMessage("Can't save! I/O error: %s", strerror(errno));
    return 1;
}

/* ============================= Terminal update ============================ */

/* We define a very simple "append buffer" structure, that is an heap
 * allocated string where we can append to. This is useful in order to
 * write all the escape sequences in a buffer and flush them to the standard
 * output in a single call, to avoid flickering effects. */
struct abuf {
    char *b;
    int len;
};

#define ABUF_INIT {NULL,0}

void abAppend(struct abuf *ab, const char *s, int len) {
    char *new = realloc(ab->b, ab->len + len);

    if (new == NULL) return;
    memcpy(new + ab->len, s, len);
    ab->b = new;
    ab->len += len;
}

void abFree(struct abuf *ab) {
    free(ab->b);
}

/**
 * VT100エスケープシーケンスを使って、スクリーン全体を描画します。
 */
void editorRefreshScreen(void) {
    int y;
    erow *r;
    char buf[32];
    struct abuf ab = ABUF_INIT;

    abAppend(&ab, "\x1b[?25l", 6); /* Hide cursor. */
    abAppend(&ab, "\x1b[H", 3); /* Go home. */

    // エディタのテキスト描画エリアを一行ずつ描画していく
    for (y = 0; y < E.screen_rows; y++) {
        // エディタのy行目に描画するファイルの行番号
        int filerow = E.row_display_offset + y;

        // これ以上描画する内容がない場合
        if (filerow >= E.file_numrows) {
            // 起動画面の表示
            if (E.file_numrows == 0 && y == E.screen_rows / 3) {
                char welcome[80];
                int welcomelen = snprintf(welcome, sizeof(welcome),
                                          "Kilo editor -- verison %s\x1b[0K\r\n", KILO_VERSION);
                int padding = (E.screen_cols - welcomelen) / 2;
                if (padding) {
                    abAppend(&ab, "~", 1);
                    padding--;
                }
                while (padding--) abAppend(&ab, " ", 1);
                abAppend(&ab, welcome, welcomelen);
            } else {
                abAppend(&ab, "~\x1b[0K\r\n", 7);
            }
        } else {
            // その行に表示するテキスト行
            r = &E.row[filerow];

            int len = r->rsize - E.col_display_offset;
            if (len > 0) {
                if (len > E.screen_cols) len = E.screen_cols;
                char *c = r->render + E.col_display_offset;
                int j;

                // 一文字ずつ描画する
                for (j = 0; j < len; j++) {
                    abAppend(&ab, c + j, 1);
                }
            }
            abAppend(&ab, "\x1b[0K", 4); // Clear line from cursor right
            abAppend(&ab, "\r\n", 2);
        }
    }

    // ステータスバーの2行分を作る

    /* １行目 */
    abAppend(&ab, "\x1b[0K", 4);
    abAppend(&ab, "\x1b[7m", 4);
    char status[80], rstatus[80];
    int len = snprintf(status, sizeof(status), "%.20s - %d lines %s",
                       E.filename, E.file_numrows, E.dirty ? "(modified)" : "");
    int rlen = snprintf(rstatus, sizeof(rstatus),
                        "%d/%d", E.row_display_offset + E.cy + 1, E.file_numrows);
    if (len > E.screen_cols) len = E.screen_cols;
    abAppend(&ab, status, len);
    while (len < E.screen_cols) {
        if (E.screen_cols - len == rlen) {
            abAppend(&ab, rstatus, rlen);
            break;
        } else {
            abAppend(&ab, " ", 1);
            len++;
        }
    }
    abAppend(&ab, "\x1b[0m\r\n", 6);

    /* 2行目 - E.statusmsgやステータスメッセージの更新時間に依る */
    abAppend(&ab, "\x1b[0K", 4);
    int msglen = strlen(E.statusmsg);
    if (msglen && time(NULL) - E.statusmsg_time < 5)
        abAppend(&ab, E.statusmsg, msglen <= E.screen_cols ? msglen : E.screen_cols);

    /* Put cursor at its current position. Note that the horizontal position
     * at which the cursor is displayed may be different compared to 'E.cx'
     * because of TABs. */
    int j;
    int cx = 1;
    int filerow = E.row_display_offset + E.cy;
    erow *row = (filerow >= E.file_numrows) ? NULL : &E.row[filerow];
    if (row) {
        for (j = E.col_display_offset; j < (E.cx + E.col_display_offset); j++) {
            if (j < row->size && row->chars[j] == TAB) cx += 7 - ((cx) % 8);
            cx++;
        }
    }
    snprintf(buf, sizeof(buf), "\x1b[%d;%dH", E.cy + 1, cx);
    abAppend(&ab, buf, strlen(buf));
    abAppend(&ab, "\x1b[?25h", 6); /* Show cursor. */
    write(STDOUT_FILENO, ab.b, ab.len);
    abFree(&ab);
}

/* Set an editor status message for the second line of the status, at the
 * end of the screen. */
void editorSetStatusMessage(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
    va_end(ap);
    E.statusmsg_time = time(NULL);
}

/* =============================== Find mode ================================ */

#define KILO_QUERY_LEN 256

void editorFind(int fd) {
    char query[KILO_QUERY_LEN + 1] = {0};
    int qlen = 0;
    int last_match = -1; /* Last line where a match was found. -1 for none. */
    int find_next = 0; /* if 1 search next, if -1 search prev. */

    /* Save the cursor position in order to restore it later. */
    int saved_cx = E.cx, saved_cy = E.cy;
    int saved_coloff = E.col_display_offset, saved_rowoff = E.row_display_offset;

    while (1) {
        editorSetStatusMessage(
                "Search: %s (Use ESC/Arrows/Enter)", query);
        editorRefreshScreen();

        int c = editorReadKey(fd);
        if (c == DEL_KEY || c == CTRL_H || c == BACKSPACE) {
            if (qlen != 0) query[--qlen] = '\0';
            last_match = -1;
        } else if (c == ESC || c == ENTER) {
            if (c == ESC) {
                E.cx = saved_cx;
                E.cy = saved_cy;
                E.col_display_offset = saved_coloff;
                E.row_display_offset = saved_rowoff;
            }
            editorSetStatusMessage("");
            return;
        } else if (c == ARROW_RIGHT || c == ARROW_DOWN) {
            find_next = 1;
        } else if (c == ARROW_LEFT || c == ARROW_UP) {
            find_next = -1;
        } else if (isprint(c)) {
            if (qlen < KILO_QUERY_LEN) {
                query[qlen++] = c;
                query[qlen] = '\0';
                last_match = -1;
            }
        }

        /* Search occurrence. */
        if (last_match == -1) find_next = 1;
        if (find_next) {
            char *match = NULL;
            int match_offset = 0;
            int i, current = last_match;

            for (i = 0; i < E.file_numrows; i++) {
                current += find_next;
                if (current == -1) current = E.file_numrows - 1;
                else if (current == E.file_numrows) current = 0;
                match = strstr(E.row[current].render, query);
                if (match) {
                    match_offset = match - E.row[current].render;
                    break;
                }
            }
            find_next = 0;

            if (match) {
                last_match = current;
                E.cy = 0;
                E.cx = match_offset;
                E.row_display_offset = current;
                E.col_display_offset = 0;
                /* Scroll horizontally as needed. */
                if (E.cx > E.screen_cols) {
                    int diff = E.cx - E.screen_cols;
                    E.cx -= diff;
                    E.col_display_offset += diff;
                }
            }
        }
    }
}

/* ========================= Editor events handling  ======================== */

/* Handle cursor position change because arrow keys were pressed. */
void editorMoveCursor(int key) {
    int filerow = E.row_display_offset + E.cy;
    int filecol = E.col_display_offset + E.cx;
    int rowlen;
    erow *row = (filerow >= E.file_numrows) ? NULL : &E.row[filerow];

    switch (key) {
        case ARROW_LEFT:
            if (E.cx == 0) {
                if (E.col_display_offset) {
                    E.col_display_offset--;
                } else {
                    if (filerow > 0) {
                        E.cy--;
                        E.cx = E.row[filerow - 1].size;
                        if (E.cx > E.screen_cols - 1) {
                            E.col_display_offset = E.cx - E.screen_cols + 1;
                            E.cx = E.screen_cols - 1;
                        }
                    }
                }
            } else {
                E.cx -= 1;
            }
            break;
        case ARROW_RIGHT:
            if (row && filecol < row->size) {
                if (E.cx == E.screen_cols - 1) {
                    E.col_display_offset++;
                } else {
                    E.cx += 1;
                }
            } else if (row && filecol == row->size) {
                E.cx = 0;
                E.col_display_offset = 0;
                if (E.cy == E.screen_rows - 1) {
                    E.row_display_offset++;
                } else {
                    E.cy += 1;
                }
            }
            break;
        case ARROW_UP:
            if (E.cy == 0) {
                if (E.row_display_offset) E.row_display_offset--;
            } else {
                E.cy -= 1;
            }
            break;
        case ARROW_DOWN:
            if (filerow < E.file_numrows) {
                if (E.cy == E.screen_rows - 1) {
                    E.row_display_offset++;
                } else {
                    E.cy += 1;
                }
            }
            break;
    }
    /* Fix cx if the current line has not enough chars. */
    filerow = E.row_display_offset + E.cy;
    filecol = E.col_display_offset + E.cx;
    row = (filerow >= E.file_numrows) ? NULL : &E.row[filerow];
    rowlen = row ? row->size : 0;
    if (filecol > rowlen) {
        E.cx -= filecol - rowlen;
        if (E.cx < 0) {
            E.col_display_offset += E.cx;
            E.cx = 0;
        }
    }
}

/* Process events arriving from the standard input, which is, the user
 * is typing stuff on the terminal. */
#define KILO_QUIT_TIMES 3

void editorProcessKeypress(int fd) {
    /* When the file is modified, requires Ctrl-q to be pressed N times
     * before actually quitting. */
    static int quit_times = KILO_QUIT_TIMES;

    int c = editorReadKey(fd);
    switch (c) {
        case ENTER:         /* Enter */
            editorInsertNewline();
            break;
        case CTRL_C:        /* Ctrl-c */
            /* We ignore ctrl-c, it can't be so simple to lose the changes
             * to the edited file. */
            break;
        case CTRL_Q:        /* Ctrl-q */
            /* Quit if the file was already saved. */
            if (E.dirty && quit_times) {
                editorSetStatusMessage("WARNING!!! File has unsaved changes. "
                                       "Press Ctrl-Q %d more times to quit.", quit_times);
                quit_times--;
                return;
            }
            exit(0);
            break;
        case CTRL_S:        /* Ctrl-s */
            editorSave();
            break;
        case CTRL_F:
            editorFind(fd);
            break;
        case BACKSPACE:     /* Backspace */
        case CTRL_H:        /* Ctrl-h */
        case DEL_KEY:
            editorDelChar();
            break;
        case PAGE_UP:
        case PAGE_DOWN:
            if (c == PAGE_UP && E.cy != 0)
                E.cy = 0;
            else if (c == PAGE_DOWN && E.cy != E.screen_rows - 1)
                E.cy = E.screen_rows - 1;
            {
                int times = E.screen_rows;
                while (times--)
                    editorMoveCursor(c == PAGE_UP ? ARROW_UP :
                                     ARROW_DOWN);
            }
            break;

        case ARROW_UP:
        case ARROW_DOWN:
        case ARROW_LEFT:
        case ARROW_RIGHT:
            editorMoveCursor(c);
            break;
        case CTRL_L: /* ctrl+l, clear screen */
            /* Just refresht the line as side effect. */
            break;
        case ESC:
            /* Nothing to do for ESC in this mode. */
            break;
        default:
            editorInsertChar(c);
            break;
    }

    quit_times = KILO_QUIT_TIMES; /* Reset it to the original value. */
}

void initEditor(void) {
    E.cx = 0;
    E.cy = 0;
    E.row_display_offset = 0;
    E.col_display_offset = 0;
    E.file_numrows = 0;
    E.row = NULL;
    E.dirty = 0;
    E.filename = NULL;
    if (getWindowSize(STDIN_FILENO, STDOUT_FILENO,
                      &E.screen_rows, &E.screen_cols) == -1) {
        perror("Unable to query the screen for size (columns / rows)");
        exit(1);
    }
    // ステータスバー用のスペースを空けておく
    E.screen_rows -= 2;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: kilo <filename>\n");
        exit(1);
    }

    initEditor();
    editorOpen(argv[1]);
    enableRawMode(STDIN_FILENO);
    editorSetStatusMessage(
            "HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find");
    while (1) {
        editorRefreshScreen();
        editorProcessKeypress(STDIN_FILENO);
    }
    return 0;
}
