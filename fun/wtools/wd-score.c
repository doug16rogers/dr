/* Copyright (c) 2019 Doug Rogers under the Zero Clause BSD License. */
/* You are free to do whatever you want with this software. See LICENSE.txt. */

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/*                        abcdefghijklmnopqrstuvwxyz */
const char *WWF_SCORES = "144214331:524214:11125483:";
const char *SCR_SCORES = "1332142418513113:11114484:";

const char *g_scores = NULL;

int g_show_words = 1;

#ifdef __GNUC__
void usage(FILE *f, int exit_code) __attribute((noreturn));
#endif
void usage(FILE *f, int exit_code) {
    fprintf(f, "SYNOPSIS\n");
    fprintf(f, "    wd-score [OPTIONS] [words]\n");
    fprintf(f, "\n");
    fprintf(f, "DESCRIPTION\n");
    fprintf(f, "    wd-score will score words using Scrabble or Words-with-Friends tile\n");
    fprintf(f, "    values.\n");
    fprintf(f, "\n");
    fprintf(f, "    If no words are provided on the command line, words will be read from\n");
    fprintf(f, "    stdin.\n");
    fprintf(f, "\n");
    fprintf(f, "OPTIONS\n");
    fprintf(f, "    -h      Print this help info.\n");
    fprintf(f, "    -s      Use Scrabble tile values.\n");
    fprintf(f, "    -w      Use Words-with-Friends tile values. [default]\n");
    fprintf(f, "    -n      Suppress printing words along with scores.\n");
    fprintf(f, "\n");
    exit(exit_code);
}

int word_score(const char *w) {
    int score = 0;
    for (; *w; w++) {
        unsigned char c = tolower(*w);
        if (('a' <= c) && (c <= 'z')) {
            score += g_scores[c - 'a'] - '0';
        }
    }
    return score;
}

void print_word_score(const char *w) {
    int score = word_score(w);
    if (g_show_words) {
        printf("%d %s\n", score, w);
    } else {
        printf("%d\n", score);
    }
}

void print_scores_for_line(char *line) {
    char *tok = strtok(line, " \t\r\n\"'");
    while (tok != NULL) {
        print_word_score(tok);
        tok = strtok(NULL, " \t\r\n\"'");
    }
}

int main(int argc, char *argv[]) {
    int ch = 0;
    g_scores = WWF_SCORES;
    while ((ch = getopt(argc, argv, "hswn")) > 0) {
        switch (ch) {
        case 'h': usage(stdout, 0); break;
        case 's': g_scores = SCR_SCORES; break;
        case 'w': g_scores = WWF_SCORES; break;
        case 'n': g_show_words = 0; break;
        default:
            fprintf(stderr, "wd-score: invalid option -%c; use -h for help\n", ch);
            return 1;
        }
    }
    argc -= optind;
    argv += optind;
    if (argc > 0) {
        for (int i = 0; i < argc; ++i) {
            print_word_score(argv[i]);
        }
    } else {
        char *line = NULL;
        size_t line_length = 0;
        while (getline(&line, &line_length, stdin) > 0) {
            print_scores_for_line(line);
        }
        free(line);
    }
    return 0;
}
