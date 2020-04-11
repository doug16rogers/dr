/* Copyright (c) 2016-2019 Doug Rogers under the Zero Clause BSD License. */
/* You are free to do whatever you want with this software. See LICENSE.txt. */

#include <assert.h>
#include <ctype.h>
#include <libgen.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "../charts/link_list.h"

/**
 * Name of this program; this is set to `basename(argv[0])`.
 */
const char *g_program = "wd-match";

/**
 * Longest pattern supported.
 */
#define MAX_PATTERN_LEN 0x20

/**
 * Letters to use, in order, for the canonical representation of a pattern to
 * match.
 */
const unsigned char CANONICAL_MAPPING[] = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-";

/**
 * Whether verbose messages should be emitted.
 */
int g_verbose = 0;

/**
 * Length of shortest pattern added.
 */
size_t g_min_pattern_len = SIZE_T_MAX;

/**
 * Length of longest pattern added.
 */
size_t g_max_pattern_len = 0;

/**
 * List of patterns and their permutations/rotations.
 */
list_t LIST_INIT(g_pattern_list);

typedef struct {
    link_t link;
    size_t pattern_len;
    unsigned char pattern[MAX_PATTERN_LEN];
} pattern_t;

/* ------------------------------------------------------------------------- */
/**
 * Prints usage information to @p file.
 *
 * Note: This function does not return.
 *
 * @param file - FILE stream to which to write the usage information.
 *
 * @param exit_code - value to pass to exit() when ending program.
 */
void usage(FILE* file, int exit_code) __attribute__((noreturn));
void usage(FILE* file, int exit_code) {
    fprintf(file,
            "\n"
            "USAGE\n"
            "    %s [options] pattern...\n"
            "\n"
            "DESCRIPTION\n"
            "    %s reads lines from stdin and echoes them only if they match <pattern>,\n"
            "    where <pattern> is a template of letters. See EXAMPLES.\n"
            "\n"
            "OPTIONS\n"
            "    -h  Show this usage information.\n"
            "    -v  Print verbose (debug) messages to stderr.\n"
            "    -i  Comparisons and patterns are case-insensitive.\n"
            "    -s  Check shifts (rotations) of pattern.\n"
            "    -a  Check all permutations of pattern (same as `wd-in`)\n"
            "\n"
            "EXAMPLES:\n"
            "\n"
            "    $ echo -e 'name\\nanna\\ngogo\\npeep\\nlull' | %s abba\n"
            "    anna\n"
            "    peep\n"
            "    $ echo -e 'name\\nanna\\ngogo\\npeep\\nlull' | %s -a xxyy\n"
            "    anna\n"
            "    gogo\n"
            "    peep\n"
            "\n", g_program, g_program, g_program, g_program);
    exit(exit_code);
}

/* ------------------------------------------------------------------------- */
/**
 * Print a formatted message if `g_verbose` is non-zero.
 */
void print_verbose(const char* format, ...) {
    if (g_verbose) {
        char text[0x400] = "";
        va_list va;
        va_start(va, format);
        vsnprintf(text, sizeof(text), format, va);
        va_end(va);
        printf("%s: %s\n", g_program, text);
    }
}

/* ------------------------------------------------------------------------- */
/**
 * @return 1 if @p line of length @p line_length matches the given letter @p
 * pattern.
 *
 * @todo Optimize the pattern searching by precalculating all the lookups.
 */
int pattern_matches(const char* line, size_t line_length, const char* pattern) {
    print_verbose("pattern=\"%s\" line=\"%s\"", pattern, line);
    if (line_length != strlen(pattern)) {
        return 0;
    }
    unsigned char line_char_for_pattern[0x100] = "";
    unsigned char pattern_for_line_char[0x100] = "";
    size_t i = 0;
    for (i = 0; i < line_length; ++i) {
        unsigned char pch = pattern[i];
        unsigned char lch = line[i];
        if ((0 == line_char_for_pattern[pch]) &&
            (0 == pattern_for_line_char[lch])) {
            line_char_for_pattern[pch] = lch;
            pattern_for_line_char[lch] = pch;
        } else if ((line_char_for_pattern[pch] != lch) ||
                   (pattern_for_line_char[lch] != pch)) {
            return 0;
        }
    }
    return 1;
}

/* ------------------------------------------------------------------------- */
int pattern_is_in_list(const unsigned char *pattern) {
    list_foreach_struct(
        &g_pattern_list, pat, pattern_t, link,
        if (0 == strcmp((const char *)pattern, (const char *)pat->pattern)) {
            return 1;
        }
    );
    return 0;
}

/* ------------------------------------------------------------------------- */
void add_pattern(const unsigned char *pattern) {
    pattern_t *pat = NULL;
    size_t len = 0;
    pat = calloc(1, sizeof(pattern_t));
    if (NULL == pat) {
        fprintf(stderr, "%s: out of memory\n", g_program);
        exit(127);
    }
    len = strlen((const char *) pattern);
    if (len < g_min_pattern_len) {
        g_min_pattern_len = len;
    }
    if (len > g_max_pattern_len) {
        g_max_pattern_len = len;
    }
    pat->pattern_len = len;
    strncpy((char *) pat->pattern, (const char *) pattern, sizeof(pat->pattern) - 1);
    link_insert_prev(&g_pattern_list, &pat->link);
    print_verbose("canonicalized pattern \"%s\" added", pattern);
}

/* ------------------------------------------------------------------------- */
typedef struct {
    unsigned char ch;
    unsigned char count;
} char_count_t;

int char_count_cmp(const void *v0, const void *v1) {
    const char_count_t * cc0 = v0;
    const char_count_t * cc1 = v1;
    if (cc0->count < cc1->count) {
        return -1;
    }
    if (cc0->count > cc1->count) {
        return +1;
    }
    return 0;
}

/* ------------------------------------------------------------------------- */
void canonicalize_string(unsigned char *s, int case_insensitive, int sort_by_count) {
    char_count_t char_count[MAX_PATTERN_LEN] = {{0}};
    unsigned char from_s[0x100] = {0};
    unsigned char sc = 0;
    size_t next_char = 0;
    size_t len = 0;
    size_t i = 0;

    len = strlen((const char *) s);
    len = (len > MAX_PATTERN_LEN) ? MAX_PATTERN_LEN : len;
    s[len] = 0;

    assert(sizeof(CANONICAL_MAPPING) > MAX_PATTERN_LEN);

    /* print_verbose("canonicalize_string() s=\"%s\" at start", s); */
    if (case_insensitive) {
        for (i = 0; i < len; ++i) {
            s[i] = tolower(s[i]);
        }
        /* print_verbose("canonicalize_string() s=\"%s\" after case_insensitive", s); */
    }
    if (sort_by_count) {
        memset(from_s, 0xFF, sizeof(from_s));
        for (i = 0; i < len; ++i) {
            sc = s[i];
            if (from_s[sc] == 0xFF) {
                from_s[sc] = next_char;
                char_count[next_char].ch = sc;
                char_count[next_char].count = 1;
                next_char++;
            } else {
                char_count[from_s[sc]].count++;
            }
        }
        /* for (i = 0; i < next_char; ++i) { */
        /*     printf("char_count[%zu] = { .ch = '%c', .count = %u }\n", i, char_count[i].ch, char_count[i].count); */
        /* } */
        qsort(char_count, next_char, sizeof(char_count_t), char_count_cmp);
        /* for (i = 0; i < next_char; ++i) { */
        /*     printf("char_count[%zu] = { .ch = '%c', .count = %u }\n", i, char_count[i].ch, char_count[i].count); */
        /* } */
        i = 0;
        while (next_char > 0) {
            next_char--;
            while (char_count[next_char].count > 0) {
                s[i++] = char_count[next_char].ch;
                char_count[next_char].count--;
            }
        }
        next_char = 0;
        memset(from_s, 0, sizeof(from_s));
        /* print_verbose("canonicalize_string() s=\"%s\" after sort_by_count", s); */
    }
    for (i = 0; i < len; ++i) {
        sc = s[i];
        if (0 == from_s[sc]) {
            from_s[sc] = CANONICAL_MAPPING[next_char++];
        }
        s[i] = from_s[sc];
    }
    /* print_verbose("canonicalize_string() s=\"%s\" after canonicalize", s); */
}

/* ------------------------------------------------------------------------- */
void canonicalize_string2(unsigned char *tgt, const unsigned char * original_pattern, int case_insensitive,
                          int sort_by_count) {
    strncpy((char *) tgt, (const char *) original_pattern, MAX_PATTERN_LEN);
    canonicalize_string(tgt, case_insensitive, sort_by_count);
}

/* ------------------------------------------------------------------------- */
void add_pattern_rotations(unsigned char *pattern) {
    size_t len = strlen((const char *) pattern);
    unsigned char canonical_pattern[MAX_PATTERN_LEN + 1] = "";
    for (size_t i = 0; i < len; ++i) {
        canonicalize_string2(canonical_pattern, pattern, 0, 0);
        if (!pattern_is_in_list(canonical_pattern)) {
            add_pattern(canonical_pattern);
        } else {
            print_verbose("rotated pattern \"%s\" is canonical \"%s\"; already in list", pattern, canonical_pattern);
        }
        unsigned char p0 = pattern[0];
        memmove(&pattern[0], &pattern[1], len - 1);
        pattern[len - 1] = p0;
    }
}

#if 0
/*
 * This works and was fun to develop, but it will not complete in the age of
 * the universe if a long pattern is added.
 */
/* ------------------------------------------------------------------------- */
static void add_permutation(const unsigned char *pattern) {
    unsigned char canonical_pattern[MAX_PATTERN_LEN + 1] = "";
    canonicalize_string2(canonical_pattern, pattern, 0, 0);
    if (!pattern_is_in_list(canonical_pattern)) {
        add_pattern(canonical_pattern);
    } else {
        print_verbose("permuted pattern \"%s\" is canonical \"%s\"; already in list", pattern, canonical_pattern);
    }
}

/* ------------------------------------------------------------------------- */
static void iterate_permutations(void (*perm_cb)(const unsigned char*), unsigned char* l, int llen,
                                 unsigned char* r, int rlen) {
    if (0 == rlen) {
        perm_cb(l);
    } else {
        l[llen] = r[0];
        l[llen+1] = 0;
        iterate_permutations(perm_cb, l, llen + 1, r + 1, rlen - 1);
        for (int ri = 1; ri < rlen; ++ri) {
            unsigned char tmp = l[llen];
            l[llen] = r[ri];
            r[ri] = tmp;
            iterate_permutations(perm_cb, l, llen + 1, r + 1, rlen - 1);
        }
        /* Restore original order for caller. */
        for (int ri = 0; ri < rlen - 1; ri++) {
            r[ri] = r[ri+1];
        }
        r[rlen - 1] = l[llen];
    }
}

/* ------------------------------------------------------------------------- */
void add_pattern_permutations(unsigned char *pattern) {
    unsigned char left_pattern[MAX_PATTERN_LEN + 1] = "";
    iterate_permutations(add_permutation, left_pattern, 0, pattern, strlen((const char *) pattern));
}

#else // if 0

/* ------------------------------------------------------------------------- */
/**
 * @return 1 if all the letters in @p line are in @p pattern.
 */
int word_in(const unsigned char* line, size_t line_length, const unsigned char* pattern) {
    unsigned char pat[MAX_PATTERN_LEN + 1] = "";
    size_t plen = 0;

    strncpy((char *) pat, (const char *) pattern, MAX_PATTERN_LEN);
    plen = strlen((const char *) pat);
    assert(plen == line_length);

    for (size_t i = 0; i < line_length; ++i) {
        char* found = strchr((const char *) pat, (char) line[i]);
        if (NULL == found) {
            return 0;
        }
        // Remove by putting the last char in this spot.
        plen--;
        *found = pat[plen];
        pat[plen] = 0;
    }
    return 1;
}

#endif // else we want to complete before Earth dies.

/* ------------------------------------------------------------------------- */
void add_user_pattern(const char *user_pattern, int expansion_level, int case_insensitive, int sort_by_count) {
    unsigned char pattern[MAX_PATTERN_LEN + 1] = "";

    strncpy((char *)pattern, user_pattern, MAX_PATTERN_LEN);
    canonicalize_string(pattern, case_insensitive, sort_by_count);
    print_verbose("canonicalize_string(\"%s\") -> \"%s\"", user_pattern, pattern);
    if (pattern_is_in_list(pattern)) {
        print_verbose("canonicalized pattern\"%s\" already added; skipping", pattern);
        return;
    }
    if (expansion_level >= 2) {
        /* add_pattern_permutations(pattern); */
        add_pattern(pattern);
    } else if (expansion_level >= 1) {
        add_pattern_rotations(pattern);
    } else {
        add_pattern(pattern);
    }
}

/* ------------------------------------------------------------------------- */
/**
 * Main program. Parses command line arguments. See Usage().
 *
 * @param argc - number of command line arguments, including program name.
 *
 * @param argv - list of pointers to command line argument strings.
 *
 * @return the program's exit code: 0 on success, something else on failure.
 */
int main(int argc, char* argv[]) {
    int ch = 0;
    int pattern_expansion = 0;
    int case_insensitive = 0;

    g_program = basename(argv[0]);

    while ((ch = getopt(argc, argv, "hvisa")) != EOF) {
        switch (ch) {
        case 'h': usage(stdout, 0); break;
        case 'v': g_verbose = 1; break;
        case 'i': case_insensitive = 1; break;
        case 's': pattern_expansion = 1; break;
        case 'a': pattern_expansion = 2; break;
        default:
            fprintf(stderr, "%s: invalid option -%c; use '%s -h' for usage\n", g_program, ch, g_program);
            exit(1);
        }
    }

    argc -= optind;
    argv += optind;

    if (argc < 1) {
        fprintf(stderr, "%s: no patterns provided\n\n", g_program);
        usage(stderr, 2);
    }

    for (int i = 0; i < argc; ++i) {
        if (strlen(argv[i]) >  MAX_PATTERN_LEN) {
            fprintf(stderr, "%s: pattern \"%s\" is too long (max=%u)\n", g_program, argv[i], MAX_PATTERN_LEN);
            exit(3);
        }
        add_user_pattern(argv[i], pattern_expansion, case_insensitive, 2 == pattern_expansion);
    }

    int sort_by_count = 2 == pattern_expansion;
    int line_no = 0;
    char line[0x0400];
    while (NULL != fgets(line, sizeof(line), stdin)) {
        size_t wlen = strlen(line);
        unsigned char canonical_line[MAX_PATTERN_LEN + 1] = "";
        line_no++;
        if (wlen > 0) {
            if (line[wlen-1] == '\n') {
                line[wlen-1] = 0;
                --wlen;
            }
        }
        if (wlen == 0) {
            continue;
        }
        if ((wlen < g_min_pattern_len) || (g_max_pattern_len < wlen)) {
            print_verbose("item #%u length %zu not in [%zu .. %zu] pattern lengths; ignoring",
                          line_no, wlen, g_min_pattern_len, g_max_pattern_len);
            continue;
        }
        strncpy((char *) canonical_line, line, MAX_PATTERN_LEN);
        canonicalize_string(canonical_line, case_insensitive, sort_by_count);
        print_verbose("checking item #%u \"%s\" canonical%s \"%s\"", line_no, line,
                      sort_by_count ? " sorted by count" : "", canonical_line);
        list_foreach_struct(
            &g_pattern_list, pat, pattern_t, link,
            if (wlen != pat->pattern_len) {
                print_verbose("line length %zu != %zu pattern length ");
                continue;
            }
            if (sort_by_count) {
                print_verbose("checking with word_in(\"%s\", \"%s\")", canonical_line, pat->pattern);
                if (word_in(canonical_line, wlen, pat->pattern)) {
                    printf("%s\n", line);
                    break;
                }
            } else {
                print_verbose("checking with strcmp(\"%s\", \"%s\")", canonical_line, pat->pattern);
                if (0 == strcmp((const char *) canonical_line, (const char *) pat->pattern)) {
                    printf("%s\n", line);
                    break;
                }
            }
        );
    }
    
    list_foreach_struct(
        &g_pattern_list, pat, pattern_t, link,
        link_remove(&pat->link);
        free(pat);
    );
    return 0;
}   /* main() */
