/* Copyright (c) 2016-2019 Doug Rogers under the Zero Clause BSD License. */
/* You are free to do whatever you want with this software. See LICENSE.txt. */

#define _GNU_SOURCE
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "text_canvas.h"

#define PROGRAM "element-chart"

/**
 * Supported size descriptors for each card in the periodic table.
 */
typedef enum card_size_e {
    CARD_SIZE_TINY,
    CARD_SIZE_SMALL,
    CARD_SIZE_MEDIUM,
    CARD_SIZE_LARGE,
    CARD_SIZE_HUGE
} card_size_t;

/**
 * Size of each card in periodic table.
 */
card_size_t g_card_size = CARD_SIZE_MEDIUM;

/**
 * @return the number of elements in @a _array.
 */
#define ARRAY_SIZE(_array)      (sizeof(_array) / sizeof((_array)[0]))

/**
 * The information about an element.
 */
typedef struct element_s {
    int row;                    /**< 0-based row in periodic table. */
    int col;                    /**< 0-based column in periodic table. */
    const char* name;           /**< American English name of element. */
    const char* symbol;         /**< Chemical symbol for element. */
    int atomic_number;          /**< Number of protons in the nucleus of element. */
    double atomic_weight;       /**< Average atomic weight of element as found on Earth. */
    double melting_point;       /**< Melting point in kelvins. */
    double boiling_point;       /**< Boining point in kelvins. */
} element_t;

/**
 * Constant designating an unknown value.
 */
#define UNK     (-1)

/**
 * From http://pastebin.com/raw/CKwm136x (from
 *   https://chemistry.stackexchange.com/questions/2793/
 *             where-can-i-find-a-downloadable-spreadsheet-of-element-properties
 */
const element_t ELEMENT_LIST[] = {
    { 0,  0, "Hydrogen",        "H",    1, 1.00794,     14.01,    20.28 },
    { 0, 17, "Helium",          "He",   2, 4.002602,     0.95,     4.216 },
    { 1,  0, "Lithium",         "Li",   3, 6.941,      553.69,  1118.15 },
    { 1,  1, "Beryllium",       "Be",   4, 9.01218,   1551,     3243 },
    { 1, 12, "Boron",           "B",    5, 10.811,    2573,     3931 },
    { 1, 13, "Carbon",          "C",    6, 12.011,    3820,     5100 },
    { 1, 14, "Nitrogen",        "N",    7, 14.00674,    63.29,    77.4 },
    { 1, 15, "Oxygen",          "O",    8, 15.9994,     54.8,     90.19 },
    { 1, 16, "Fluorine",        "F",    9, 18.998403,   53.53,    85.01 },
    { 1, 17, "Neon",            "Ne",  10, 20.1797,     48,       27.1 },
    { 2,  0, "Sodium",          "Na",  11, 22.989768,  370.96,  1156.1 },
    { 2,  1, "Magnesium",       "Mg",  12, 24.305,     922,     1363 },
    { 2, 12, "Aluminum",        "Al",  13, 26.981539,  933.5,   2740 },
    { 2, 13, "Silicon",         "Si",  14, 28.0855,   1683,     2628 },
    { 2, 14, "Phosphorus",      "P",   15, 30.973762,  317.3,    553 },
    { 2, 15, "Sulfur",          "S",   16, 32.066,     386,      717.824 },
    { 2, 16, "Chlorine",        "Cl",  17, 35.4527,    172.2,    238.6 },
    { 2, 17, "Argon",           "Ar",  18, 39.948,      83.8,     87.3 },
    { 3,  0, "Potassium",       "K",   19, 39.0983,    336.8,   1047 },
    { 3,  1, "Calcium",         "Ca",  20, 40.078,    1112,     1757 },
    { 3,  2, "Scandium",        "Sc",  21, 44.95591,  1814,     3104 },
    { 3,  3, "Titanium",        "Ti",  22, 47.88,     1933,     3560 },
    { 3,  4, "Vanadium",        "V",   23, 50.9415,   2160,     3650 },
    { 3,  5, "Chromium",        "Cr",  24, 51.9961,   2130,     2945 },
    { 3,  6, "Manganese",       "Mn",  25, 54.93805,  1517,     2235 },
    { 3,  7, "Iron",            "Fe",  26, 55.847,    1808,     3023 },
    { 3,  8, "Cobalt",          "Co",  27, 58.9332,   1768,     3143 },
    { 3,  9, "Nickel",          "Ni",  28, 58.6934,   1726,     3005 },
    { 3, 10, "Copper",          "Cu",  29, 63.546,    1356.6,   2840 },
    { 3, 11, "Zinc",            "Zn",  30, 65.39,      692.73,  1180 },
    { 3, 12, "Gallium",         "Ga",  31, 69.723,     302.93,  2676 },
    { 3, 13, "Germanium",       "Ge",  32, 72.61,     1210.6,   3103 },
    { 3, 14, "Arsenic",         "As",  33, 74.92159,  1090,      876 },
    { 3, 15, "Selenium",        "Se",  34, 78.96,      490,      958.1 },
    { 3, 16, "Bromine",         "Br",  35, 79.904,     265.9,    331.9 },
    { 3, 17, "Krypton",         "Kr",  36, 83.8,       116.6,    120.85 },
    { 4,  0, "Rubidium",        "Rb",  37, 85.4678,    312.2,    961 },
    { 4,  1, "Strontium",       "Sr",  38, 87.62,     1042,     1657 },
    { 4,  2, "Yttrium",         "Y",   39, 88.90585,  1795,     3611 },
    { 4,  3, "Zirconium",       "Zr",  40, 91.224,    2125,     4650 },
    { 4,  4, "Niobium",         "Nb",  41, 92.90638,  2741,     5015 },
    { 4,  5, "Molybdenum",      "Mo",  42, 95.94,     2890,     4885 },
    { 4,  6, "Technetium",      "Tc",  43, 97.9072,   2445,     5150 },
    { 4,  7, "Ruthenium",       "Ru",  44, 101.07,    2583,     4173 },
    { 4,  8, "Rhodium",         "Rh",  45, 102.9055,  2239,     4000 },
    { 4,  9, "Palladium",       "Pd",  46, 106.42,    1825,     3413 },
    { 4, 10, "Silver",          "Ag",  47, 107.8682,  1235.1,   2485 },
    { 4, 11, "Cadmium",         "Cd",  48, 112.411,    594.1,   1038 },
    { 4, 12, "Indium",          "In",  49, 114.818,    429.32,  2353 },
    { 4, 13, "Tin",             "Sn",  50, 118.71,     505.1,   2543 },
    { 4, 14, "Antimony",        "Sb",  51, 121.76,     903.9,   1908 },
    { 4, 15, "Tellurium",       "Te",  52, 127.6,      722.7,   1263 },
    { 4, 16, "Iodine",          "I",   53, 126.90447,  386.7,    457.5 },
    { 4, 17, "Xenon",           "Xe",  54, 131.29,     161.3,    166.1 },
    { 5,  0, "Cesium",          "Cs",  55, 132.90543,  301.6,    951.6 },
    { 5,  1, "Barium",          "Ba",  56, 137.327,   1002,     1910 },
    { 7,  2, "Lanthanum",       "La",  57, 138.9055,  1194,     3730 },
    { 7,  3, "Cerium",          "Ce",  58, 140.115,   1072,     3699 },
    { 7,  4, "Praseodymium",    "Pr",  59, 140.90765, 1204,     3785 },
    { 7,  5, "Neodymium",       "Nd",  60, 144.24,    1294,     3341 },
    { 7,  6, "Promethium",      "Pm",  61, 144.9127,  1441,     3000 },
    { 7,  7, "Samarium",        "Sm",  62, 150.36,    1350,     2064 },
    { 7,  8, "Europium",        "Eu",  63, 151.965,   1095,     1870 },
    { 7,  9, "Gadolinium",      "Gd",  64, 157.25,    1586,     3539 },
    { 7, 10, "Terbium",         "Tb",  65, 158.92534, 1629,     3296 },
    { 7, 11, "Dysprosium",      "Dy",  66, 162.5,     1685,     2835 },
    { 7, 12, "Holmium",         "Ho",  67, 164.93032, 1747,     2968 },
    { 7, 13, "Erbium",          "Er",  68, 167.26,    1802,     3136 },
    { 7, 14, "Thulium",         "Tm",  69, 168.93421, 1818,     2220 },
    { 7, 15, "Ytterbium",       "Yb",  70, 173.04,    1097,     1466 },
    { 5,  2, "Lutetium",        "Lu",  71, 174.967,   1936,     3668 },
    { 5,  3, "Hafnium",         "Hf",  72, 178.49,    2503,     5470 },
    { 5,  4, "Tantalum",        "Ta",  73, 180.9479,  3269,     5698 },
    { 5,  5, "Tungsten",        "W",   74, 183.84,    3680,     5930 },
    { 5,  6, "Rhenium",         "Re",  75, 186.207,   3453,     5900 },
    { 5,  7, "Osmium",          "Os",  76, 190.23,    3327,     5300 },
    { 5,  8, "Iridium",         "Ir",  77, 192.22,    2683,     4403 },
    { 5,  9, "Platinum",        "Pt",  78, 195.08,    2045,     4100 },
    { 5, 10, "Gold",            "Au",  79, 196.96654, 1337.58,  3080 },
    { 5, 11, "Mercury",         "Hg",  80, 200.59,     234.28,   629.73 },
    { 5, 12, "Thallium",        "Tl",  81, 204.3833,   576.6,   1730 },
    { 5, 13, "Lead",            "Pb",  82, 207.2,      600.65,  2013 },
    { 5, 14, "Bismuth",         "Bi",  83, 208.98037,  544.5,   1883 },
    { 5, 15, "Polonium",        "Po",  84, 208.9824,   527,     1235 },
    { 5, 16, "Astatine",        "At",  85, 209.9871,   575,      610 },
    { 5, 17, "Radon",           "Rn",  86, 222.0176,   202,      211.4 },
    { 6,  0, "Francium",        "Fr",  87, 223.0197,   300,      950 },
    { 6,  1, "Radium",          "Ra",  88, 226.0254,   973,     1413 },
    { 8,  2, "Actinium",        "Ac",  89, 227.0278,  1320,     3470 },
    { 8,  3, "Thorium",         "Th",  90, 232.0381,  2028,     5060 },
    { 8,  4, "Protactinium",    "Pa",  91, 231.03588, 2113,     4300 },
    { 8,  5, "Uranium",         "U",   92, 238.0289,  1405.5,   4018 },
    { 8,  6, "Neptunium",       "Np",  93, 237.048,    913,     4175 },
    { 8,  7, "Plutonium",       "Pu",  94, 244.0642,   914,     3505 },
    { 8,  8, "Americium",       "Am",  95, 243.0614,  1267,     2880 },
    { 8,  9, "Curium",          "Cm",  96, 247.0703,  1340,      UNK },
    { 8, 10, "Berkelium",       "Bk",  97, 247.0703,   UNK,      UNK },
    { 8, 11, "Californium",     "Cf",  98, 251.0796,   900,      UNK },
    { 8, 12, "Einsteinium",     "Es",  99, 252.083,     UNK,    1130 },
    { 8, 13, "Fermium",         "Fm", 100, 257.0951,  1800,      UNK },
    { 8, 14, "Mendelevium",     "Md", 101, 258.1,     1100,      UNK },
    { 8, 15, "Nobelium",        "No", 102, 259.1009,  1100,      UNK },
    { 6,  2, "Lawrencium",      "Lr", 103, 262.11,     UNK,      UNK },
    { 6,  3, "Rutherfordium",   "Rf", 104, 261,        UNK,      UNK },
    { 6,  4, "Dubnium",         "Db", 105, 262,        UNK,      UNK },
    { 6,  5, "Seaborgium",      "Sg", 106, 266,        UNK,      UNK },
    { 6,  6, "Bohrium",         "Bh", 107, 264,        UNK,      UNK },
    { 6,  7, "Hassium",         "Hs", 108, 269,        UNK,      UNK },
    { 6,  8, "Meitnerium",      "Mt", 109, 268,        UNK,      UNK },
    { 6,  9, "Darmstadtium",    "Ds", 110, 281,        UNK,      UNK },
    { 6, 10, "Roentgenium",     "Rg", 111, 280,        UNK,      UNK },
    { 6, 11, "Copernicium",     "Cn", 112, 285,        UNK,      UNK },
    { 6, 12, "Nihonium",        "Nh", 113, 284,        UNK,      UNK },
    { 6, 13, "Flerovium",       "Fl", 114, 289,        UNK,      UNK },
    { 6, 14, "Moscovium",       "Mc", 115, 288,        UNK,      UNK },
    { 6, 15, "Livermorium",     "Lv", 116, 293,        UNK,      UNK },
    { 6, 16, "Tennessine",      "Ts", 117, 294,        UNK,      UNK },
    { 6, 17, "Oganesson",       "Og", 118, 294,        UNK,      UNK },
};   /* ELEMENT_LIST[] */

/**
 * Number of elements in database.
 */
const int ELEMENT_COUNT = ARRAY_SIZE(ELEMENT_LIST);

/**
 * Number of periodic groups in the periodic table.
 */
#define GROUPS  18

/**
 * Number of lines in the periodic table (so far!). This number includes two
 * separate rows for the lanthanoids and actinoids.
 */
#define LINES   (7 + 2)

/**
 * Periodic table group (0-based) at which the gap should be inserted.
 */
#define GAP_GROUP       2

/**
 * Periodic table line at which to insert the gap before the lanthanoids and
 * actinoids.
 */
#define GAP_LINE        7

/* ------------------------------------------------------------------------- */
/**
 * Draw the periodic chart using the size indicated in @a g_card_size.
 */
void draw_periodic_chart(void) {
    text_canvas_t* tc = NULL;
    int card_x_size = 0;        /* Including border. */
    int card_y_size = 0;        /* Including border. */
    switch (g_card_size) {
    case CARD_SIZE_TINY:
        card_x_size = 5;
        card_y_size = 4;
        break;
    case CARD_SIZE_SMALL:
        card_x_size = 6;
        card_y_size = 5;
        break;
    case CARD_SIZE_MEDIUM:
        card_x_size = 7;
        card_y_size = 5;
        break;
    case CARD_SIZE_LARGE:
        card_x_size = 12;
        card_y_size = 6;
        break;
        
    case CARD_SIZE_HUGE:
        card_x_size = 15;
        card_y_size = 6;
        break;
        
    default:
        fprintf(stderr, "%s: card size %d not yet suported.\n", PROGRAM, g_card_size);
    }
    /* 1 for final border, 2 for gap for lanthanoids and actinoids. */
    tc = tc_new(GROUPS * (card_x_size - 1) + 1 + 2, LINES * (card_y_size - 1) + 1 + 2);
    for (int e = 0; e < ELEMENT_COUNT; ++e) {
        const element_t* el = &ELEMENT_LIST[e];
        int x = el->col * (card_x_size - 1) + ((el->col >= GAP_GROUP) ? 2 : 0);
        int y = el->row * (card_y_size - 1) + ((el->row >= GAP_LINE) ? 2 : 0);
        y = tc_y_size(tc) - y - card_y_size;    /* from top */
        tc_move_to(tc, x, y);
        tc_push(tc, card_x_size, card_y_size);

        tc_move_to(tc, 1, card_y_size - 2);
        tc_printf(tc, "%d", el->atomic_number);

        tc_move_to(tc, (card_x_size - 1) / 2, card_y_size - 3);
        tc_puts(tc, el->symbol);

        if (card_y_size >= 5) {
            int left_of_dot = 1;
            if (el->atomic_weight > 99.5) {
                left_of_dot = 3;
            } else if (el->atomic_weight > 9.5) {
                left_of_dot = 2;
            }
            int right_of_dot = card_x_size - 2 - 1 - left_of_dot;
            right_of_dot = (right_of_dot <= 4) ? right_of_dot : 4;
            char format[0x40] = "";
            snprintf(format, sizeof(format), "%%%d.%df", left_of_dot, right_of_dot);
            tc_move_to(tc, card_x_size - 1 - left_of_dot - 1 - right_of_dot, card_y_size - 4);
            tc_printf(tc, format, el->atomic_weight);
        }

        if (card_y_size >= 6) {
            tc_move_to(tc, 1, card_y_size - 5);
            tc_puts(tc, el->name);
        }

        tc_move_to(tc, 0, 0);
        tc_line_x(tc, card_x_size - 1);
        tc_line_y(tc, card_y_size - 1);
        tc_line_x(tc, -(card_x_size - 1));
        tc_line_y(tc, -(card_y_size - 1));
        tc_pop(tc);
    }
    tc_fprint_canvas(tc, stdout);
    tc_del(tc);
}   /* draw_grid() */

/* --------------------------------------------------------------------- */
void usage(FILE* out, int exit_code) {
    fprintf(out,
            "\n"
            "  Usage: %s [options] [pattern...]\n"
            "\n"
            "  With no arguments, print a periodic table of elements.\n"
            "  With <pattern>, print information for elements matching <pattern>.\n"
            "  If <pattern> is '*' print information for all elements.\n"
            "  Non-periodic chart elements appear with the following fields:\n"
            "    atomic-number symbol name atomic-weight melting-point-K boiling-point-K\n"
            "  A value of -1 indicates an unknown value.\n"
            "\n"
            "  OPTIONS\n"
            "\n"
            "    -h                     Print usage information.\n"
            "    -T, -S, -M, -L, -H     Tiny/Small/Medium/Large/Huge periodic table. [-M]\n"
            "\n",
            PROGRAM);
    exit(exit_code);
}   /* usage() */

/* ------------------------------------------------------------------------- */
void parse_args(int argc, char* argv[]) {
    while (1) {
        int c = getopt(argc, argv, "hTSMLH");
        if (-1 == c) {
            break;
        }
        switch (c) {
        case 'h':
            usage(stdout, 0);
            break;
        case 'T': g_card_size = CARD_SIZE_TINY; break;
        case 'S': g_card_size = CARD_SIZE_SMALL; break;
        case 'M': g_card_size = CARD_SIZE_MEDIUM; break;
        case 'L': g_card_size = CARD_SIZE_LARGE; break;
        case 'H': g_card_size = CARD_SIZE_HUGE; break;
        default:
            /* fprintf(stderr, "%s: invalid option '%c'", PROGRAM, c); */
            exit(1);
        }
    }
}   /* parse_args() */

/* --------------------------------------------------------------------- */
int main(int argc, char* argv[]) {
    parse_args(argc, argv);
    if (optind < argc) {
        for (int e = 0; e < ELEMENT_COUNT; ++e) {
            const element_t* el = &ELEMENT_LIST[e];
            int show = 0;
            for (int i = optind; !show && (i < argc); ++i) {
                show = (0 == strcmp("*", argv[i])) ||
                    (NULL != strcasestr(el->name, argv[i])) ||
                    (NULL != strcasestr(el->symbol, argv[i]));
            }
            if (show) {
                printf("  %3d  %-2s  %-14s  %8g %8g %8g\n",
                       el->atomic_number, el->symbol, el->name,
                       el->atomic_weight, el->melting_point, el->boiling_point);
            }
        }
    } else {
        draw_periodic_chart();
    }
    return 0;
}   /* main() */
