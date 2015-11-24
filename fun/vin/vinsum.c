/* Copyright (c) 2015 FireEye Incorporated. All rights reserved. */

/*
 * This program calculates, displays and checks the checksum of a Vehicle
 * Identification Number (VIN). The VIN is a 17-digit alphanumeric code whose
 * 9th digit is a checksum that is calculated in a bizarre way. See ISO 3779.
 *
 * Links of interest:
 *  http://www.autocalculator.org/VIN/VIN-Checkdigit.aspx#.VlSlb2SrQsk
 *  http://www.autohausaz.com/vw-auto-parts/vw-vehicle-identification-numbers.html
 *  https://www.iso.org/obp/ui/#iso:std:iso:3779:ed-4:v1:en
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vin.h"

/**
 * Name of this program.
 */
#define PROGRAM  "vinsum"

/* ------------------------------------------------------------------------- */
/**
 * Prints usage information to stderr.
 *
 * Note: This function does not return.
 *
 * @param exit_code - value to pass to exit() when ending program.
 */
void usage(int exit_code) __attribute__((noreturn));
void usage(int exit_code) {
    fprintf(stderr,
            "Usage: %s <vin>..."
            
            "\n"
            "    %s calculates and checks the checksum digit of a 17-digit VIN (vehicle\n"
            "    identification number).\n",
            PROGRAM, PROGRAM);
    exit(exit_code);
}   /* usage() */

/* ------------------------------------------------------------------------- */




/* ------------------------------------------------------------------------- */
/**
 * Main program. Parses command line arguments. See usage().
 *
 * @param argc - number of command line arguments, including program name.
 *
 * @param argv - list of pointers to command line argument strings.
 *
 * @return the program's exit code: 0 on success, something else on failure.
 */
int main(int argc, char* argv[]) {
    if (argc < 2) {
        usage(1);
    }

    int rval = 0;
    for (int i = 1; i < argc; ++i) {
        char* vin = argv[i];
        if (strlen(vin) != kVinLength) {
            fprintf(stderr, "%s: invalid VIN '%s'; must have exactly 17 characters.\n", PROGRAM, vin);
            rval = 1;
        } else {
            int error_index = 0;
            int csum = VinChecksum(vin, &error_index);
            if (csum < 0) {
                fprintf(stderr, "%s: invalid VIN '%s'; character '%c' at index %d is invalid.\n",
                        PROGRAM, vin, vin[error_index] ? vin[error_index] : '?', error_index);
                rval = 1;
            } else {
                char csum_char = VinCharForChecksum(csum);
                printf("%04u=%c\t%s\n", csum, csum_char, vin);
                if (csum_char != vin[kVinChecksumDigitIndex]) {
                    fprintf(stderr, "%s: VIN '%s' calculated check digit '%c' does not match digit %u, '%c'.\n",
                            PROGRAM, vin, csum_char, kVinChecksumDigitIndex + 1, vin[kVinChecksumDigitIndex]);
                    rval = 2;
                }
            }
        }
    }

    return rval;
}   /* main() */
