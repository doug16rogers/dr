/* Copyright (c) 2015 Doug Rogers under the terms of the MIT License. */
/* See http://www.opensource.org/licenses/mit-license.html.. */
/* $Id$ */

#include "vin.h"

#include <ctype.h>
#include <string.h>

#ifndef ARRAY_COUNT
#define ARRAY_COUNT(_a) (sizeof(_a) / sizeof((_a)[0]))
#endif

const int kVinLength = 17;
const int kVinChecksumDigitIndex = 8;

/**
 * The checksum weights at each index, in array form.
 */
static const int kIndexWeight[kVinLength] = {
    8, 7, 6, 5, 4, 3, 2, 10, 0,
    9, 8, 7, 6, 5, 4, 3, 2
};   /* kIndexWeight[] */

/* ------------------------------------------------------------------------- */
int VinCharChecksumValue(char c) {
    const char cu = toupper(c);
    if (('0' <= cu) && (cu <= '9')) {
        return c - '0';
    } else if (('A' <= c) && (c <= 'Z') &&
               ('I' != c) && ('O' != c) && ('Q' != c)) {
        /*
         * It's weird how they had a mod 9 thing going then skipped a number
         * between 'R' and 'S'.
         */
        const int code = (c - 'A') + ((c >= 'S') ? 1 : 0);
        return (code % 9) + 1;
    }
    return VIN_ERROR_INVALID_CHARACTER;
}   /* VinCharChecksumValue() */

/* ------------------------------------------------------------------------- */
int VinCharForChecksum(int checksum) {
    if (checksum < 0) {
        return VIN_ERROR_UNSPECIFIED;
    }
    const int mod11 = checksum % 11;
    return (10 == mod11) ? 'X' : ('0' + mod11);
}   /* VinCharForChecksum() */

/* ------------------------------------------------------------------------- */
int VinIndexChecksumWeight(int index) {
    if ((index < 0) || (index >= ARRAY_COUNT(kIndexWeight))) {
        return VIN_ERROR_INVALID_INDEX;
    }
    return kIndexWeight[index];
}   /* VinIndexChecksumWeight() */

/* ------------------------------------------------------------------------- */
int VinChecksum(const char* vin, int* out_error_index) {
    if (NULL == vin) {
        return VIN_ERROR_INVALID_PARAMETER;
    }
    if (NULL != out_error_index) {
        *out_error_index = 0;
    }
    int rval = 0;
    for (int i = 0; i < kVinLength; ++i) {
        if (kVinChecksumDigitIndex != i) {
            const int char_value = VinCharChecksumValue(vin[i]);
            if (char_value < 0) {
                rval = VIN_ERROR_INVALID_CHARACTER;
                if (NULL != out_error_index) {
                    *out_error_index = i;
                }
                break;
            }
            rval += char_value * VinIndexChecksumWeight(i);
        }
    }
    return rval;
}   /* VinChecksum() */

/* ------------------------------------------------------------------------- */
int VinChecksumChar(const char* vin, int* out_error_index) {
    int checksum = VinChecksum(vin, out_error_index);
    if (checksum >= 0) {
        return VinCharForChecksum(checksum);
    }
    return checksum;
}   /* VinChecksumChar() */

