/* Copyright (c) 2015 Doug Rogers under the terms of the MIT License. */
/* See http://www.opensource.org/licenses/mit-license.html.. */
/* $Id$ */

#ifndef VIN_VIN_H_
#define VIN_VIN_H_

#ifdef __cplusplus
extern "C" {
#endif

#define VIN_ERROR_UNSPECIFIED           (-23201)
#define VIN_ERROR_INVALID_PARAMETER     (-23202)
#define VIN_ERROR_INVALID_CHARACTER     (-23203)
#define VIN_ERROR_INVALID_INDEX         (-23204)

/**
 * The length of a VIN. This never changes.
 */
extern const int kVinLength;

/**
 * The 0-based index of the check digit within the VIN. This corresponds to
 * the 9th digit when counted ordinally.
 */
extern const int kVinChecksumDigitIndex;

/* ------------------------------------------------------------------------- */
/**
 * @param c - one of the characters of a VIN.
 *
 * @return the value associated with a given VIN character for checksum
 * purposes, or a negative VIN_ERROR_xxx value on error.
 */
int VinCharChecksumValue(char c);

/* ------------------------------------------------------------------------- */
/**
 * @param checksum - A VIN checksum as returned by VinChecksum().
 *
 * @return the character associated with the @a checksum, or a negative
 * VIN_ERROR_xxx int value on error.
 */
int VinCharForChecksum(int checksum);

/* ------------------------------------------------------------------------- */
/**
 * @param index - A 0-based index into the 17-digit VIN.
 *
 * @return the checksum weight for the index, or a negative VIN_ERROR_xxx
 * value on error.
 */
int VinIndexChecksumWeight(int index);

/* ------------------------------------------------------------------------- */
/**
 * Calculate the VIN checksum using the ISO 3779 standard.
 *
 * @param vin - A 17-character string whose VIN checksum is to be determined.
 *
 * @param out_index - optional output to hold the index where any error
 * occurred. Will be set to 0 on success.
 *
 * @return the VIN checksum, or a negative VIN_ERROR_xxx int value on error.
 */
int VinChecksum(const char* vin, int* out_error_index);

/* ------------------------------------------------------------------------- */
/**
 * Calculate the VIN checksum using the ISO 3779 standard.
 *
 * @param vin - A 17-character string whose VIN checksum is to be determined.
 *
 * @param out_index - optional output to hold the index where any error
 * occurred. Will be set to 0 on success.
 *
 * @return the VIN checksum character, or a negative VIN_ERROR_xxx int value
 * on error.
 */
int VinChecksumChar(const char* vin, int* out_error_index);

#ifdef __cplusplus
}
#endif

#endif  // VIN_VIN_H_
