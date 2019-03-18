/* Copyright (c) 2003-2019 Doug Rogers under the Zero Clause BSD License. */
/* You are free to do whatever you want with this software. See LICENSE.txt. */

#ifndef __icitypes
#define __icitypes
/*****************************************************************************
*
*  TITLE:        ICI Types
*
*  DESCRIPTION:  The module "ICITypes"
*                provides type definitions to be
*                used in all programs written by ICI.
*
*  *k "%n"
*  FILE NAME:    "ICITYPES.H"
*
*  *k "%v"
*  VERSION:      "1"
*
*  REFERENCE:    None.
*
*****************************************************************************/

typedef unsigned long int  UINT32;
typedef unsigned short int UINT16;
typedef unsigned char      UINT8;

typedef unsigned int       UINT;

typedef long int  INT32;
typedef short int INT16;
typedef char      INT8;

typedef int       INT;

typedef unsigned int BIT;       /* used for bitfields */

typedef enum
{
   false = 0,
   true = 1
} BOOLEAN;


#ifndef NULL
#define NULL 0
#endif


#endif

