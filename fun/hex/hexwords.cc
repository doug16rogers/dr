#include <stdiostr.h>

#include "icitypes.h"


const char hex_alpha[] = "OISGABCDEF";
const char hex_digit[] = "0156ABCDEF";

#define DIGITS (sizeof(hex_alpha) - 1)

const UINT8 can_be_in_order[DIGITS][DIGITS] =
{  //         O  I   S  G   A  B   C  D   E  F
   /* O */ {  1, 1,  1, 1,  1, 1,  1, 1,  1, 1 }, // O
   /* I */ {  1, 0,  1, 1,  1, 1,  1, 1,  1, 1 }, // I
   /* S */ {  1, 1,  1, 0,  1, 0,  1, 0,  1, 0 }, // S
   /* G */ {  1, 1,  0, 1,  1, 0,  0, 0,  1, 0 }, // G
   /* A */ {  0, 1,  1, 1,  0, 1,  1, 1,  0, 1 }, // A
   /* B */ {  1, 1,  0, 0,  1, 1,  0, 0,  1, 0 }, // B
   /* C */ {  1, 1,  0, 0,  1, 0,  0, 0,  1, 0 }, // C
   /* D */ {  1, 1,  0, 0,  1, 0,  0, 1,  1, 0 }, // D
   /* E */ {  0, 1,  1, 1,  1, 1,  1, 1,  1, 1 }, // E
   /* F */ {  1, 1,  0, 0,  1, 0,  0, 0,  1, 1 }  // F
};




void Generate_4_Letter_Words()
{
   for (UINT letter_1 = 0; letter_1 < DIGITS; letter_1++)
   {
      for (UINT letter_2 = 0; letter_2 < DIGITS; letter_2++)
      {
         if (!can_be_in_order[letter_1][letter_2])
         {
            continue;
         }

         for (UINT letter_3 = 0; letter_3 < DIGITS; letter_3++)
         {
            if (!can_be_in_order[letter_2][letter_3])
            {
               continue;
            }

            for (UINT letter_4 = 0; letter_4 < DIGITS; letter_4++)
            {
               if (!can_be_in_order[letter_3][letter_4])
               {
                  continue;
               }

               printf("%c%c%c%c = %c%c%c%c\n",
                         hex_alpha[letter_1],
                         hex_alpha[letter_2],
                         hex_alpha[letter_3],
                         hex_alpha[letter_4],
                         hex_digit[letter_1],
                         hex_digit[letter_2],
                         hex_digit[letter_3],
                         hex_digit[letter_4]);
            }
         }
      }
   }

}   // Generate_4_Letter_Words


void Generate_3_Letter_Words()
{
   for (UINT letter_1 = 0; letter_1 < DIGITS; letter_1++)
   {
      for (UINT letter_2 = 0; letter_2 < DIGITS; letter_2++)
      {
         if (!can_be_in_order[letter_1][letter_2])
         {
            continue;
         }

         for (UINT letter_3 = 0; letter_3 < DIGITS; letter_3++)
         {
            if (!can_be_in_order[letter_2][letter_3])
            {
               continue;
            }

            printf("%c%c%c = %c%c%c\n",
                      hex_alpha[letter_1],
                      hex_alpha[letter_2],
                      hex_alpha[letter_3],
                      hex_digit[letter_1],
                      hex_digit[letter_2],
                      hex_digit[letter_3]);
         }
      }
   }

}   // Generate_3_Letter_Words



void Generate_2_Letter_Words()
{
   for (UINT letter_1 = 0; letter_1 < DIGITS; letter_1++)
   {
      for (UINT letter_2 = 0; letter_2 < DIGITS; letter_2++)
      {
         if (!can_be_in_order[letter_1][letter_2])
         {
            continue;
         }

         printf("%c%c = %c%c\n",
                   hex_alpha[letter_1],
                   hex_alpha[letter_2],
                   hex_digit[letter_1],
                   hex_digit[letter_2]);
      }
   }

}   // Generate_2_Letter_Words


void main(void)
{
   Generate_2_Letter_Words();
   Generate_3_Letter_Words();
   Generate_4_Letter_Words();
}
