#include <ctype.h>
#include <dir.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>



#define ADVANCE_TO_END(s)       while (*s) s++
#define MAXNAME                 (MAXFILE + MAXEXT)
#define TERMINATE(s)            s[sizeof(s)-1] = 0


//===========================================================================
//
//  Global Types...
//
//===========================================================================

   //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   //!!!!
   //!!!!  This is where you define your own return codes...
   //!!!!
   //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

typedef enum
{
   success = 0,         // program ran successfully

   commandline_error,   // error in a commandline parameter or switch
   could_not_open_file, // input file not openable

} DOS_Return_Codes;

//===========================================================================
//
//  Global Data...
//
//===========================================================================

char run_file[MAXNAME];         // name for executing program
char run_path[MAXPATH];         // directory of file that was executed

#define DISPLAY_ASCII   1
#define COLUMNS         4
#define FILE_OFFSET     0UL
#define NUMBER_OF_WORDS 0xFFFFFFFFUL

char file_name[0x80] = "";

unsigned int columns = COLUMNS;
char display_ascii = DISPLAY_ASCII;
unsigned long int file_offset = FILE_OFFSET;
unsigned long int number_of_words = NUMBER_OF_WORDS;

const char* minus_plus[] = { "-", "+" };  // text for flags


//===========================================================================
//
//  Function Prototypes...
//
//===========================================================================

/*****************************************************************************
*
*  TITLE:        Initialize
*
*  DESCRIPTION:  The function "Initialize" loads the commandline arguments.
*                It returns success when all parameters are loaded
*                successfully.
*                Otherwise, it returns the first non-success return code
*                from the Load_Argument() function.
*
*  REFERENCE:    None.
*
*****************************************************************************/

DOS_Return_Codes Initialize(

   int count,                   // count of commandline arguments
   char* argument[]);           // list of commandline arguments

/*****************************************************************************
*
*  TITLE:        Load Argument
*
*  DESCRIPTION:  The function "Load_Argument" loads a single
*                argument from the commandline.
*                If the argument begins with either '-' or '/',
*                it is loaded as an option (via Load_Option()).
*                If the argument begins with '@', it is loaded
*                as an argument file (via Load_Argument_File()).
*
*                If the argument is accepted, Load_Argument()
*                returns success, otherwise commandline_error.
*
*  REFERENCE:    None.
*
*****************************************************************************/

DOS_Return_Codes Load_Argument(

   char* argument);     // argument to load

/*****************************************************************************
*
*  TITLE:        Load Argument File
*
*  DESCRIPTION:  The function "Load_Argument_File" opens the given file and
*                reads each line as if it were an argument/option.
*                The line comment character is ';' -- that is, any line
*                whose first non-blank character is ';' will be skipped.
*                If the given file can not be opened,
*                Load_Argument_File() will return commandline_error and
*                print an error message.
*                If Load_Argument() returns anything other than success,
*                Load_Argument_File() will stop processing and return the
*                error code.
*                Otherwise, it returns success.
*
*  REFERENCE:    None.
*
*****************************************************************************/

DOS_Return_Codes Load_Argument_File(

   char* filename);     // name of file to load

/*****************************************************************************
*
*  TITLE:        Load Flag
*
*  DESCRIPTION:  The function "Load_Flag" loads a flag
*                and increments the options string from
*                which it reads the flag's value.
*                The flag is assumed to be a character,
*                and will be set to 0 ('\x00') if
*                a minus character ('-') is found.
*                If any other character is found,
*                then the value of flag will be 1 ('\x01').
*                If a minus or plus ('+') is found,
*                the string pointer is incremented to
*                point to the next character in
*                the options list.
*
*  REFERENCE:    None.
*
*****************************************************************************/

void Load_Flag(

   char* flag,          // pointer to flag variable
   char** string);      // string to check for +/-

/*****************************************************************************
*
*  TITLE:        Load Option
*
*  DESCRIPTION:  The function "Load_Option" loads a string as a set of
*                options.  More than one option may reside in the string.
*                Returns success on success, commandline_error on failure.
*
*  REFERENCE:    None.
*
*****************************************************************************/

DOS_Return_Codes Load_Option(

   char* option);       // the option/switch string to load

/*****************************************************************************
*
*  TITLE:        Main
*
*  DESCRIPTION:  The function "main" gets the commandline arguments
*                and sends an error code back to DOS.
*                See the definition of DOS_Return_Codes
*                in the "Global Types" section.
*
*  REFERENCE:    None.
*
*****************************************************************************/

DOS_Return_Codes main(          // 0 on success, error code on failure

   int count,                   // count of commandline arguments
   char* argument_list[]);      // the commandline arguments

/*****************************************************************************
*
*  TITLE:        Usage
*
*  DESCRIPTION:  The procedure "Usage" displays usage information for the
*                program.
*
*  REFERENCE:    None.
*
*****************************************************************************/

void Usage(void);


//===========================================================================
//
//  Function Bodies...
//
//===========================================================================


static char hex_table[16] = "0123456789ABCDEF";


/*****************************************************************************
*
*  TITLE:        Hex Word 32 String
*
*  DESCRIPTION:  The subprogram "Hex_Word_32_String"
*                dumps (only) up to 4 32-bit words into a string
*                in hexadecimal format.
*
*                It returns a pointer to the resulting
*                ASCIIZ string.
*
*  REFERENCE:    None.
*
*****************************************************************************/

char* Hex_Word_32_String(
   void* data,
   int length,
   int width)
{
   static char text[0x200];
   static char ascii[0x80];
   char* s = text;
   char* a = ascii;
   char* c = (char*) data;
   unsigned long int* input = (unsigned long int*) data;

   int i;

   s = text;
   a = ascii;

   *a++ = '|';

   for (i = 0; i < width; i++)
   {
      if (i < length)
      {
         *s++ = hex_table[(*input >> 28) & 0x0f];
         *s++ = hex_table[(*input >> 24) & 0x0f];
         *s++ = hex_table[(*input >> 20) & 0x0f];
         *s++ = hex_table[(*input >> 16) & 0x0f];
         *s++ = hex_table[(*input >> 12) & 0x0f];
         *s++ = hex_table[(*input >> 8) & 0x0f];
         *s++ = hex_table[(*input >> 4) & 0x0f];
         *s++ = hex_table[*input & 0x0f];

         for (int j = 0; j < sizeof(unsigned long); j++)
         {
            if (isprint(*c))
            {
               *a++ = *c;
            }
            else
            {
               *a++ = '.';
            }
            c++;
         }
      }
      else
      {
         *s++ = '-';
         *s++ = '-';
         *s++ = '-';
         *s++ = '-';
         *s++ = '-';
         *s++ = '-';
         *s++ = '-';
         *s++ = '-';

         *a++ = ' ';
         *a++ = ' ';
         *a++ = ' ';
         *a++ = ' ';
      }

      *s++ = ' ';
      input++;

   }   // for each uword

   *a++ = '|';
   *a = 0;
   if (display_ascii)
   {
      strcpy(s, ascii);
   }
   else
   {
      s--;
      *s = 0;
   }

   return text;

}   // Hex_Word_32_String

/*****************************************************************************
*
*  TITLE:        Initialize
*
*****************************************************************************/

DOS_Return_Codes Initialize(

   int count,                   // count of commandline arguments
   char* argument[])            // list of commandline arguments

{
   char drive[MAXDRIVE];
   char dir[MAXDIR];
   char file[MAXFILE];
   char extension[MAXEXT];
   DOS_Return_Codes return_code = success;
   int index;


     //  Load name and path of running program.

   fnsplit(argument[0], drive, dir, file, extension);
   sprintf(run_file, "%s%s", file, extension);
   sprintf(run_path, "%s%s", drive, dir);

   index = 1;
   while ( (index < count) &&
           (return_code == success) )
   {
      return_code = Load_Argument(argument[index]);
      index++;
   }

   return return_code;

}   // Initialize


/*****************************************************************************
*
*  TITLE:        Load Argument
*
*****************************************************************************/

DOS_Return_Codes Load_Argument(

  char* argument)       // argument to load

{
   switch (*argument)
   {
      case '-':
      case '/':
         return Load_Option(++argument);

      case '@':
         return Load_Argument_File(++argument);

   }   // switch


   //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   //!!!!
   //!!!!  This is where you place your own normal arguments...
   //!!!!
   //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   if (file_name[0] != 0)
   {
      printf("extra parameter \"%s\" on line\n", argument);
      return commandline_error;
   }

   strncpy(file_name, argument, sizeof(file_name));
   TERMINATE(file_name);
   return success;

}   // Load_Argument

/*****************************************************************************
*
*  TITLE:        Load Argument File
*
*****************************************************************************/

DOS_Return_Codes Load_Argument_File(

   char* filename)      // name of file to load

{
   FILE* file;
   DOS_Return_Codes return_code = success;
   char line[0x100];
   char* first_non_blank;

   file = fopen(filename, "rt");

   if (file == NULL)
   {
      printf("could not open \"%s\" to read arguments\n", filename);
      return commandline_error;
   }

   while ( !feof(file) &&
           (return_code == success) )
   {
      fgets(line, sizeof(line), file);
      TERMINATE(line);

      for (first_non_blank = line;
           (*first_non_blank != 0) && isspace(*first_non_blank);
           first_non_blank++)
      {
      }

      if ((*first_non_blank != ';') &&
          (*first_non_blank != 0))
      {
         return_code = Load_Argument(first_non_blank);
      }
   }   // while

   fclose(file);

   return return_code;

}   // Load_Argument_File

/*****************************************************************************
*
*  TITLE:        Load Flag
*
*****************************************************************************/

void Load_Flag(

   char* flag,          // pointer to flag variable
   char** string)       // string to check for +/-

{

   if (**string == '-')
   {
      *flag = 0;
      (*string)++;
   }
   else if (**string == '+')
   {
      *flag = 1;
      (*string)++;
   }
   else
   {
      *flag = 1;
   }

}   // Load_Flag

/*****************************************************************************
*
*  TITLE:        Load Option
*
*****************************************************************************/

DOS_Return_Codes Load_Option(

   char* option)        // the option/switch string to load

{

   //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   //!!!!
   //!!!!  This is where you place your own switch arguments...
   //!!!!
   //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   while (*option)
   {
      switch (*option++)
      {
         case '?':
            return commandline_error;

         case 'a':
            Load_Flag(&display_ascii, &option);
            break;

         case 'c':
            if (sscanf(option, "%u", &columns) != 1)
            {
               printf("bad decimal number of columns \"%s\"\n", option);
               return commandline_error;
            }
            while (isdigit(*option)) option++;
            break;

         case 'n':
            if (sscanf(option, "%lx", &number_of_words) != 1)
            {
               printf("bad hex number of words \"%s\"\n", option);
               return commandline_error;
            }
            while (isxdigit(*option)) option++;
            break;

         case 'o':
            if (sscanf(option, "%lx", &file_offset) != 1)
            {
               printf("bad hex file offset \"%s\"\n", option);
               return commandline_error;
            }
            while (isxdigit(*option)) option++;
            break;

         default:
            option--;
            printf("unknown switch option \"-%s\"\n", option);
            return commandline_error;

      }   // switch
   }   // while

   return success;

}   // Load_Option

/*****************************************************************************
*
*  TITLE:        Main
*
*****************************************************************************/


DOS_Return_Codes main(

   int count,                   // count of commandline arguments
   char* argument_list[])       // the commandline arguments

{
   DOS_Return_Codes return_code = success;

   return_code = Initialize(count, argument_list);

   if (return_code != success)
   {
      Usage();
      return return_code; //----------------------------------> return!
   }

   //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   //!!!!
   //!!!!  You may wish to display help when no arguments are provided...
   //!!!!
   //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                        //!!!!
   if (file_name[0] == 0)               //!!!!
   {                                    //!!!!
      printf("no input file given\n");
      Usage();                          //!!!!
      return commandline_error;         //!!!!
   }                                    //!!!!

   //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   //!!!!
   //!!!!  Insert your application's code here...
   //!!!!
   //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   FILE* file = fopen(file_name, "rb");
   if (file == NULL)
   {
      printf("couldn't open \"%s\"\n", file_name);
      return could_not_open_file;
   }

   if (file_offset != 0UL)
   {
      fseek(file, file_offset, SEEK_SET);
   }

   static unsigned long int buffer[0x40];        // 16K buffer
   unsigned long words_read;
   unsigned long int words_read_so_far = 0;
   unsigned long words_to_read;

   while (!feof(file) &&
          (words_read_so_far < number_of_words))
   {
      words_to_read = columns;
      if (words_to_read > (number_of_words - words_read_so_far))
      {
         words_to_read = (number_of_words - words_read_so_far);
      }
      words_read = fread(buffer, 4, words_to_read, file);
      if (words_read == 0) break;
      printf("%05lx: %s\n",
          words_read_so_far,
          Hex_Word_32_String(buffer, words_read, columns));
      words_read_so_far += words_read;
   }
   fclose(file);

   return success;

}   // main


/*****************************************************************************
*
*  TITLE:        Usage
*
*****************************************************************************/

void Usage(void)

{
   //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   //!!!!
   //!!!!  This is where you place your own usage information...
   //!!!!
   //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   printf(

      "\n"
      "Usage: %s [options] <file>\n"
      "Where <file> is the file to dump.\n"
      "\n"
      "Options must begin with '-' or '/' (defaults in []):\n"
      "   @<file>    read more arguments and options from <file>\n"
      "   -a[+/-]    display ASCII on right or not [%s]\n"
      "   -c<cols>   decimal number of columns [%u]\n"
      "   -n<count>  hex number of words to read [%lx]\n"
      "   -o<offset> hex starting word offset into file [%lx]\n"
      "   -?         display usage information\n"

      ,minus_plus[DISPLAY_ASCII]
      ,columns
      ,(unsigned long) number_of_words
      ,(unsigned long) file_offset
      ,run_file

   );

}   // Usage


