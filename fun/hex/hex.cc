// Copyright (c) 2003-2019 Doug Rogers under the Zero Clause BSD License.
// You are free to do whatever you want with this software. See LICENSE.txt.

#include <ctype.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "typedefs.h"

#define ADVANCE_TO_END(s)       while (*s) s++
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
    out_of_memory,       // couldn't allocate buffer

} RETURN_CODES;

//===========================================================================
//
//  Global Data...
//
//===========================================================================

char* run_file;         // name for executing program

#define BYTES_PER_UNIT  1
#define DISPLAY_ASCII   1
#define COLUMNS         16
#define FILE_OFFSET     0UL

#if defined(_DOS) || defined(_Windows) ||       \
    defined(__BORLANDC__) || defined(__MSC__)
#define INTEL_ENDIAN    1
#else
#define INTEL_ENDIAN    0
#endif

#define NUMBER_OF_UNITS 0xFFFFFFFFUL

char file_name[0x200] = "";

#define DEFAULT_LOWER 0

unsigned int  bytes_per_unit = BYTES_PER_UNIT;
unsigned int  columns = COLUMNS;
int           display_ascii = DISPLAY_ASCII;
unsigned long file_offset = FILE_OFFSET;
int           intel_endian = INTEL_ENDIAN;
int           g_lower = DEFAULT_LOWER;
unsigned int  number_of_units = NUMBER_OF_UNITS;

const char* endian_name[] = { "m", "i" };   // Motorola/Intel byte order
const char* minus_plus[] = { "-", "+" };    // text for flags


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

RETURN_CODES Initialize(

    int count,                   // count of commandline arguments
    char* argument[]);           // list of commandline arguments

/*****************************************************************************
 *
 *  TITLE:        Load Argument
 *
 *  DESCRIPTION:  The function "Load_Argument" loads a single
 *                argument from the commandline.
 *                If the argument begins with '-',
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

RETURN_CODES Load_Argument(

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

RETURN_CODES Load_Argument_File(

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

RETURN_CODES Load_Option(

    char* option);       // the option/switch string to load

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


static char lower_hex_table[17] = "0123456789abcdef";
static char upper_hex_table[17] = "0123456789ABCDEF";
static char* hex_table = upper_hex_table;


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
    void*        data,
    unsigned int length,
    unsigned int width)
{
    static char text[0x300];
    static char ascii[0x100];
    char* s = text;
    char* a = ascii;
    char* c = (char*) data;
    UINT8* input = (UINT8*) data;
    UINT8* unit = (UINT8*) data;

    unsigned int i;
    unsigned int j;

    s = text;
    a = ascii;

    *a++ = '|';

    if (intel_endian)
    {
        unit += (bytes_per_unit - 1);     // point to end of list
    }

    for (i = 0; i < width; i++, unit += bytes_per_unit)
    {
        input = unit;
        if (i < length)
        {
            for (j = 0; (j < bytes_per_unit); j++)
            {
                *s++ = hex_table[(*input >> 4) & 0x0f];
                *s++ = hex_table[*input & 0x0f];

                if (((*c) < ' ') || (*c > '~'))
                {
                    *a++ = ' ';
                }
                else
                {
                    *a++ = *c;
                }

                c++;

                if (intel_endian)
                {
                    input--;
                }
                else
                {
                    input++;
                }
            }
        }
        else
        {
            for (j = 0; (j < bytes_per_unit); j++)
            {
                *s++ = '-';
                *s++ = '-';
                *a++ = ' ';
            }
        }

        *s++ = ' ';
        if ( ((i & 3) == 3) &&
             (i < (width - 1)) )
        {
            *s++ = ' ';
        }
        input++;

    }   // for each unit

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

// ----------------------------------------------------------------------------
/**
 * Skip @a offset bytes in @a file.
 *
 * @param name - description
 * @param name - description
 *
 * @return
 */
void Skip_Bytes(FILE* file, unsigned long offset) {
    if (0 != fseek(file, offset, SEEK_SET)) {
        unsigned long bytes_left = offset;
        while ((bytes_left > 0) && !feof(file) && !ferror(file)) {
            char buffer[0x100];
            size_t bytes_to_read = (bytes_left > sizeof(buffer)) ? sizeof(buffer) : bytes_left;
            size_t bytes_read = fread(buffer, 1, bytes_to_read, file);
            bytes_left -= bytes_read;
        }
    }
}   // Skip_Bytes()

// ----------------------------------------------------------------------------
/**
 * Initialize the program by loading the command line arguments.
 */
RETURN_CODES Initialize(int count, char* argument[]) {
    RETURN_CODES return_code = success;
    int index;
    run_file = argument[0];
    index = 1;
    while ((index < count) && (return_code == success)) {
        return_code = Load_Argument(argument[index]);
        index++;
    }
    return return_code;
}   // Initialize()

// ----------------------------------------------------------------------------
/**
 * Load a single argument - either a command line argument or an option.
 */
RETURN_CODES Load_Argument(char* argument) {
    switch (*argument) {
    case '-':
        if (0 != argument[1]) {
            return Load_Option(++argument);
        }
        break;
    case '@':
        return Load_Argument_File(++argument);
    }   // switch

    if (file_name[0] != 0) {
        printf("extra parameter \"%s\" on line\n", argument);
        return commandline_error;
    }
    strncpy(file_name, argument, sizeof(file_name));
    TERMINATE(file_name);
    return success;
}   // Load_Argument()

// ----------------------------------------------------------------------------
/**
 * Load command line arguments from the file given by @a filename.
 */
RETURN_CODES Load_Argument_File(char* filename) {
    FILE* file;
    RETURN_CODES return_code = success;
    char line[0x100];
    int line_length;
    char* first_non_blank;
    file = fopen(filename, "rt");
    if (file == NULL) {
        printf("could not open \"%s\" to read arguments\n", filename);
        return commandline_error;
    }
    while (!feof(file) && (return_code == success)) {
        if (fgets(line, sizeof(line), file) == NULL) {
            break;
        }
        TERMINATE(line);

        line_length = strlen(line);
        if (line_length == 0)
            continue;
        if (line[line_length - 1] == '\n') {
            line_length--;
            if (line_length == 0)
                continue;
            line[line_length] = 0;
        }
        for (first_non_blank = line;
             (*first_non_blank != 0) && isspace(*first_non_blank);
             first_non_blank++) {
            ;
        }
        if ((*first_non_blank != ';') &&
            (*first_non_blank != 0)) {
            return_code = Load_Argument(first_non_blank);
        }
    }   // while
    fclose(file);
    return return_code;
}   // Load_Argument_File()

// ----------------------------------------------------------------------------
/**
 * Load a boolean @a flag from the given @a *string, incrementing @a *string
 * if it is valid.
 */
void Load_Flag(int* flag, char** string) {
    if (**string == '-') {
        *flag = false;
        (*string)++;
    } else if (**string == '+') {
        *flag = true;
        (*string)++;
    } else {
        *flag = true;
    }
}   // Load_Flag()

// ----------------------------------------------------------------------------
/**
 * Load an option - something that has already had the '-' stripped from it.
 */
RETURN_CODES Load_Option(char* option) {
    while (*option) {
        switch (*option++) {
        case '?':
            return commandline_error;
        case '1':
            bytes_per_unit = 1;
            columns = 16;
            display_ascii = true;
            break;
        case '2':
            bytes_per_unit = 2;
            columns = 8;
            display_ascii = true;
            break;
        case '4':
            bytes_per_unit = 4;
            columns = 8;
            display_ascii = false;
            break;
        case 'a':
            Load_Flag(&display_ascii, &option);
            break;
        case 'b':
            if ((sscanf(option, "%u", &bytes_per_unit) != 1) ||
                (bytes_per_unit < 1) ||
                (bytes_per_unit > 32)) {
                printf("bad decimal number of columns \"%s\"\n", option);
                return commandline_error;
            }
            while (isdigit(*option)) option++;
            break;
        case 'c':
            if ((sscanf(option, "%u", &columns) != 1) ||
                (columns < 1) ||
                (columns > 64)) {
                printf("bad decimal number of columns \"%s\"\n", option);
                return commandline_error;
            }
            while (isdigit(*option)) option++;
            break;
        case 'i':
            intel_endian = true;
            break;
        case 'm':
            intel_endian = false;
            break;
        case 'l':
            g_lower = 1;
            hex_table = lower_hex_table;
            break;
        case 'u':
            g_lower = 0;
            hex_table = upper_hex_table;
            break;
        case 'n':
            if (sscanf(option, "%x", &number_of_units) != 1) {
                printf("bad hex number of words \"%s\"\n", option);
                return commandline_error;
            }
            while (isxdigit(*option)) option++;
            break;
        case 'o':
            if (sscanf(option, "%lx", &file_offset) != 1) {
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

// ----------------------------------------------------------------------------
/**
 * Main program.
 */
int main(int count, char* argument_list[]) {
    RETURN_CODES return_code = success;
    return_code = Initialize(count, argument_list);
    if (return_code != success) {
        Usage();
        return return_code; //----------------------------------> return!
    }
    FILE* file = NULL;
    if ((file_name[0] == 0) || (0 == strcmp(file_name, "-"))) {
        file = stdin;
        strncpy(file_name, "<stdin>", sizeof(file_name));
    } else {
        file = fopen(file_name, "rb");
        if (file == NULL) {
            printf("couldn't open \"%s\"\n", file_name);
            return could_not_open_file;
        }
    }

    if (file_offset != 0UL) {
        Skip_Bytes(file, file_offset * (UINT32) bytes_per_unit);
    }
    UINT8* buffer = (UINT8*) malloc(bytes_per_unit * columns);
    if (buffer == NULL) {
        printf("no memory\n");
        return out_of_memory;
    }
    UINT32 units_read;
    UINT32 units_read_so_far = 0UL;
    UINT32 units_to_read;
    while (!feof(file) &&
           (units_read_so_far < number_of_units)) {
        units_to_read = columns;
        if (units_to_read > (number_of_units - units_read_so_far)) {
            units_to_read = (number_of_units - units_read_so_far);
        }
        units_read = fread(buffer, bytes_per_unit, units_to_read, file);
        if (units_read == 0) break;
        printf("%05lX: %s\n",
               units_read_so_far,
               Hex_Word_32_String(buffer, units_read, columns));
        units_read_so_far += units_read;
    }
    fclose(file);
    free(buffer);

    return success;
}   // main


/*****************************************************************************
 *
 *  TITLE:        Usage
 *
 *****************************************************************************/

void Usage(void) {
    printf(
        "\n"
        "Usage: %s [options] [file]\n"
        "\n"
        "Where <file> is the file to dump. If no file is given, stdin is used.\n"
        "\n"
        "Options must begin with '-' (defaults in []):\n"
        "   @<file>    read more arguments and options from <file>\n"
        "   -?         display usage information\n"
        "   -1         one byte per unit; with ascii (-a+ -b1 -c16)\n"
        "   -2         two bytes per unit; with ascii (-a+ -b2 -c8)\n"
        "   -4         four bytes per unit; no ascii (-a- -b4 -c8)\n"
        "   -a[+/-]    display ASCII on right or not [%s]\n"
        "   -b<bytes>  decimal bytes per unit [%u]\n"
        "   -c<cols>   decimal number of columns [%u]\n"
        "   -i/m       use Intel (LSB first) or Motorola (MSB first) [%s]\n"
        "   -l/u       use lower or upper case [%s]\n"
        "   -n<count>  hex number of words to read [%lx]\n"
        "   -o<offset> hex starting unit offset into file [%lx]\n"
        "Note: ASCII text is in ascending order, regardless of -i/m setting.\n"
        , run_file
        , minus_plus[DISPLAY_ASCII]
        , BYTES_PER_UNIT
        , COLUMNS
        , endian_name[INTEL_ENDIAN]
        , DEFAULT_LOWER ? "lower" : "upper"
        , (unsigned long) NUMBER_OF_UNITS
        , (unsigned long) FILE_OFFSET
           );
}   // Usage


