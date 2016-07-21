/* Copyright (c) 2016 FireEye Incorporated. All rights reserved. */

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <capstone/capstone.h>

/**
 * Default name of this program.
 */
#define kProgram "csdisasm"

/**
 * Name of this program; this may be modified by argv[0] in main().
 */
const char* g_program = kProgram;

/**
 * Default value for verbose setting.
 */
#define kDefaultVerbose 0

/**
 * Whether or not to emit verbose messages.
 */
int g_verbose = 0;

/**
 * Default value for whether no input (zero bytes) is an error.
 */
#define kDefaultErrorNoInput 0

/**
 * Whether or not to emit an error message when no input data bytes are seen.
 */
int g_error_no_input = kDefaultErrorNoInput;

/**
 * Default setting for hexadecimal (ASCII) input.
 */
#define kDefaultHexInput 0

/**
 * Architecture to use when disassembling.
 */
int g_hex_input = kDefaultHexInput;

/**
 * Output file stream. By default use stdout.
 */
FILE* g_out_file = NULL;

/**
 * Default architecture as an index into the architecture list, kArchList[].
 */
#define kDefaultArch 0

/**
 * Architecture to use when disassembling.
 */
size_t g_arch = kDefaultArch;

/**
 * Architecture descriptions.
 */
typedef struct _ArchEntry {
    cs_arch arch;
    cs_mode mode;
    const char* name;
    const char* description;
} ArchEntry;

/**
 * List of supported architectures.
 */
const ArchEntry kArchList[] = {
    { CS_ARCH_X86,   CS_MODE_32,  "x86",     "Intel x86 32-bit" },
    { CS_ARCH_X86,   CS_MODE_64,  "x64",     "Intel x86 64-bit" },
    { CS_ARCH_ARM,   CS_MODE_ARM, "arm",     "ARM32, Thumb and Thumb-2" },
    { CS_ARCH_ARM64, CS_MODE_ARM, "arm64",   "ARM64" },
};

/**
 * Number of supported architectures.
 */
#define kArchListCount  (sizeof(kArchList) / sizeof(kArchList[0]))

/* ------------------------------------------------------------------------- */
/**
 * Prints usage information to @a file.
 *
 * Note: This function does not return.
 *
 * @param file - FILE stream to which to write the usage information.
 *
 * @param exit_code - value to pass to exit() when ending program.
 */
void Usage(FILE* file, int exit_code) __attribute__((noreturn));
void Usage(FILE* file, int exit_code) {
    fprintf(file,
            "\n"
            "USAGE\n"
            "    %s [options] args...\n"
            , g_program);
    fprintf(file,
            "\n"
            "DESCRIPTION\n"
            "    %s uses the Capstone disassembler library to disassemble binary or.\n"
            "    hexadecimal input.\n"
            , g_program);
    fprintf(file,
            "\n"
            "OPTIONS\n"
            "    Options may begin with '-' or '--'. A ':' indicates where options may be\n"
            "    abbreviated\n");
    fprintf(file,
            "\n"
            "    -h:elp                      Show this usage information.\n");
    fprintf(file,
            "\n"
            "    -a:rch=arch                 Architecture. [%s]\n"
            , kArchList[kDefaultArch].name);
    for (size_t i = 0; i < kArchListCount; ++i) {
        fprintf(file, "           %-8s %s\n", kArchList[i].name, kArchList[i].description);
    }
    fprintf(file,
            "\n"
            "    -[no-]error-no-input         Whether to mark no input as an error. [%s-error-no-input]\n"
            , kDefaultErrorNoInput ? "" : "-no");
    fprintf(file,
            "\n"
            "    -[no-]hex, -x               Convert input from hexadecimal ASCII. [%s-hex]\n"
            , kDefaultHexInput ? "" : "-no");
    fprintf(file,
            "\n"
            "    -[no-]v:erbose              Print verbose (debug) messages. [%s-verbose]\n"
            , kDefaultVerbose ? "" : "-no");
    exit(exit_code);
}   /* Usage() */

/* ------------------------------------------------------------------------- */
/**
 * Print an error message to stderr then exit the program with exit code 1.
 */
void PrintUsageError(const char* format, ...) {
    char text[0x0100] = "";
    va_list va;
    va_start(va, format);
    vsnprintf(text, sizeof(text), format, va);
    va_end(va);
    fprintf(stderr, "%s: %s\n", g_program, text);
    fprintf(stderr, "%s: Use '%s --help' for usage information.\n", g_program, g_program);
    exit(1);
}   /* PrintUsageError() */

/* ------------------------------------------------------------------------- */
/**
 * Print an error message to stderr.
 */
void PrintError(const char* format, ...) {
    char text[0x0100] = "";
    va_list va;
    va_start(va, format);
    vsnprintf(text, sizeof(text), format, va);
    va_end(va);
    fprintf(stderr, "%s: %s\n", g_program, text);
}   /* PrintError() */

/* ------------------------------------------------------------------------- */
/**
 * Print a message to stdout if g_verbose is non-zero.
 */
void PrintVerbose(const char* format, ...) {
    if (g_verbose) {
        char text[0x0100] = "";
        va_list va;
        va_start(va, format);
        vsnprintf(text, sizeof(text), format, va);
        va_end(va);
        fprintf(stdout, "%s: %s\n", g_program, text);
    }
}   /* PrintVerbose() */

/* ------------------------------------------------------------------------- */
/**
 * Find and return a pointer to the file name portion of @a path.
 *
 * @param path - a path whose name is desired. Typically this is argv[0] from
 * main().
 *
 * @return a pointer the first character after the last directory delimiter
 * (forward or back slash) in @a path, or @a path if none is found.
 */
const char* NamePartOfPath(const char* path) {
    const char* rval = path;
    if (NULL != path) {
        for (; 0 != *path; ++path) {
            if ((('/' == path[0]) || ('\\' == path[0])) &&
                !((0 == path[1]) || ('/' == path[1]) || ('\\' == path[1]))) {
                rval = &path[1];
            }
        }
    }
    return rval;
}   /* NamePartOfPath() */

/* ------------------------------------------------------------------------- */
/**
 * Look for an option of the form "[-[-]]option[=value]".
 *
 * If @a input contains '=' then non-null @a *value_ptr is set to point
 * to the character after '=' (or is set to NULL if there is no argument).
 *
 * @a descriptor may contain ':' characters which indicate abbreviation
 * points for the option. For example, "o:pt:ion" will match "-o",
 * "-o=value", "-opt", "-opt=value", "-option" and "-option=value".
 *
 * @return 1 if @a input matches @a descriptor, 0 otherwise.
 */
int IsOption(const char* input, const char** value_ptr, const char* descriptor) {
    int rval = 0;
    int finished = 0;
    assert(NULL != input);
    assert(NULL != descriptor);
    if ('-' == *input) {
        ++input;
        if ('-' == *input) {
            ++input;
        }
    } else {
        finished = 1;
    }
    while (!finished) {
        if ((0 == *input) || ('=' == *input)) {
            finished = 1;
            rval = (0 == *descriptor) || (':' == *descriptor);
        } else if ((0 == *descriptor) || ((':' != *descriptor) && (*input != *descriptor))) {
            finished = 1;
        } else {
            if (':' != *descriptor) {
                ++input;
            }
            ++descriptor;
        }
    }
    if (NULL != value_ptr) {
        *value_ptr = (rval && ('=' == *input)) ? (input + 1) : NULL;
    }
    return rval;
}   /* IsOption() */

/* ------------------------------------------------------------------------- */
/**
 * Look for flag option of the form "-[-][no-]option".
 *
 * @a descriptor may contain ':' characters which indicate abbreviation
 * points for the option. See IsOption() for more information.
 *
 * If @a input matches the descriptor then the value of @a *flag_value_ptr (if
 * not NULL) will be set to 1. If @a input matches the descriptor with "no-"
 * prefixed then @a *flag_value_ptr will be set to 0. If @a input does not
 * match @a descriptor, @a *flag_value_ptr is not modified.
 *
 * @return 1 if @a input matches @a descriptor with or without a "no-" prefix,
 * 0 otherwise.
 */
int IsFlagOption(const char* input, int* flag_value_ptr, const char* descriptor) {
    int flag_value = 1;
    int rval = 0;
    assert(NULL != input);
    assert(NULL != descriptor);
    if ('-' == *input) {
        rval = IsOption(input, NULL, descriptor);
        if (!rval) {
            flag_value = 0;
            const int k = ('-' == input[1]) ? 1 : 0;
            if (('n' == input[k+1]) && ('o' == input[k+2]) && ('-' == input[k+3])) {
                rval = IsOption(&input[k+3], NULL, descriptor);
            }
        }
    }
    if (rval && (NULL != flag_value_ptr)) {
        *flag_value_ptr = flag_value;
    }
    return rval;
}   /* IsFlagOption() */

/* ------------------------------------------------------------------------- */
/**
 * Parse options from the command line, removing them from @a argv[].
 *
 * Note that on error this function does not return.
 *
 * If an argument starts with '-' then it will be treated as an option. See
 * the OPTIONS section of Usage() for the list of options available to this
 * program.
 *
 * If "--" is encountered as an argument, no further option processing will
 * occur, even if a later argument begins with '-'.
 *
 * @param argc - number of argument pointers in array @a argv[].
 *
 * @param argv - array of argument string pointers to parse.
 *
 * @return the number of (non-option) command line arguments that remain in
 * @a argv[] after option processing.
 */
int ParseOptions(int argc, char* argv[]) {
    int rval = 1;       /* Skip program name. */
    char** non_option_argument_list = argv;
    int i = 0;
    int end_of_options = 0;
    for (i = 1; i < argc; ++i) {
        char* arg = argv[i];
        const char* val;
        if (end_of_options || ('-' != *arg)) {
            non_option_argument_list[rval++] = arg;
        } else if (('-' == arg[1]) && (0 == arg[2])) {
            end_of_options = 1;
        } else if (IsOption(arg, NULL, "h:elp")) {
            Usage(stdout, 0);
        } else if (IsOption(arg, &val, "a:rch")) {
            size_t i = 0;
            if (NULL == val) {
                PrintUsageError("no architecture given in -arch=...");
            }
            for (size_t i = 0; i < kArchListCount; ++i) {
                if (0 == strcasecmp(val, kArchList[i].name)) {
                    g_arch = i;
                    break;
                }
            }
            if (kArchListCount == i) {
                PrintUsageError("unknown architecture \"%s\"", val);
            }
            PrintVerbose("Using architecture \"%s\".", kArchList[g_arch].name);
        } else if (IsFlagOption(arg, &g_error_no_input, "error-no-input")) {
        } else if (IsFlagOption(arg, &g_hex_input, "hex")) {
        } else if (IsFlagOption(arg, &g_hex_input, "x")) {
        } else if (IsFlagOption(arg, &g_verbose, "v:erbose")) {
        } else {
            PrintUsageError("invalid option \"%s\"", arg);
        }
    }
    return rval;
}   /* ParseOptions() */

/* ------------------------------------------------------------------------- */
/**
 * @return the digit value of a hexadecimal character, or a value greater
 * than 0x0F if @a c is not a hexadecimal character.
 */
uint8_t HexDigit(char c) {
    if (('0' <= c) && (c <= '9')) {
        return c - '0';
    }
    if (('a' <= c) && (c <= 'f')) {
        return c - 'a' + 10;
    }
    if (('A' <= c) && (c <= 'F')) {
        return c - 'A' + 10;
    }
    return 0x10;
}   /* HexDigit() */

/* ------------------------------------------------------------------------- */
/**
 * Convert from hexadecimal ASCII to raw binary.
 *
 * @return the number of bytes converted, 0 on error.
 */
size_t UnhexBufferInPlace(char* data, size_t size) {
    size_t bytes_out = 0;
    uint8_t* out = (uint8_t*) data;
    int have_one = 0;
    for (size_t i = 0; i < size; ++i) {
        const uint8_t xval = HexDigit(data[i]);
        if (xval <= 0x0F) {
            if (have_one) {
                *out = (*out << 4) + xval;
                bytes_out++;
                out++;
                have_one = 0;
            } else {
                *out = xval;
                have_one = 1;
            }
        } else if (have_one) {
            bytes_out++;
            out++;
            have_one = 0;
        }
    }
    if (have_one) {
        bytes_out++;
    }
    return bytes_out;
}   /* UnhexBufferInPlace() */

/* ------------------------------------------------------------------------- */
/**
 * 
 */
void PrintInstruction(FILE* file, cs_insn* ins) {
    assert(NULL != file);
    assert(NULL != ins);
    fprintf(file, "%06X:\t", (unsigned) ins->address);
    for (size_t i = 0; i < 16; ++i) {
        if (i < ins->size) {
            fprintf(file, "%02X", ins->bytes[i]);
        } else if (i < 7) {
            fprintf(file, "  ");
        }
    }
    fprintf(file, "\t%s\t%s\n", ins->mnemonic, ins->op_str);
}   /* PrintInstruction() */

/* ------------------------------------------------------------------------- */
/**
 * Print the disassembled data to @a file stream.
 *
 * @return the number of instructions printed, 0 on error.
 */
size_t PrintDisassembly(FILE* file, uint8_t* buffer, size_t size) {
    if (NULL == file) {
        file = stdout;
    }
    assert(NULL != buffer);
    assert(size > 0);
    csh handle;
    cs_insn* instruction = NULL;
    size_t instruction_count = 0;
    const ArchEntry* arch = &kArchList[g_arch];
    int cs_result = cs_open(arch->arch, arch->mode, &handle);
    if (CS_ERR_OK != cs_result) {
        PrintError("cs_open() returned error %d", cs_result);
        return 0;
    }
    instruction_count = cs_disasm(handle, (const uint8_t*) buffer, size, 0, 0, &instruction);
    PrintVerbose("cs_disasm() returned instruction_count=%u.", (unsigned) instruction_count);
    if (0 == instruction_count) {
        cs_close(&handle);
        PrintError("disassembly failed for %d bytes", (int) size);
        return 0;
    }
    size_t j = 0;
    for (j = 0; j < instruction_count; ++j) {
        PrintInstruction(file, &instruction[j]);
    }
    cs_free(instruction, instruction_count);
    cs_close(&handle);
    return instruction_count;
}   /* PrintDisassembly() */

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
#define kMaxBufferBytes 0x00100000
    uint8_t* buffer = NULL;
    size_t buffer_bytes = 0;
    g_program = NamePartOfPath(argv[0]);
    argc = ParseOptions(argc, argv);  /* Remove options; leave program name and arguments. */
    buffer = malloc(kMaxBufferBytes + 1);
    if (NULL == buffer) {
        PrintError("could not allocate buffer");
        return 1;
    }
    if (1 == argc) {
        ssize_t bytes = fread(buffer, 1, kMaxBufferBytes, stdin);
        if (bytes < 0) {
            PrintError("could not read from stdin");
            return 2;
        }
        buffer_bytes = bytes;
    } else {
        size_t i = 1;
        for (i = 1; (i < argc) && (buffer_bytes < kMaxBufferBytes); ++i) {
            FILE* file = fopen(argv[i], "rb");
            ssize_t bytes = 0;
            if (NULL == file) {
                PrintError("could not open \"%s\"", argv[i]);
                return 2;
            }
            bytes = fread(&buffer[buffer_bytes], 1, kMaxBufferBytes - buffer_bytes, file);
            if (bytes < 0) {
                PrintError("could not read from \"%s\"", argv[i]);
                fclose(file);
                return 2;
            }
            buffer_bytes += bytes;
            if (buffer_bytes >= kMaxBufferBytes) {
                break;
            }
        }
        if (i < argc) {
            PrintError("buffer overflow reading \"%s\"; size limit is %d bytes", argv[i], (int) kMaxBufferBytes);
            return 3;
        }
    }
    PrintVerbose("Read %u bytes of input.", (unsigned) buffer_bytes);
    if ((buffer_bytes > 0) && g_hex_input) {
        /* if (g_verbose) { */
        /*     buffer[buffer_bytes] = 0; */
        /*     fprintf(stderr, "%s: Hex ASCII input: %s\n", g_program, (const char*) buffer); */
        /* } */
        buffer_bytes = UnhexBufferInPlace((char*) buffer, buffer_bytes);
        if (0 == buffer_bytes) {
            PrintError("no buffer data left after unhexing");
            return 4;
        }
        PrintVerbose("After unhexing, %u bytes of input.", (unsigned) buffer_bytes);
    }
    if (buffer_bytes > 0) {
        PrintVerbose("Disassembling %u bytes.", (unsigned) buffer_bytes);
        if (g_verbose && (buffer_bytes > 0)) {
            fprintf(stderr, "%s:   Data: %02X", g_program, buffer[0]);
            if (buffer_bytes > 1) fprintf(stderr, " %02X", buffer[1]);
            if (buffer_bytes > 2) fprintf(stderr, " %02X", buffer[2]);
            if (buffer_bytes > 3) fprintf(stderr, " %02X", buffer[3]);
            if (buffer_bytes > 4) fprintf(stderr, " %02X", buffer[4]);
            if (buffer_bytes > 5) fprintf(stderr, " %02X", buffer[5]);
            if (buffer_bytes > 6) fprintf(stderr, " %02X", buffer[6]);
            if (buffer_bytes > 7) fprintf(stderr, " %02X", buffer[7]);
            if (buffer_bytes > 8) fprintf(stderr, "...");
            fprintf(stderr, "\n");
        }
        if (!PrintDisassembly(g_out_file, buffer, buffer_bytes)) {
            return 5;   /* error message already printed */
        }
    } else if (g_error_no_input) {
        PrintError("no input bytes");
        return 6;
    }
    free(buffer);
    if (stdout != g_out_file) {
        fclose(g_out_file);
    }
    return 0;
}   /* main() */

