/* Copyright (c) 2016-2019 Doug Rogers under the Zero Clause BSD License. */
/* You are free to do whatever you want with this software. See LICENSE.txt. */

#ifdef _MSC_BUILD
#include <Windows.h>
#endif

#include <assert.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _MSC_BUILD
#include <capstone.h>
#define strcasecmp   strcmpi
#define ssize_t      long
#define __attribute__(_x)
#pragma warning(disable: 4996)      // For fopen(), sscanf(), strcmpi().
#else
#include <capstone/capstone.h>
#endif

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
 * Default value for whether instructions are stored big-endian.
 */
#define kDefaultBigEndian 0

int g_big_endian = kDefaultBigEndian;

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
 * Default setting for the start address.
 */
#define kDefaultStartAddress 0

/**
 * Address to use for the first index (before the ':') when printing the
 * disassembly.
 */
uint64_t g_start_address = kDefaultStartAddress;

/**
 * Output file stream. By default use stdout.
 */
FILE* g_out_file = NULL;

/**
 * Default assembly comment sequence.
 */
#define kDefaultAsmComment ";"

/**
 * String to use for assembly line comment.
 */
const char *g_asm_comment = kDefaultAsmComment;

/**
 * Default flag for printing extra comments in the assembly listing.
 */
#define kDefaultPrintAsmComments 1

/**
 * Whether or not to print summary comments in the assembly output.
 */
int g_print_asm_comments = kDefaultPrintAsmComments;

/**
 * Default percentage of bytes used for printing instructions.
 */
#define kDefaultMinPercentBytesUsed 0

/**
 * Percentage of bytes used for printing instructions.
 */
unsigned int g_min_percent_bytes_used = kDefaultMinPercentBytesUsed;

/**
 * Default architecture as an index into the architecture list, kArchList[].
 */
#define kDefaultArchIndex 1

/**
 * Architecture to use when disassembling.
 */
size_t g_arch_index = kDefaultArchIndex;

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
 * My own constant to try running against all architectures.
 */
#define ARCH_ALL -6487

/**
 * List of supported architectures.
 */
const ArchEntry kArchList[] = {
    { ARCH_ALL, 0, "all", "Try all architectures." },

    { CS_ARCH_X86,   CS_MODE_64,     "x86-64",  "Intel x86 64-bit" },
    { CS_ARCH_X86,   CS_MODE_32,     "x86-32",  "Intel x86 32-bit" },
    { CS_ARCH_X86,   CS_MODE_16,     "x86-16",  "Intel x86 16-bit" },

    { CS_ARCH_ARM64, CS_MODE_ARM,    "arm-64",      "ARM64" },
    { CS_ARCH_ARM,   CS_MODE_ARM,    "arm-32",      "ARM32" },
    { CS_ARCH_ARM,   CS_MODE_THUMB,  "arm-thumb",   "ARM Thumb and Thumb-2" },
    { CS_ARCH_ARM,   CS_MODE_MCLASS, "arm-cortex",  "ARM M-Cortex" },
    { CS_ARCH_ARM,   CS_MODE_V8,     "arm-v8",      "ARM V8 encodings" },

    { CS_ARCH_MIPS,  CS_MODE_MIPS32, "mips-32",     "MIPS 32-bit" },
    { CS_ARCH_MIPS,  CS_MODE_MIPS64, "mips-64",     "MIPS 64-bit" },
    { CS_ARCH_MIPS,  CS_MODE_MIPS3,  "mips-iii",    "MIPS III" },
    { CS_ARCH_MIPS,  CS_MODE_MICRO,  "mips-micro",  "MicroMIPS" },

    { CS_ARCH_PPC,   CS_MODE_32,   "ppc",     "PowerPC 32-bit" },

    { CS_ARCH_SPARC, CS_MODE_32,    "sparc-32",   "SPARC 32-bit" },
    { CS_ARCH_SPARC, CS_MODE_V9,    "sparc-v9",   "SPARC V9" },

    { CS_ARCH_SYSZ,  0,  "sysz",    "SystemZ" },

    { CS_ARCH_XCORE, 0,  "xcore",   "X-Core" },
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
            "    -a:rch:itecture=arch        Architecture. [%s]\n"
            , kArchList[kDefaultArchIndex].name);
    for (size_t i = 0; i < kArchListCount; ++i) {
        fprintf(file, "           %-14s %s\n", kArchList[i].name, kArchList[i].description);
    }
    fprintf(file,
            "           %-14s %s\n", "all", "Try all defined architectures.");
    fprintf(file,
            "\n"
            "    -[no-]error-no-input        Whether to mark no input as an error. [%s-error-no-input]\n"
            , kDefaultErrorNoInput ? "" : "-no");
    fprintf(file,
            "    -b:ig:-endian               Instructions are stored big-endian. [-%sbig-endian]\n"
            , kDefaultBigEndian ? "" : "no-");
    fprintf(file,
            "    -s:tart:-address=<hex>      Address of first instruction. [%08" PRIX64 "]\n"
            , (uint64_t) kDefaultStartAddress);
    fprintf(file,
            "    -<u|min-used-percent>=NUM   Minimum percentage of bytes used to print instructions. [%u]\n"
            , kDefaultMinPercentBytesUsed);
    fprintf(file,
            "    -[no-]p:rint-comments       Whether to print comments. [%s-print-comments]\n"
            , kDefaultPrintAsmComments ? "" : "-no");
    fprintf(file,
            "    -c:omment=STR               Line comment string. [\"%s\"]\n"
            , kDefaultAsmComment);
    fprintf(file,
            "    -[no-]hex, -x               Convert input from hexadecimal ASCII. [%s-hex]\n"
            , kDefaultHexInput ? "" : "-no");
    fprintf(file,
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
    int i = 0;
    int end_of_options = 0;
    for (i = 1; i < argc; ++i) {
        char* arg = argv[i];
        const char* val;
        if (end_of_options || ('-' != *arg)) {
            argv[rval++] = arg;
        } else if (('-' == arg[1]) && (0 == arg[2])) {
            end_of_options = 1;
        } else if (IsOption(arg, NULL, "h:elp")) {
            Usage(stdout, 0);
        } else if (IsOption(arg, &val, "a:rch:itecture")) {
            size_t i = 0;
            if (NULL == val) {
                PrintUsageError("no architecture given in -arch=...");
            }
            for (i = 0; i < kArchListCount; ++i) {
                if (0 == strcasecmp(val, kArchList[i].name)) {
                    g_arch_index = i;
                    break;
                }
            }
            PrintVerbose("Using architecture \"%s\".", kArchList[g_arch_index].name);
        } else if (IsFlagOption(arg, &g_error_no_input, "error-no-input")) {
        } else if (IsFlagOption(arg, &g_hex_input, "hex")) {
        } else if (IsFlagOption(arg, &g_hex_input, "x")) {
        } else if (IsFlagOption(arg, &g_big_endian, "b:ig:-endian")) {
        } else if (IsOption(arg, &val, "s:tart:-address")) {
            if (NULL == val) {
                PrintUsageError("no start address given in -start-address=...");
            }
            if (sscanf(val, "%" PRIX64, &g_start_address) != 1) {
                PrintUsageError("invalid -start-address=%s", val);
            }
        } else if (IsOption(arg, &val, "min-used-percent") || IsOption(arg, &val, "u")) {
            if (NULL == val) {
                PrintUsageError("no minimum percent bytes used given in -min-used-percent=...");
            }
            if (sscanf(val, "%u", &g_min_percent_bytes_used) != 1) {
                PrintUsageError("invalid -min-used-percent=%s", val);
            }
        } else if (IsOption(arg, &val, "s:tart:-address")) {
            g_asm_comment = (NULL == val) ? "" : val;
        } else if (IsFlagOption(arg, &g_print_asm_comments, "p:rint-comment:s")) {
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
 * Print instruction @p ins to FILE stream @p file.
 */
void PrintInstruction(FILE* file, cs_insn* ins) {
    assert(NULL != file);
    assert(NULL != ins);
    fprintf(file, "%06" PRIX64 ":\t", (uint64_t) ins->address);
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
 * @return the number of instructions printed or @c 0 on error.
 */
size_t PrintDisassembly(FILE* file, uint8_t* buffer, size_t size, int arch_index, size_t *opt_bytes_consumed) {
    if (NULL == file) {
        file = stdout;
    }
    assert(NULL != buffer);
    assert(size > 0);
    csh handle;
    cs_insn* instruction = NULL;
    size_t instruction_count = 0;
    size_t local_bytes_consumed = 0;
    if (NULL == opt_bytes_consumed) {
        opt_bytes_consumed = &local_bytes_consumed;
    }
    *opt_bytes_consumed = 0;
    const ArchEntry* arch = &kArchList[arch_index];
    int mode = arch->mode | (g_big_endian ? CS_MODE_BIG_ENDIAN : 0);
    int cs_result = cs_open(arch->arch, mode, &handle);
    if (CS_ERR_OK != cs_result) {
        PrintError("cs_open() returned error %d", cs_result);
        return 0;
    }
    instruction_count = cs_disasm(handle, (const uint8_t*) buffer, size, g_start_address, 0, &instruction);
    PrintVerbose("cs_disasm() returned instruction_count=%u.", (unsigned) instruction_count);
    if (instruction_count > 0) {
        cs_insn *last_instruction = &instruction[instruction_count - 1];
        *opt_bytes_consumed = last_instruction->address + last_instruction->size - g_start_address;
    }
    size_t per10k = *opt_bytes_consumed * 10000 / size;
    if ((per10k / 100) < g_min_percent_bytes_used) {
        PrintVerbose("Skipping %s because bytes used %zu.%02zu%% < %u.00%% minimum.", arch->name,
                     per10k / 100, per10k % 100, g_min_percent_bytes_used);
        goto Exit;
    }
    if (g_print_asm_comments) {
        fprintf(g_out_file, "\n%s %s (%s)\n", g_asm_comment, arch->name, arch->description);
        fprintf(g_out_file, "%s %zu / %zu (%zu.%02zu%%) bytes disassembled for arch=%s (%s)\n", g_asm_comment,
                *opt_bytes_consumed, size, per10k / 100, per10k % 100, arch->name, arch->description);
    }
    size_t j = 0;
    for (j = 0; j < instruction_count; ++j) {
        PrintInstruction(file, &instruction[j]);
    }

Exit:
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
    int rval = 0;
    uint8_t* buffer = NULL;
    size_t buffer_bytes = 0;
    size_t disasm_bytes = 0;
    size_t longest_disasm_bytes = 0;
    int longest_arch = 1;
    size_t instructions = 0;
    g_program = NamePartOfPath(argv[0]);
    argc = ParseOptions(argc, argv);  /* Remove options; leave program name and arguments. */
    g_out_file = (NULL == g_out_file) ? stdout : g_out_file;
    buffer = calloc(1, kMaxBufferBytes + 1);
    if (NULL == buffer) {
        PrintError("could not allocate buffer");
        return 1;
    }
    if (1 == argc) {
        size_t bytes = fread(buffer, 1, kMaxBufferBytes, stdin);
        if (bytes == 0) {
            PrintError("could not read from stdin");
            rval = 2;
            goto Exit;
        }
        buffer_bytes = bytes;
    } else {
        size_t i = 1;
        for (i = 1; (i < (size_t) argc) && (buffer_bytes < kMaxBufferBytes); ++i) {
            FILE* file = fopen(argv[i], "rb");
            size_t bytes = 0;
            if (NULL == file) {
                PrintError("could not open \"%s\"", argv[i]);
                rval = 2;
                goto Exit;
            }
            bytes = fread(&buffer[buffer_bytes], 1, kMaxBufferBytes - buffer_bytes, file);
            if (bytes == 0) {
                PrintError("could not read from \"%s\"", argv[i]);
                fclose(file);
                rval = 2;
                goto Exit;
            }
            buffer_bytes += bytes;
            if (buffer_bytes >= kMaxBufferBytes) {
                break;
            }
        }
        if (i < (size_t) argc) {
            PrintError("buffer overflow reading \"%s\"; size limit is %d bytes", argv[i], (int) kMaxBufferBytes);
            rval = 3;
            goto Exit;
        }
    }

    PrintVerbose("Read %u bytes of input.", (unsigned) buffer_bytes);

    if ((buffer_bytes > 0) && g_hex_input) {
        buffer_bytes = UnhexBufferInPlace((char*) buffer, buffer_bytes);
        if (0 == buffer_bytes) {
            PrintError("no buffer data left after unhexing");
            rval = 4;
            goto Exit;
        }
        PrintVerbose("After unhexing, %u bytes of input.", (unsigned) buffer_bytes);
    }

    if (buffer_bytes > 0) {
        PrintVerbose("Disassembling %u bytes.", (unsigned) buffer_bytes);

        if (g_verbose && (buffer_bytes > 0)) {
            fprintf(stderr, "%s:   Data: %02X", g_program, buffer[0]);
            if (buffer_bytes >  1) fprintf(stderr, " %02X", buffer[1]);
            if (buffer_bytes >  2) fprintf(stderr, " %02X", buffer[2]);
            if (buffer_bytes >  3) fprintf(stderr, " %02X", buffer[3]);
            if (buffer_bytes >  4) fprintf(stderr, " %02X", buffer[4]);
            if (buffer_bytes >  5) fprintf(stderr, " %02X", buffer[5]);
            if (buffer_bytes >  6) fprintf(stderr, " %02X", buffer[6]);
            if (buffer_bytes >  7) fprintf(stderr, " %02X", buffer[7]);
            if (buffer_bytes > 16) fprintf(stderr, " ...");
            if (buffer_bytes > 15) fprintf(stderr, " %02X", buffer[buffer_bytes - 8]);
            if (buffer_bytes > 14) fprintf(stderr, " %02X", buffer[buffer_bytes - 7]);
            if (buffer_bytes > 13) fprintf(stderr, " %02X", buffer[buffer_bytes - 6]);
            if (buffer_bytes > 12) fprintf(stderr, " %02X", buffer[buffer_bytes - 5]);
            if (buffer_bytes > 11) fprintf(stderr, " %02X", buffer[buffer_bytes - 4]);
            if (buffer_bytes > 10) fprintf(stderr, " %02X", buffer[buffer_bytes - 3]);
            if (buffer_bytes >  9) fprintf(stderr, " %02X", buffer[buffer_bytes - 2]);
            if (buffer_bytes >  8) fprintf(stderr, " %02X", buffer[buffer_bytes - 1]);
            fprintf(stderr, "\n");
        }

        if ((const cs_arch) ARCH_ALL == kArchList[g_arch_index].arch) {
            int arch = 0;
            for (arch = 1; arch < kArchListCount; arch++) {
                instructions = PrintDisassembly(g_out_file, buffer, buffer_bytes, arch, &disasm_bytes);
                if(disasm_bytes > longest_disasm_bytes) {
                    longest_disasm_bytes = disasm_bytes;
                    longest_arch = arch;
                }
                if (0 == disasm_bytes) {
                    rval = 5;
                }
            }
            size_t per10k = longest_disasm_bytes * 10000 / buffer_bytes;
            fprintf(g_out_file, "\n%s Longest disassembly: %zu / %zu (%zu.%02zu%%) bytes disassembled for arch=%s\n",
                    g_asm_comment, longest_disasm_bytes, buffer_bytes, per10k / 100, per10k % 100,
                    kArchList[longest_arch].name);
        } else {
            instructions = PrintDisassembly(g_out_file, buffer, buffer_bytes, g_arch_index, &disasm_bytes);
            if (0 == instructions) {
                PrintError("%s disassembly failed for %zu bytes", kArchList[g_arch_index].name, buffer_bytes);
                rval = 5;
            }
        }
    } else if (g_error_no_input) {
        PrintError("no input bytes");
        rval = 6;
    }

Exit:
    free(buffer);
    if (stdout != g_out_file) {
        fclose(g_out_file);
    }
    return rval;
}   /* main() */

