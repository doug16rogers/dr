#include <stdio.h>
#include <errno.h>
#include <string.h>

#define PROGRAM   "nonul"

size_t nonul_to_stdout(FILE* f)
{
    unsigned char buf[0x1000] = {0};
    size_t bytes_out = 0;

    while (!feof(f))
    {
        ssize_t bytes_read = fread(buf, 1, sizeof(buf), f);
        if (bytes_read <= 0) break;
        ssize_t i_out = 0;
        ssize_t i_in = 0;

        for (i_in = 0; i_in < bytes_read; i_in++)
        {
            if (buf[i_in] != 0) buf[i_out++] = buf[i_in];
        }

        if (i_out > 0)
        {
            bytes_out += i_out;
            fwrite(buf, 1, i_out, stdout);
        }
    }

    return bytes_out;
}   /* nonul_to_stdout() */

int main(int argc, char* argv[])
{
    int return_code = 0;
    size_t bytes_out = 0;

    if (argc == 1)
    {
        bytes_out += nonul_to_stdout(stdin);
    }
    else
    {
        int i = 0;

        for (i = 1; i < argc; i++)
        {
            FILE* f = fopen(argv[i], "rb");

            if (NULL == f)
            {
                fprintf(stderr, "%s: could not open \"%s\": %s\n", PROGRAM, argv[i], strerror(errno));
                return_code = 1;
            }
            else
            {
                fprintf(stderr, "%s: reading \"%s\"...\n", PROGRAM, argv[i]);
                bytes_out += nonul_to_stdout(f);
                fclose(f);
            }
        }
    }

    fprintf(stderr, "%s: wrote %lu bytes.\n", PROGRAM, bytes_out);
    return return_code;
}   /* main() */
