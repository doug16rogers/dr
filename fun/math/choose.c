#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

void usage(int exit_code) __attribute__((noreturn));
void usage(int exit_code) {
    fprintf(stderr, "usage: choose n k        where n >= k >= 0\n");
    exit(exit_code);
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        usage(1);
    }
    uint64_t n;
    uint64_t k;
    if ((1 != sscanf(argv[1], "%" SCNu64, &n)) ||
        (1 != sscanf(argv[2], "%" SCNu64, &k)) ||
        (k > n)) {
        usage(2);
    }
    uint64_t c = 1;
    uint64_t prev_c;
    k = (k < (n - k)) ? k : (n - k);
    for (uint64_t i = 1; i <= k; i++) {
        prev_c = c;
        c *= n;
        if (c < prev_c) {
            fprintf(stderr, "choose: uint64 overflow (c=%" PRIu64 ", i=%" PRIu64 ").\n", prev_c, i);
            return 3;
        }
        c /= i;
        n--;
    }
    printf("%" PRIu64 "\n", c);
    return 0;
}
