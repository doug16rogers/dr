/* Copyright (c) 2016-2019 Doug Rogers under the Zero Clause BSD License. */
/* You are free to do whatever you want with this software. See LICENSE.txt. */

#include <stdint.h>
#include <stdio.h>

#define print_sizeof(_t)   printf("%4u %s\n", (int) sizeof(_t), #_t)

int main(void) {
    print_sizeof(char);
    print_sizeof(unsigned char);
    print_sizeof(short);
    print_sizeof(unsigned short);
    print_sizeof(int);
    print_sizeof(unsigned int);
    print_sizeof(long);
    print_sizeof(unsigned long);
    print_sizeof(long long);
    print_sizeof(unsigned long long);
    print_sizeof(size_t);
    print_sizeof(uintptr_t);
    print_sizeof(void*);
    return 0;
}
