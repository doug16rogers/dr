
mandelpi: mandelpi.c Makefile
	$(CC) -o $@ -Wall -Werror -g $< -lm
	./$@ 2
	./$@ 4
	./$@ 6
	./$@ 8
	./$@ 10
# Floating point errors start to kill the calculation beginning here.
# Need to switch to using rational numbers.
	./$@ 12
	./$@ 14
	./$@ 16
#	./$@ 18
