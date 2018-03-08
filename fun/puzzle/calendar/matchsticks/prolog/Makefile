TARGETS = ms_demo1 ms_cal_20160920_erratum ms_cal_20160920_corrected
CC = gcc
PCC = gplc
PROLOG_LD_FLAGS = -L /opt/local/lib/gprolog-1.4.4/lib -lbips_fd -lbips_pl -lengine_fd -llinedit -lengine_pl

ifeq ($(METHOD),)
METHOD = full_brute_force
endif

SOLVER = ms_solve_$(METHOD)

EXPORTS = export LOCALSZ=750000; export GLOBALSZ=750000; export TRAILSZ=524288

%.o: %.pl
	gplc -o $@ -c $<

all: $(TARGETS)

ms_demo1: ms_demo1.o $(SOLVER).o
	$(PCC) -o $@ $^

ms_cal_20160920_erratum: ms_cal_20160920_erratum.o $(SOLVER).o
	$(PCC) -o $@ $^

ms_cal_20160920_corrected: ms_cal_20160920_corrected.o $(SOLVER).o
	$(PCC) -o $@ $^

clean:
	rm -f *~ *.o $(TARGETS)

.PHONY: run-ms_demo1
run-ms_demo1: ms_demo1
	./$<

.PHONY: run-ms_cal_20160920_erratum
run-ms_cal_20160920_erratum: ms_cal_20160920_erratum
	$(EXPORTS); ./$<

.PHONY: run-ms_cal_20160920_corrected
run-ms_cal_20160920_corrected: ms_cal_20160920_corrected
	$(EXPORTS); ./$<
