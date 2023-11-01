
FC = gfortran

GCFLAGS = -c -Ofast 
# GCFLAGS += -fopenmp -ftree-parallelize-loops=6
# GCFLAGS += -g -pg -no-pie



all: lib

lib: \
	lib_kinds.o\
	lib_constants.o\
	lib_array.o\
	lib_math.o\
	lib_linalg.o\
	lib_random.o\
	lib_statistics.o

lib_array.o: lib_kinds.mod
	$(FC) $(GCFLAGS) lib_array.f08

lib_constants.o: lib_kinds.mod
	$(FC) $(GCFLAGS) lib_constants.f08

lib_kinds.o:
	$(FC) $(GCFLAGS) lib_kinds.f08

lib_linalg.o: lib_kinds.mod lib_math.mod
	$(FC) $(GCFLAGS) lib_linalg.f08

lib_math.o: lib_kinds.mod
	$(FC) $(GCFLAGS) lib_math.f08

lib_random.o:
	$(FC) $(GCFLAGS) lib_random.f08

lib_statistics.o: lib_kinds.mod
	$(FC) $(GCFLAGS) lib_statistics.f08

clean:
	del /f *.o *.mod