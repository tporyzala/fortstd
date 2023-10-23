# compile flags
GCFLAGS = -c -ffixed-line-length-none -ffree-line-length-none -fimplicit-none -fopenmp -Ofast -ftree-parallelize-loops=6

all: lib

lib: lib_kinds.mod lib_constants.mod lib_math.mod

lib_kinds.mod:
	gfortran $(GCFLAGS) lib_kinds.f08

lib_constants.mod:
	gfortran $(GCFLAGS) lib_constants.f08

lib_math.mod:
	gfortran $(GCFLAGS) lib_math.f08

clean:
	del /f *.o *.mod