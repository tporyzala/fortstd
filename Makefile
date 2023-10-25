# compile flags
GCFLAGS = -c -Ofast 
# GCFLAGS += -fopenmp -ftree-parallelize-loops=6
# GCFLAGS += -g -pg -no-pie

all: lib

lib: \
	lib_kinds.mod\
	lib_constants.mod\
	lib_array.mod\
	lib_linalg.mod\
	lib_math.mod\
	lib_random.mod\
	lib_statistics.mod

lib_array.mod: lib_kinds.mod
	gfortran $(GCFLAGS) lib_array.f08

lib_constants.mod: lib_kinds.mod
	gfortran $(GCFLAGS) lib_constants.f08

lib_kinds.mod:
	gfortran $(GCFLAGS) lib_kinds.f08

lib_linalg.mod: lib_kinds.mod
	gfortran $(GCFLAGS) lib_linalg.f08

lib_math.mod: lib_kinds.mod
	gfortran $(GCFLAGS) lib_math.f08

lib_random.mod:
	gfortran $(GCFLAGS) lib_random.f08

lib_statistics.mod: lib_kinds.mod
	gfortran $(GCFLAGS) lib_statistics.f08

clean:
	del /f *.o *.mod