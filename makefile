SRCS = hash_functions.f90 \
       hash_function_test.f90 \
       hash_table_test.f90 \
       hash_table_module.f90 

OBJS = $(SRCS:.f90=.o)

FC     = gfortran
FFLAGS = -fimplicit-none -fdefault-integer-8 -fdefault-real-8 -O1
EXES   = hash_function_test hash_table_test

default : $(EXES)

# Compiler steps for all objects
$(OBJS) : %.o : %.f90
	$(FC) $(FFLAGS) -c $<

# Linker
$(EXES) : % : %.o
	$(FC) $(FFLAGS) -o $@ $^

clean:
	rm -rf $(OBJS) $(EXES:=.exe) *.mod
#	del $(OBJS) $(EXES:=.exe) *.mod

.PHONY: default clean

# Dependencies
$(OBJS) : $(SRCS)
hash_table_module.o : hash_functions.o
hash_table_test.o   : hash_functions.o hash_table_module.o
hash_table_test     : hash_table_test.o hash_functions.o hash_table_module.o
hash_function_test.o: hash_functions.o
hash_function_test  : hash_function_test.o hash_functions.o
