#
# this is a comment

#
# define environment variables (compilers/linker/libraries...)

FC = ifort
FFLAGS = -c
LIBS =

#
# define targets

all: hello

hello.o: hello.f90
	$(FC) -c hello.f90 -o hello.o

hello: hello.o
	$(FC) -o hello hello.o

# run tests
check: hello
	@echo "Running program hello..."
	@hello

clean:
	rm -f hello.o hello

