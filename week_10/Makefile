FC = mpif90
CC = cc

all : hello mpi_ring

hello : hello.c
	$(CC) -g -o hello hello.c

mpi_ring : mpi_ring.o
	gfortran -o mpi_ring mpi_ring.o

mpi_ring.o : mpi_ring.f90
	$(FC) -c -g mpi_ring.f90

check : mpi_ring
	@echo "Running mpi_ring ..."
	@echo
	@mpirun -np 8 mpi_ring

clean :
	rm -f *.o *.mod mpi_ring hello
