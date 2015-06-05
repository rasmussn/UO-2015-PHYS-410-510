FC = mpif90
CC = mpicc

all : mpi_ring mpi_ring_c

mpi_ring_c : ring.c
	$(CC) -o mpi_ring_c ring.c

mpi_ring : mpi_ring.o
	gfortran -o mpi_ring mpi_ring.o

mpi_ring.o : mpi_ring.F90
	$(FC) -c -g mpi_ring.F90

check : mpi_ring
	@echo "Running mpi_ring ..."
	@echo
	@mpirun -np 8 mpi_ring

clean :
	rm -f *.o *.mod mpi_ring mpi_ring_c
