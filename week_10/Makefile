FC = mpif90

all : mpi_ring

mpi_ring : mpi_ring.o
	gfortran -o mpi_ring mpi_ring.o

mpi_ring.o : mpi_ring.F90
	$(FC) -c -g mpi_ring.F90

check : mpi_ring
	@echo "Running mpi_ring ..."
	@echo
	@mpirun -np 8 mpi_ring

clean :
	rm -f *.o *.mod mpi_ring
