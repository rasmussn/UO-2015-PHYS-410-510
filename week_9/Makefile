FC = mpif90

all : mpi_ring

mpi_ring : mpi_ring.f90
	$(FC) -o mpi_ring mpi_ring.f90

clean :
	rm -f *.o *.mod mpi_ring
