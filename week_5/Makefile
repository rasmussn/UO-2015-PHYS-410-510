FC = ifort

FCFLAGS = -g -I/usr/local/include

## LDFLAGS is needed to link (this file is broken if LDFLAGS not defined)
#
#LDFLAGS = -L/usr/local/lib -lnetcdff -Wl,-no_pie


all: surfdata

surfdata: surfdata.o
	$(FC) -o surfdata $(LDFLAGS) surfdata.o

surfdata.o: surfdata.F90
	$(FC) -c $(FCFLAGS) surfdata.F90

check: surfdata
	@surfdata

clean:
	rm -f *.o *.mod surfdata surfdata.i90
