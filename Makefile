# Makefile for creating the go.proton program.
# Charles Bardeen
# October 2016

FORTRAN =	ifort
#FORTRAN =	pgf90
#FORTRAN =	pathf90
#FORTRAN =	gfortran
#FORTRAN =	g95
#FORTRAN =	xlf90

#NFDIR = /usr/local
NFDIR = ${NCAR_ROOT_NETCDF}
#NFDIR = /Volumes/Data/Libraries/netcdf

FFLAGS =
#FFLAGS += -DSINGLE                    # for single precision
#FFLAGS += -DDEBUG                     # for debug print statements


# Add options for the Intel Fortran compiler.
ifeq ($(FORTRAN),ifort)
#  FFLAGS += -ftz -fp-model precise
#  FFLAGS += -fp-model precise
#  FFLAGS += -f77rtl -fast -O3 -ip -static -I${NFDIR}/include -L${NFDIR/LIB}/lib -lnetcdff -lnetcdf
#  FFLAGS += -f77rtl -fast -O3 -ip -I${NFDIR}/include -L${NFDIR/LIB}/lib -lnetcdff -lnetcdf
#  FFLAGS += -f77rtl -fast -O3 -ip -I${NFDIR}/include
  FFLAGS += -132 -I${NFDIR}/include

  # Debug options.
#  FFLAGS += -g -O0 -traceback -fp-stack-check -check bounds -check uninit -fpe0 -ftrapuv

  # The no_pie flags also the executable to work with idb.
#  LDFLAGS = $(FFLAGS) -no_pie -static -L${NFDIR}/lib -lnetcdff -lnetcdf
#  LDFLAGS = $(FFLAGS) -no_pie -static -L${NFDIR}/lib -lnetcdf
  LDFLAGS = $(FFLAGS) -L${NFDIR}/lib -lnetcdf
endif

# Add options for the Portland Group compiler.
ifeq ($(FORTRAN),pgf90)
  FFLAGS  += -Mextend -I${NFDIR}/include

  # Debug options.
#  FFLAGS += -g -O0 -Mbounds

  LDFLAGS = $(FFLAGS) -L${NFDIR}/lib -lnetcdff -lnetcdf
endif

# Add options for the g95 compiler.
ifeq ($(FORTRAN),g95)
#  FFLAGS  += -fzero -ffree-line-length-huge
  FFLAGS  += -ffree-line-length-huge

  # Debug options.
#  FFLAGS += -g -fbounds-check -ftrace=full

  LDFLAGS = $(FFLAGS)
endif

# Add options for the gfortran compiler.
ifeq ($(FORTRAN),gfortran)
  FFLAGS  += -ffree-line-length-none

  # Debug options.
  FFLAGS += -g -fbounds-check -ffpe-trap=zero,invalid,overflow -fbacktrace

  LDFLAGS = $(FFLAGS)
endif

# Add options for the IBM XL Fortran compiler.
#
# NOTE: It doesn't support float to zero.
ifeq ($(FORTRAN),xlf90)
  FFLAGS  += -q64 -qarch=auto -qspillsize=2500 -g -qfullpath

  # Debug options.
  FFLAGS += -qinitauto=7FF7FFFF -qflttrap=ov:zero:inv:en -C

  LDFLAGS = $(FFLAGS)
endif


# Overridning the implicit rules, which would try to use m2c to
# create the .mod.
#%.mod : %.o ;
#%.o : %.F90 ;

# Add the directories where the source files are located.
VPATH := ../source

# Rules for each executable that could be build.
go_proton.exe : prargs.o pratmo.o prfit.o prread.o prwrite.o prspec.o prinp_cesm.o prmain.o
	$(FORTRAN) $(LDFLAGS) -o go_proton prargs.o pratmo.o prfit.o prinp_cesm.o prmain.o prread.o prwrite.o prspec.o

prargs.o : prargs.f
	$(FORTRAN) $(FFLAGS) -c $<

pratmo.o : pratmo.f
	$(FORTRAN) $(FFLAGS) -c $<

prfit.o : prfit.f
	$(FORTRAN) $(FFLAGS) -c $<

prread.o : prread.f
	$(FORTRAN) $(FFLAGS) -c $<

prinp_cesm.o : prinp_cesm.f
	$(FORTRAN) $(FFLAGS) -c $<

prmain.o : prmain.f
	$(FORTRAN) $(FFLAGS) -c $<

prspec.o : prspec.f
	$(FORTRAN) $(FFLAGS) -c $<

prwrite.o : prwrite.f
	$(FORTRAN) $(FFLAGS) -c $<

clean:
	/bin/rm -f *.o *.mod *.exe *.txt *.html
