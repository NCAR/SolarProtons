The source code and scripts in this directory create ionization rates for solar
protons. This consistes of two parts, an ncl script that manages the whole process,
and a Fortran program that is based upon code originally provided by Charlie
Jackman that calculates the ionization rates.

THe fortran code is in source directory, and it must be compiled prior to running
the scripts. There is a Makefile and a scripts (make-proton.csh) that can be used
to compile the code. It can be executed as:

make_proton.csh

Modifications may need to be made to the makelfile depending on your environment
and the compiler you wish to use. The objects and executables whould go into the
bin directory, and the executable name is go_proton.

The overall script is in the scripts directory and is called spe.ncl. There are
comments in the script file, but the main execution section of the code is at the
bottom of the file: There are 4 different things that can be done:

getFiles     - download the data files
processFiles - convert the text based format to a NETCDF format
calcIons     - determine the ion production rates for each NETCDF file
combineFiles - combine the individual flux files together

The raw text file are downloaded into the noaa_txt directory. These files have
5 minute data, and are converted into a 5 minute netcdf file in noass_5m and
also into a hourly file in noaa_1h. It is the hourly files that are used
to generate the ionization rates.

The WACCM4 and WACCM6 code use different formats for the ionization data, so
the script can generate in 3 different formats: WACCM4, WACCM6,or CMIP6. The
output is put into the ions_1h directory, with subidrectories for each of
the types (w4, w6, and c6).

To invoke the scripts for the root directory type:

ncl < scripts/spe.ncl

The data in umbra from GOES starts on 6/13/1998 and ends on 3/9/2020, so
that is the range for which ionization rates can be calculated. There is a
scripts called spe_pad.ncl, which can be used to pad out the file to later
dates in case the are needed for the model to run, but the ionizations rates
will be 0.
