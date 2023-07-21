C Read command line arguments for the flux program. The expected
C arguments are:
C   arg1 = input file (NETCDF) with proton fluxes 
C   arg2 = output file (NETCDF) for ionization rates
C   arg3 - output file format (1=WACCM4,2=WACCM6,3=CMIP)
C
C NOTE: These routines may not be compatible with all F77 compilers.
      SUBROUTINE GETCMDLINE(INFILE, OUTFILE,FMAT)
        IMPLICIT NONE
        
        INTEGER IARGC
        EXTERNAL IARGC
        CHARACTER*80 INFILE
        CHARACTER*80 OUTFILE
        CHARACTER*80 FMATSTR
        INTEGER      FMAT
        INTEGER      NARGS
        
    
        NARGS = iargc()
    
        IF (NARGS .NE. 3) THEN
          PRINT *,"ERROR: Program requires three command line arguments:"
          PRINT *,"  speflux <input_file> <output_file> <output_format>"
        END IF
    
        CALL GETARG(1, INFILE)
        CALL GETARG(2, OUTFILE)
        CALL GETARG(3, FMATSTR)
        
        READ(FMATSTR, '(I5)') FMAT         
        
        RETURN
      END
    