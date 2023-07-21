C These are a series of routines to read flux data from
C NETCDF files.

      SUBROUTINE CHECK_ERR(STATUS)
        IMPLICIT NONE
        
        INTEGER STATUS
        INCLUDE 'netcdf.inc'
      
        IF (STATUS .NE. NF_NOERR) then
          PRINT *, NF_STRERROR(STATUS)
          STOP
        ENDIF
      END


C Open the proton flux file.
      INTEGER FUNCTION RDOPEN(PATH)
        IMPLICIT NONE
        
        CHARACTER*80 PATH
        INCLUDE 'netcdf.inc'
        INTEGER STATUS
    
        STATUS = NF_OPEN(PATH, NF_SHARE, RDOPEN)
        CALL CHECK_ERR(STATUS)
        RETURN 
      END

C Close the proton flux file.
      SUBROUTINE RDCLOSE(NCID)
        IMPLICIT NONE
        
        INTEGER NCID
        INCLUDE 'netcdf.inc'
        INTEGER STATUS

        STATUS = NF_CLOSE(NCID)
        CALL CHECK_ERR(STATUS)  
      END

C Get the total number of flux values (times) in the file.  
      INTEGER FUNCTION RDNFLUX(NCID)
        IMPLICIT NONE
        
        INTEGER NCID
        INCLUDE 'netcdf.inc'
        INTEGER STATUS
        INTEGER DIMID
    
        STATUS = NF_INQ_DIMID(NCID, 'time', DIMID)
        CALL CHECK_ERR(STATUS)
        STATUS = NF_INQ_DIMLEN(NCID, DIMID, RDNFLUX)
        CALL CHECK_ERR(STATUS)
      END
  
C Read the energy values associated with the proton fluxes.  
      SUBROUTINE RDENERGY(NCID, PENERGY)
        IMPLICIT NONE
        
        INTEGER NCID
        REAL    PENERGY(6)
        INCLUDE 'netcdf.inc'
        INTEGER VARID
        INTEGER STATUS
  
        STATUS = NF_INQ_VARID(NCID, "penergy", VARID)
        CALL CHECK_ERR(STATUS)  

        STATUS = NF_GET_VAR_REAL(NCID, VARID, PENERGY)
        CALL CHECK_ERR(STATUS)  
      END
  
C Read the flux values at a particular time offset.
      SUBROUTINE RDFLUX(NCID,INDEX,PFLUX)
        IMPLICIT NONE
        
        INTEGER NCID
        INTEGER INDEX
        REAL    PFLUX(6)
        INCLUDE 'netcdf.inc'
        INTEGER VARID
        INTEGER STATUS
        INTEGER START(2)
        INTEGER COUNT(2)
  
        STATUS = NF_INQ_VARID(NCID, "pflux", VARID)
        CALL CHECK_ERR(STATUS)  
  
        START(1) = 1
        START(2) = INDEX
        COUNT(1) = 6
        COUNT(2) = 1
  
        STATUS = NF_GET_VARA_REAL(NCID, VARID, START, COUNT, PFLUX)
        CALL CHECK_ERR(STATUS)  
      END
  
C Read the date and time at a particular time offset.
C NOTE: This will make the format that is needed for
C output to the SPE file:
C  Date  :  YYYYMMDD
C  Time  :  seconds
      SUBROUTINE RDDATE(NCID,INDEX,DATE,TIME)
        IMPLICIT NONE
        
        INTEGER NCID
        INTEGER INDEX
        INTEGER DATE
        INTEGER TIME
        INCLUDE 'netcdf.inc'
        INTEGER VARID
        INTEGER STATUS
        INTEGER START(1)
        INTEGER COUNT(1)
        INTEGER YEAR
        INTEGER MONTH
        INTEGER DAY
        REAL    RTIME
  
        START(1) = INDEX
        COUNT(1) = 1
  
        STATUS = NF_INQ_VARID(NCID, "year", VARID)
        CALL CHECK_ERR(STATUS)  
  
        STATUS = NF_GET_VARA_INT(NCID, VARID, START, COUNT, YEAR)
        CALL CHECK_ERR(STATUS)  

        STATUS = NF_INQ_VARID(NCID, "month", VARID)
        CALL CHECK_ERR(STATUS)  
  
        STATUS = NF_GET_VARA_INT(NCID, VARID, START, COUNT, MONTH)
        CALL CHECK_ERR(STATUS)  

        STATUS = NF_INQ_VARID(NCID, "day", VARID)
        CALL CHECK_ERR(STATUS)  
  
        STATUS = NF_GET_VARA_INT(NCID, VARID, START, COUNT, DAY)
        CALL CHECK_ERR(STATUS)
  
        DATE = YEAR * 10000 + 100 * MONTH + DAY
  
        STATUS = NF_INQ_VARID(NCID, "tod", VARID)
        CALL CHECK_ERR(STATUS)  
  
        STATUS = NF_GET_VARA_REAL(NCID, VARID, START, COUNT, RTIME)
        CALL CHECK_ERR(STATUS)
        
        TIME = INT(RTIME * 3600.)
      END


C Read the time and time_bnds at a particular time offset.
      SUBROUTINE RDTIME(NCID,INDEX,TIME,BNDS)
        IMPLICIT NONE
        
        INTEGER NCID
        INTEGER INDEX
        DOUBLE PRECISION TIME
        DOUBLE PRECISION BNDS(2)
        INCLUDE 'netcdf.inc'
        INTEGER VARID
        INTEGER STATUS
        INTEGER START(2)
        INTEGER COUNT(2)
  
        START(1) = INDEX
        COUNT(1) = 1
  
        STATUS = NF_INQ_VARID(NCID, "time", VARID)
        CALL CHECK_ERR(STATUS)  
  
        STATUS = NF_GET_VARA_DOUBLE(NCID, VARID, START, COUNT, TIME)
        CALL CHECK_ERR(STATUS)  

        START(1) = 1
        START(2) = INDEX
        COUNT(1) = 2
        COUNT(2) = 1
  
        STATUS = NF_INQ_VARID(NCID, "time_bnds", VARID)
        CALL CHECK_ERR(STATUS)  
  
        STATUS = NF_GET_VARA_DOUBLE(NCID, VARID, START, COUNT, BNDS)
        CALL CHECK_ERR(STATUS) 
      END
