C Create a file formatted for WACCM4.
C
C  dimensions:
C  	time = UNLIMITED ; // (18994 currently)
C  	pressure = 58 ;
C  variables:
C  	int date(time) ;
C  		date:long_name = "current date" ;
C		  date:units = "YYYYMMDD" ;
C  	int datesec(time) ;
C  		datesec:long_name = "current seconds of current date" ;
C  	float pressure(pressure) ;
C  		pressure:long_name = "pressure" ;
C  		pressure:units = "mbar" ;
C  		pressure:positive = "down" ;
C  	float Prod(time, pressure) ;
C  		Prod:long_name = "Ion pair production rate" ;
C  		Prod:units = "/cm3/sec" ;
      SUBROUTINE WRCREATE1(NCID, NLEV)
        IMPLICIT NONE
        
        INTEGER NCID
        INTEGER NLEV
        INCLUDE 'netcdf.inc'
        INTEGER STATUS
        INTEGER TIME_DIM
        INTEGER P_DIM
        INTEGER DIMS(2)
        INTEGER DATE_ID
        INTEGER DATESEC_ID
        INTEGER P_ID
        INTEGER PROD_ID
        
C Define dimensions
        STATUS = NF_DEF_DIM(NCID, 'time', NF_UNLIMITED, TIME_DIM)
        CALL CHECK_ERR(STATUS)
        STATUS = NF_DEF_DIM(NCID, 'pressure', NLEV, P_DIM)
        CALL CHECK_ERR(STATUS)

C Define variables and their attributes
        DIMS(1) = TIME_DIM
        STATUS = NF_DEF_VAR(NCID, 'date', NF_INT, 1, DIMS, DATE_ID)
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, DATE_ID, 'long_name', 4, 'date')
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, DATE_ID, 'units', 8, 'YYYYMMDD')
        CALL CHECK_ERR(STATUS)

        DIMS(1) = TIME_DIM
        STATUS = NF_DEF_VAR(NCID, 'datesec', NF_INT, 1, DIMS, DATESEC_ID)
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, DATESEC_ID, 'long_name', 31, 'current seconds of current date')
        CALL CHECK_ERR(STATUS)

        DIMS(1) = P_DIM
        STATUS = NF_DEF_VAR(NCID, 'pressure', NF_FLOAT, 1, DIMS, P_ID)
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, P_ID, 'long_name', 8, 'pressure')
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, P_ID, 'units', 4, 'mbar')
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, P_ID, 'positive', 4, 'down')
        CALL CHECK_ERR(STATUS)

        DIMS(1) = P_DIM
        DIMS(2) = TIME_DIM
        STATUS = NF_DEF_VAR(NCID, 'Prod', NF_FLOAT, 2, DIMS, PROD_ID)
        CALL CHECK_ERR(STATUS)        
        STATUS = NF_PUT_ATT_TEXT(NCID, PROD_ID, 'long_name', 24, 'Ion pair production rate')
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, PROD_ID, 'units', 8, '/cm3/sec')
        CALL CHECK_ERR(STATUS)
      END
      
C Create a file formatted for WACCM6. This is like WACCM4,
C but with time and time_bnds added and the units for ion
C production converted to /g/s.
C
C  dimensions:
C  	time = UNLIMITED ; // (18994 currently)
C  	pressure = 58 ;
C   nbd = 2 ;
C  variables:
C  	double time(time) ;
C		  time:long_name = "time" ;
C		  time:units = "days since 1850-01-01 00:00:00" ;
C		  time:calendar = "gregorian" ;
C		  time:bounds = "time_bnds" ;
C	  double time_bnds(time, nbd) ;
C		  time_bnds:long_name = "time bounds" ;
C		  time_bnds:units = "days since 1850-01-01 00:00:00" ;
C  	int date(time) ;
C  		date:long_name = "current date" ;
C		  date:units = "YYYYMMDD" ;
C  	int datesec(time) ;
C  		datesec:long_name = "current seconds of current date" ;
C  	float pressure(pressure) ;
C  		pressure:long_name = "pressure" ;
C  		pressure:units = "mbar" ;
C  		pressure:positive = "down" ;
C  	float Prod(time, pressure) ;
C  		Prod:long_name = "Ion pair production rate" ;
C  		Prod:units = "/g/sec" ;
      SUBROUTINE WRCREATE2(NCID, NLEV)
        IMPLICIT NONE
        
        INTEGER NCID
        INTEGER NLEV
        INCLUDE 'netcdf.inc'
        INTEGER STATUS
        INTEGER TIME_DIM
        INTEGER P_DIM
        INTEGER BND_DIM
        INTEGER DIMS(2)
        INTEGER TIME_ID
        INTEGER BND_ID
        INTEGER DATE_ID
        INTEGER DATESEC_ID
        INTEGER P_ID
        INTEGER PROD_ID
        
C Define dimensions
        STATUS = NF_DEF_DIM(NCID, 'time', NF_UNLIMITED, TIME_DIM)
        CALL CHECK_ERR(STATUS)
        STATUS = NF_DEF_DIM(NCID, 'nbd',  2, BND_DIM)
        CALL CHECK_ERR(STATUS)
        STATUS = NF_DEF_DIM(NCID, 'pressure', NLEV, P_DIM)
        CALL CHECK_ERR(STATUS)

C Define variables and their attributes
        DIMS(1) = TIME_DIM
        STATUS = NF_DEF_VAR(NCID, 'time', NF_DOUBLE, 1, DIMS, TIME_ID)
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, TIME_ID, 'long_name', 4, 'time')
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, TIME_ID, 'units', 28, 'days since 1850-01-01 00:00:00')
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, TIME_ID, 'calendar', 9, 'gregorian')
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, TIME_ID, 'bounds', 6, 'time_bnds')
        CALL CHECK_ERR(STATUS)

        DIMS(1) = BND_DIM
        DIMS(2) = TIME_DIM
        STATUS = NF_DEF_VAR(NCID, 'time_bnds', NF_DOUBLE, 2, DIMS, BND_ID)
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, BND_ID, 'long_name', 11, 'time bounds')
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, BND_ID, 'units', 28, 'days since 1850-01-01 00:00:00')
        CALL CHECK_ERR(STATUS)

        DIMS(1) = TIME_DIM
        STATUS = NF_DEF_VAR(NCID, 'date', NF_INT, 1, DIMS, DATE_ID)
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, DATE_ID, 'long_name', 4, 'date')
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, DATE_ID, 'units', 8, 'YYYYMMDD')
        CALL CHECK_ERR(STATUS)

        DIMS(1) = TIME_DIM
        STATUS = NF_DEF_VAR(NCID, 'datesec', NF_INT, 1, DIMS, DATESEC_ID)
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, DATESEC_ID, 'long_name', 31, 'current seconds of current date')
        CALL CHECK_ERR(STATUS)

        DIMS(1) = P_DIM
        STATUS = NF_DEF_VAR(NCID, 'pressure', NF_FLOAT, 1, DIMS, P_ID)
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, P_ID, 'long_name', 8, 'pressure')
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, P_ID, 'units', 4, 'mbar')
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, P_ID, 'positive', 4, 'down')
        CALL CHECK_ERR(STATUS)

        DIMS(1) = P_DIM
        DIMS(2) = TIME_DIM
        STATUS = NF_DEF_VAR(NCID, 'Prod', NF_FLOAT, 2, DIMS, PROD_ID)
        CALL CHECK_ERR(STATUS)        
        STATUS = NF_PUT_ATT_TEXT(NCID, PROD_ID, 'long_name', 24, 'Ion pair production rate')
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, PROD_ID, 'units', 6, '/g/sec')
        CALL CHECK_ERR(STATUS)
      END


C Create a file formatted for CMIP6.
C
C  dimensions:
C	  time = UNLIMITED ; // (164359 currently)
C	  plev = 61 ;
C	  glat = 32 ;
C	nbd = 2 ;
C  variables:
C  	double time(time) ;
C		  time:axis = "T" ;
C		  time:long_name = "time" ;
C		  time:standard_name = "time" ;
C		  time:units = "days since 1850-01-01 00:00:00" ;
C		  time:calendar = "gregorian" ;
C		  time:_CoordinateAxisType = "Time" ;
C		  time:bounds = "time_bnds" ;
C	  double time_bnds(time, nbd) ;
C		  time_bnds:long_name = "bounds of time bin" ;
C		  time_bnds:units = "days since 1850-01-01 00:00:00" ;
C	  double plev(plev) ;
C		  plev:long_name = "Pressure level" ;
C		  plev:axis = "Z" ;
C		  plev:_CoordinateAxisType = "Pressure" ;
C		  plev:units = "hPa" ;
C	  double glat(glat) ;
C		  glat:long_name = "geomagnetic latitude" ;
C		  glat:axis = "Y" ;
C		  glat:_CoordinateAxisType = "Lat" ;
C		  glat:units = "degrees_north" ;
C		  glat:bounds = "glat_bnds" ;
C	  double glat_bnds(glat, nbd) ;
C		  glat_bnds:long_name = "bounds of geomagnetic latitude" ;
C		  glat_bnds:units = "degrees_north" ;
C	    double calyear(time) ;
C		  calyear:long_name = "year of Gregorian calendar" ;
C	  double calmonth(time) ;
C		  calmonth:long_name = "month of Gregorian calendar" ;
C	  double calday(time) ;
C		  calday:long_name = "day of Gregorian calendar" ;
C 	float iprp(time, plev, glat) ;
C  		iprp:long_name = "Ion pair production rate by solar protons" ;
C	  	iprp:units = "g^-1 s^-1" ;
C	  	iprp:cell_methods = "time: mean" ;

      SUBROUTINE WRCREATE3(NCID, NLEV)
        IMPLICIT NONE
        
        INTEGER NCID
        INTEGER NLEV
        INCLUDE 'netcdf.inc'
        INTEGER STATUS
        INTEGER TIME_DIM
        INTEGER P_DIM
        INTEGER BND_DIM
        INTEGER DIMS(2)
        INTEGER TIME_ID
        INTEGER BND_ID
        INTEGER YEAR_ID
        INTEGER MONTH_ID
        INTEGER DAY_ID
        INTEGER P_ID
        INTEGER PROD_ID
        

C Define dimensions
        STATUS = NF_DEF_DIM(NCID, 'time', NF_UNLIMITED, TIME_DIM)
        CALL CHECK_ERR(STATUS)
        STATUS = NF_DEF_DIM(NCID, 'nbd',  2, BND_DIM)
        CALL CHECK_ERR(STATUS)
        STATUS = NF_DEF_DIM(NCID, 'plev', NLEV, P_DIM)
        CALL CHECK_ERR(STATUS)

C Define variables and their attributes
        DIMS(1) = TIME_DIM
        STATUS = NF_DEF_VAR(NCID, 'time', NF_DOUBLE, 1, DIMS, TIME_ID)
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, TIME_ID, 'long_name', 4, 'time')
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, TIME_ID, 'units', 28, 'days since 1850-01-01 00:00:00')
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, TIME_ID, 'calendar', 9, 'gregorian')
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, TIME_ID, 'bounds', 6, 'time_bnds')
        CALL CHECK_ERR(STATUS)

        DIMS(1) = BND_DIM
        DIMS(2) = TIME_DIM
        STATUS = NF_DEF_VAR(NCID, 'time_bnds', NF_DOUBLE, 2, DIMS, BND_ID)
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, BND_ID, 'long_name', 11, 'time bounds')
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, BND_ID, 'units', 28, 'days since 1850-01-01 00:00:00')
        CALL CHECK_ERR(STATUS)

        DIMS(1) = TIME_DIM
        STATUS = NF_DEF_VAR(NCID, 'calyear', NF_INT, 1, DIMS, YEAR_ID)
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, YEAR_ID, 'long_name', 26, 'year of Gregorian calendar')
        CALL CHECK_ERR(STATUS)

        DIMS(1) = TIME_DIM
        STATUS = NF_DEF_VAR(NCID, 'calmonth', NF_INT, 1, DIMS, MONTH_ID)
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, MONTH_ID, 'long_name', 27, 'month of Gregorian calendar')
        CALL CHECK_ERR(STATUS)

        DIMS(1) = TIME_DIM
        STATUS = NF_DEF_VAR(NCID, 'calday', NF_INT, 1, DIMS, DAY_ID)
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, DAY_ID, 'long_name', 25, 'day of Gregorian calendar')
        CALL CHECK_ERR(STATUS)

        DIMS(1) = P_DIM
        STATUS = NF_DEF_VAR(NCID, 'plev', NF_DOUBLE, 1, DIMS, P_ID)
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, P_ID, 'long_name', 14, 'Pressure level')
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, P_ID, 'units', 3, 'hPa')
        CALL CHECK_ERR(STATUS)

        DIMS(1) = P_DIM
        DIMS(2) = TIME_DIM
        STATUS = NF_DEF_VAR(NCID, 'iprp', NF_FLOAT, 2, DIMS, PROD_ID)
        CALL CHECK_ERR(STATUS)        
        STATUS = NF_PUT_ATT_TEXT(NCID, PROD_ID, 'long_name', 40, 'Ion pair production rate by solar protons')
        CALL CHECK_ERR(STATUS)
        STATUS = NF_PUT_ATT_TEXT(NCID, PROD_ID, 'units', 9, 'g^-1 s^-1')
        CALL CHECK_ERR(STATUS)
      END


C Create the ion production file.
C
C There are different formats for the output file:
C   1 - WACCM4 style
C   2 - CMIP6 style
      INTEGER FUNCTION WRCREATE(PATH, NLEV, FMAT)
        IMPLICIT NONE
        
        CHARACTER*80 PATH
        INTEGER NLEV
        INTEGER FMAT
        INCLUDE 'netcdf.inc'
        INTEGER STATUS
        
        STATUS = NF_CREATE(PATH, NF_CLOBBER, WRCREATE)
        CALL CHECK_ERR(STATUS)

        IF (FMAT .EQ. 1) THEN
          CALL WRCREATE1(WRCREATE, NLEV)
        ELSE IF (FMAT .EQ. 2) THEN
          CALL WRCREATE2(WRCREATE, NLEV)
        ELSE IF (FMAT .EQ. 3) THEN
          CALL WRCREATE3(WRCREATE, NLEV)
        ELSE
          PRINT *, 'ERROR: UNKNOWN OUTPUT FORMAT ', FMAT
          STOP
        END IF                  

C Done with the file definition
        STATUS = NF_ENDDEF(WRCREATE);
        CALL CHECK_ERR(STATUS)
      END
      
      
C Close the ion production file.
      SUBROUTINE WRCLOSE(NCID)
        IMPLICIT NONE
        
        INTEGER NCID
        INCLUDE 'netcdf.inc'
        INTEGER STATUS

        STATUS = NF_CLOSE(NCID)
        CALL CHECK_ERR(STATUS)  
      END

C Write the ion production at a particular time offset.
      SUBROUTINE WRPROD(NCID,FMAT,INDEX,NLEV,PROD,RHOA)
        IMPLICIT NONE
        
        INTEGER NCID
        INTEGER FMAT
        INTEGER INDEX
        INTEGER NLEV
        REAL    PROD(NLEV)
        REAL    RHOA(NLEV)
        INCLUDE 'netcdf.inc'
        INTEGER VARID
        INTEGER STATUS
        INTEGER START(2)
        INTEGER COUNT(2)
        INTEGER I
        REAL    TMP(NLEV)
  
        IF (FMAT .LE. 2) THEN
          STATUS = NF_INQ_VARID(NCID, "Prod", VARID)
        ELSE
          STATUS = NF_INQ_VARID(NCID, "iprp", VARID)
        END IF
        CALL CHECK_ERR(STATUS)  
  
        START(1) = 1
        START(2) = INDEX
        COUNT(1) = NLEV
        COUNT(2) = 1
        
C The vertical levels need to be reversed for format 1 and 2, and the units
C are different for format 2 and 3.       
        DO I = 1, NLEV
          IF (FMAT .EQ. 1) THEN        
            TMP(NLEV-I+1) = PROD(I)
          ELSE IF (FMAT .EQ. 2) THEN
            TMP(NLEV-I+1) = PROD(I) / RHOA(I)
          ELSE IF (FMAT .EQ. 3) THEN 
            TMP(I) = PROD(I) / RHOA(I)
          END IF
        END DO
        
        STATUS = NF_PUT_VARA_REAL(NCID, VARID, START, COUNT, TMP)           
        CALL CHECK_ERR(STATUS)  
      END

  
C Write the date and time at a particular time offset.
C
C The calendar information is fairly different between the two
C output formats and is not used by the ion production
C calculation, so let this routine access both the input
C and output files to transfer the appropriate information.
C
C Format 1 (WACCM4) NOTE: The format that is needed for output to the SPE file is:
C  Date  :  YYYYMMDD
C  Time  :  seconds
C
C Format 2 (CMIP6)
      SUBROUTINE WRDATE(NCOUT,FMAT,INDEX,DATE,TIME,NCIN)
        IMPLICIT NONE
        
        INTEGER NCOUT
        INTEGER FMAT
        INTEGER INDEX
        INTEGER DATE
        INTEGER TIME
        INTEGER NCIN
        INCLUDE 'netcdf.inc'
        INTEGER VARID
        INTEGER STATUS
        INTEGER START(2)
        INTEGER COUNT(2)
        DOUBLE PRECISION DTIME
        DOUBLE PRECISION BNDS(2)
        INTEGER YEAR
        INTEGER MONTH
        INTEGER DAY

        IF (FMAT .LE. 2) THEN
          START(1) = INDEX
          COUNT(1) = 1
  
          STATUS = NF_INQ_VARID(NCOUT, "date", VARID)
          CALL CHECK_ERR(STATUS)  
  
          STATUS = NF_PUT_VARA_INT(NCOUT, VARID, START, COUNT, DATE)
          CALL CHECK_ERR(STATUS)  

          STATUS = NF_INQ_VARID(NCOUT, "datesec", VARID)
          CALL CHECK_ERR(STATUS)  
  
          STATUS = NF_PUT_VARA_INT(NCOUT, VARID, START, COUNT, TIME)
          CALL CHECK_ERR(STATUS)
        END IF
        
        IF (FMAT .GE. 2) THEN
C Read in the time and timebnds and copy to the output.

          CALL RDTIME(NCIN,INDEX,DTIME,BNDS) 

          START(1) = INDEX
          COUNT(1) = 1
  
          STATUS = NF_INQ_VARID(NCOUT, "time", VARID)
          CALL CHECK_ERR(STATUS)  
  
          STATUS = NF_PUT_VARA_DOUBLE(NCOUT, VARID, START, COUNT, DTIME)
          CALL CHECK_ERR(STATUS)  

          START(1) = 1
          START(2) = INDEX
          COUNT(1) = 2
          COUNT(2) = 1
  
          STATUS = NF_INQ_VARID(NCOUT, "time_bnds", VARID)
          CALL CHECK_ERR(STATUS)  
  
          STATUS = NF_PUT_VARA_DOUBLE(NCOUT, VARID, START, COUNT, BNDS)
          CALL CHECK_ERR(STATUS)  
        END IF    

        IF (FMAT .GE. 3) THEN
C Write out calyear, calmonth, and calday form date.

          YEAR = DATE / 10000
          MONTH = (DATE - YEAR*10000) / 100
          DAY = DATE - YEAR*10000 - MONTH*100

          START(1) = INDEX
          COUNT(1) = 1
  
          STATUS = NF_INQ_VARID(NCOUT, "calyear", VARID)
          CALL CHECK_ERR(STATUS)  
  
          STATUS = NF_PUT_VARA_INT(NCOUT, VARID, START, COUNT, YEAR)
          CALL CHECK_ERR(STATUS)  

          STATUS = NF_INQ_VARID(NCOUT, "calmonth", VARID)
          CALL CHECK_ERR(STATUS)  
  
          STATUS = NF_PUT_VARA_INT(NCOUT, VARID, START, COUNT, MONTH)
          CALL CHECK_ERR(STATUS)  

          STATUS = NF_INQ_VARID(NCOUT, "calday", VARID)
          CALL CHECK_ERR(STATUS)  
  
          STATUS = NF_PUT_VARA_INT(NCOUT, VARID, START, COUNT, DAY)
          CALL CHECK_ERR(STATUS)  
        END IF    
      END
  
C Write the pressure of the vertical coordinate.
      SUBROUTINE WRPRES(NCID,FMAT,NLEV,PRES)
        IMPLICIT NONE
        
        INTEGER NCID
        INTEGER FMAT
        INTEGER NLEV
        REAL    PRES(NLEV)
        INCLUDE 'netcdf.inc'
        INTEGER VARID
        INTEGER STATUS
        INTEGER START(1)
        INTEGER COUNT(1)
        INTEGER I
        REAL    TMP(NLEV)
         
        START(1) = 1
        COUNT(1) = NLEV
  
        IF (FMAT .LE. 2) THEN
          STATUS = NF_INQ_VARID(NCID, "pressure", VARID)
        ELSE
          STATUS = NF_INQ_VARID(NCID, "plev", VARID)
        END IF
        CALL CHECK_ERR(STATUS)  
  
C The vertical levels need to be reversed.
        IF (FMAT .LE. 2) THEN
          DO I = 1, NLEV
            TMP(NLEV-I+1) = PRES(I)
          END DO
          STATUS = NF_PUT_VARA_REAL(NCID, VARID, START, COUNT, TMP)
        ELSE
          STATUS = NF_PUT_VARA_DOUBLE(NCID, VARID, START, COUNT, DBLE(PRES))
        END IF
                
        CALL CHECK_ERR(STATUS) 
      END
