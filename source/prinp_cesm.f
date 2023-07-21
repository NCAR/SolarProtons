C This is a verion of the input routine that has been hard coded
C with settings that are used to create an SPE file for CESM (WACCM).
C It has also been modified to work with NETCDF files for the
C proton flux data instead of text files and that input has been
C moved to prread. 

      SUBROUTINE INPUT
        REAL MO,MASS
        COMMON/DIMVAR/ACF(10),AEX(10),ADV(10),ALPHA(61),DISS(1),E(160),
     *    EN(10),EO(160),FLUX(160),MASS(80),RHO(80),Z(80),GLAT(15),
     *    GCUT(15),GFRC(15)
        COMMON/VAR/C,EFLUX,ERGMEV,ETOT,IBTA,IDIS,IDIVID,IMAX,ISPCTR,
     *    JMAX,JMAXL,KMAX,MO,NENX,NEVNT,PI,Y,ERNG0,
     *    ERNG1,ERNG2,RK1,RK2,RB1,RB2,NEFL,NLAT,ICHQ,NRUN,ILAT
        COMMON/DAY/DAYB,DAYE
        DIMENSION A1(10)
  
  
C  IMAX=NO. ENERGY GRID POINTS                                                  
C  JMAX=NO. ATMOSPHERIC LEVELS                                                  
C  KMAX=NO. PITCH ANGLES                                                        
C  IDIS=NO. OF DIFFERENT DISSOCIATION PRODUCTS
        IMAX = 60
        JMAX = 59
        KMAX = 35
        IDIS = 1  
        PRINT *,IMAX,JMAX,KMAX,IDIS

C  DISS(1) = ENERGY REQ'D TO PRODUCE AN ION PAIR
        DISS(1) = 35.0E-6
        PRINT *,(DISS(I),I=1,IDIS)
                                                                
C  GENERATE ATMOSPHERIC LEVELS...MASS                                             
        JMAXL=JMAX-1
        CALL ATMASS

C  MO=REST MASS OF INCIDENT PARTICLES...GRAMS                                   
C  Y=CHARGE  (FOR PROTONS Y=1...FOR ALPHAS Y=2)
        MO = 1.673E-24
        Y  = 1.0                                 
        PRINT *,MO,Y   
                                                         
C  SPECIFY RANGE-ENERGY PARAMETERS FOR PROTONS
        ERNG0 = 0.0
        ERNG1 = 1550.
        ERNG2 = 1.E+5
  
        RK1 = 2.71E-3
        RB1 = 1.72
        RK2 = 0.834
        RB2 = 0.94
  
C  SPECITY GEOMAGNETIC CUTOFFS AND AREAS INVOLVED
        NLAT = 1
        ILAT = 1

C   NLAT ARE THE NUMBER OF LATITUDES READ IN.
C   IF ILAT=0 THEN AVERAGE THE PRODUCTION RATE OF IONS OVER THE
C   LATITUDE RANGE.
C   IF ILAT=NLAT THEN CALCULATE THE PRODUCTION RATE OF IONS AT
C   ILAT LATITUDES.
        GLAT(1) = 90.
        GCUT(1) = 0.0
        GFRC(1) = 1.0

  
C   IF ISPCTR=0 THEN USE A POWER LAW FOR THE PROTON SPECTRUM
C   IF ISPCTR=1 THEN USE AN EXPONENTIAL FALL-OFF FOR THE PROTON SPEC.
C   IF ISPCTR=2 THEN USE A COMBINATION OF THE EXPONENTIAL FALL-OFF
C   AND THE POWER LAW.  THE EXPONENTIAL IS USED FIRST.
C   IBTA GIVES THE PITCH ANGLE DISTRIBUTION
C   IDIVID=NO. OF INTERVALS INTO WHICH THE ENERGY RANGE IS
C   DIVIDED WITH TWO PARAMETERS DESCRIBING THE SPECTRUM IN EACH
C   INTERVAL.
C   NRUN ARE THE NUMBER OF PCA EVENTS STUDIED.
C  EN(II) ARE THE LIMITS OF THESE INTERVALS                                     
        ISPCTR = 1
        IBTA = 0
        NEVNT = 0
        IDIVID = 3
C        NRUN = 365

        EN(2) = 10.0
        EN(3) = 50.0

C  ENERGY FLUX PENETRATING BELOW 110 KM (ERG/CM**2 SEC)                         
        EFLUX = 0.000E+00
        NEFL = 0
C  CONVERT TO MEV/CM**2-SEC                                                     
        EFLUX=EFLUX/(1.602E-06)    

C  READ SMALLEST ENERGY TO BE CONSIDERED                                        
        E(1) = 1.0
        EMAX = 300.
                                                            


C  COEFFICIENTS AND EXPONENTS FOR 2 PARAMETER SPECTRA                           
C READ(1,75)(A1(II),II=1,10)
C 75  FORMAT(10A4)
C READ(1,2233)DAYB,DAYE
C 2233  FORMAT(1X,2F10.2)
C      READ(1,53)(ACF(II),AEX(II),ADV(II),II=1,IDIVID)                                   
C  53  FORMAT(6E12.3)                                                            
C  ENERGY FLUX PENETRATING BELOW 110 KM (ERG/CM**2 SEC)                         

      
                                                     
        WRITE(2,18)                                                               
18      FORMAT(1H1)                                                               
        WRITE(2,19)                                                               
19      FORMAT(/////)                                                             
        WRITE(2,20)                                                               
20      FORMAT(10X,'ATMOSPHERIC PRODUCTION RATES DUE TO ENERGETIC PARTICLE BOMBARDMENT'///)                                                         
        IF(NEFL .EQ. 0)GO TO 514
        WRITE(2,118) EFLUX,NEFL                                                        
118     FORMAT(/2X,'ENERGY FLUX BELOW 115 KM=',1PE14.3,1X,'MEV/CM2 SEC'/,         
     *  ' NEFL=',I3,' IF NEFL=0 THEN NO NORMALIZATION',//)
C  GENERATE PITCH ANGLE GRID POINTS                                             
514     SPACE=PI/2./(KMAX-1)
        DO 10 K=1,KMAX                                                            
          AK=K                                                                      
          ALPHA(K)=(AK-1.)*SPACE                                                    
          IF(K.EQ.KMAX) ALPHA(K)=1.569                                                      
10      CONTINUE                                                                  
        WRITE(2,21)KMAX                                                               
21      FORMAT(2X,'THE PITCH ANGLES USED IN THE INTEGRATION ARE (IN RADIANS) FOR',        
     *    I5,' INTERVALS')
        WRITE(2,22)(ALPHA(K),K=1,KMAX)                                            
22      FORMAT(10(1PE11.3))                                                       
        IMAX1=IMAX-1
        ALO=ALOG10(E(1))
        AHI=ALOG10(EMAX)
        SP=(AHI-ALO)/IMAX1
        DO 113 I=1,IMAX1
113     E(I+1)=10.**(ALO+SP*I)
        WRITE(2,23) E(1),E(IMAX)                                                  
23      FORMAT(//2X,'THE ENERGY RANGE IS',F15.2,1X,'TO',F15.2,1X,'MEV'/)            
        WRITE(2,24) IMAX                                                          
24      FORMAT(2X,'THERE ARE',I5,1X,'DISCRETE ENERGIES CONSIDERED'/)              
        WRITE(2,25) JMAX,MASS(1),MASS(JMAX)                                           
25      FORMAT(2X,'CALCULATIONS ARE DONE AT',I3,1X,'ATMOSPHERIC DEPTHS FROM',        
     1    1PE10.3,1X,'TO',1PE11.3,1X,'GRAMS/CM**2'/)                             
C  COMPUTE PITCH ANGLE NORMALIZATION FACTOR                                     
        ABTA=IBTA                                                                 
        FBTA=(ABTA+1.)/(2.*PI)                                                    
        WRITE(2,40) IBTA,FBTA                                                     
40      FORMAT(/2X,'PITCH ANGLE DISTRIBUTION IS OF THE FORM FBTA*(COS(ALPHA))**',        
     1    I2,1X,'WITH FBTA=',1PE10.3/)                                       
C  ENERGY INTERVALS                                                             
        EN(1)=E(1)-0.001                                                                
        NENX=IDIVID+1                                                             
        EN(NENX)=E(IMAX)+0.001                                                          
        WRITE(2,36) IDIVID                                                        
36      FORMAT(/2X,'THE SPECTRUM IS FIT BY 2 PARAMETERS IN EACH OF',I3,           
     1 1X,'ENERGY BANDS'/)                                                      
        WRITE(2,37)                                                               
37      FORMAT(/10X,'ENERGY RANGE (MEV)',10X,'COEFFICIENT',10X,                   
     1 'IN EXPONENT',6X,'DIVISOR(POWER LAW)',/)
        DO 38 II=1,IDIVID                                                         
          WRITE(2,39) EN(II),EN(II+1),ACF(II),AEX(II),ADV(II)                               
38      CONTINUE                                                                  
C       WRITE(2,76)(A1(II),II=1,10)
C76     FORMAT(///,' THE PARTICLE EVENT WAS ',3X,10A4)
39      FORMAT(10X,F9.3,F12.3,3(10X,1PE11.3))                                          
31      WRITE(2,35) ISPCTR                                                        
35      FORMAT(/2X,'ISPCTR=',I5,/,' IF ISPCTR=0 THEN USE A',
     *  ' POWER LAW FOR THE PROTON SPECTRUM',/,' IF ISPCTR=1',
     *  ' THEN USE AN EXPONENTIAL FALL-OFF FOR THE PROTON SPECTRUM',/,
     *  ' IF ISPCTR=2 THEN USE A COMBINATION OF THE EXPONENTIAL FALL-'
     *  'OFF AND THE POWER LAW',///)
        WRITE(2,43)DISS(1)
43      FORMAT(///,' ENERGY TO CREATE AN ION PAIR=',E12.3,' MEV')
        WRITE(2,18)
        WRITE(2,300)ERNG0,ERNG1,RK1,RB1
300     FORMAT(' BETWEEN',1PE11.3,'  AND',1PE11.3,'  MEV, THE RANGE=  ',
     *  1PE11.3,'*E**(',1PE11.3,')',/)
        WRITE(2,300)ERNG1,ERNG2,RK2,RB2
        WRITE(2,19)
        WRITE(2,301)NLAT
301     FORMAT(' FOR',I3,' LATITUDE INTERVALS WE HAVE LISTED',/,
     * ' THE LATITUDE, THE GEOMAGNETIC CUTOFF (IN MEV),',/,
     * ' AND THE FRACTION OF THE EARTHS  SURFACE ABOVE THAT LATITUDE.',
     *  /,' REMEMBER THAT THIS IS THE GEOMAGNETIC LATITUDE!!!',//)
        WRITE(2,302)(GLAT(I),I=1,NLAT)
        WRITE(2,302)(GCUT(I),I=1,NLAT)
        WRITE(2,302)(GFRC(I),I=1,NLAT)
302     FORMAT(1P8E11.3)
        RETURN
      END
