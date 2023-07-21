C  PROGRAM TO COMPUTE ATMOSPHERIC IONIZATION AND DISSOCIATION RATES             
C  DUE TO CORPUSCULAR RADIATION, QM WITH UNITS..NO. ION PAIRS/GRAM/SEC          
C  INCIDENT SPECTRUM HAS FORM...DJ(E,ALPHA,M=0)/D(ALPHA)DE=                     
C  J(E,M=0)*FBTA*((COS(ALPHA)**IBTA)                                            
C  WITH UNITS NO. PARTICLES/(CM**2 SEC MEV STERADIAN)                         
C  MASS IS ATMOSPHERIC MASS IN GRAMS/CM**2                                
C  E(I) IS ENERGY IN MEV                                                        
C  ALPHA(K) IS THE PITCH ANGLE IN RADIANS                                       
C  IBTA SPECIFIES THE PITCH ANGLE DISTRIBUTION...IBTA=0 GIVES ISOTROPY
C
C ????, CHARLEY JACKMAN - ORIGINAL
C OCTOBER 2016, CHARLES BARDEEN - ADDED COMMAND LINE
C                                 ADDED NETCDF FILES
C                                 MOVED POWER LAW FIT FROM NOAA ROUTINE


C DECLARE ANY EXTERNAL FUNCTIONS THAT ARE NEEDED BY MAIN.     
      INTEGER  RDOPEN
      EXTERNAL RDOPEN
      INTEGER  RDNFLUX
      EXTERNAL RDNFLUX
      INTEGER  WRCREATE
      EXTERNAL WRCREATE
                                                                             
      REAL MO,MASS                                                                   
      DIMENSION ELSAT(80),EK(81),QM(15),QV(15),HMAS(80),HVOL(80),
     *  ZAVE(80),AVEDM(80),DHEA(12),PROD(200)
      COMMON/DIMVAR/ACF(10),AEX(10),ADV(10),ALPHA(61),DISS(1),E(160),
     *  EN(10),EO(160),FLUX(160),MASS(80),RHO(80),Z(80),GLAT(15),
     *  GCUT(15),GFRC(15)
      COMMON/VAR/C,EFLUX,ERGMEV,ETOT,IBTA,IDIS,IDIVID,IMAX,
     *  ISPCTR,JMAX,JMAXL,KMAX,MO,NENX,NEVNT,PI,Y,ERNG0,
     *  ERNG1,ERNG2,RK1,RK2,RB1,RB2,NEFL,NLAT,ICHQ,NRUN,ILAT
      COMMON/DAY/DAYB,DAYE
      
      CHARACTER*80 INFILE
      CHARACTER*80 OUTFILE
      INTEGER      HIN
      INTEGER      HOUT
      REAL         PENERGY(6)
      REAL         PFLUX(6)
      INTEGER      DATE
      INTEGER      TIME
      INTEGER      FMAT
      REAL         RHOA(80)
      
C                                                                               
C                                                                               
      ICHQ=0
      PI=3.14159                                                                
      C=2.99793E+10                                                             
      ERGMEV=6.24116E+05  
C  IMAX=NO. ENERGY GRID POINTS                                                  
C  JMAX=NO. ATMOSPHERIC LEVELS                                                  
C  KMAX=NO. PITCH ANGLES                                                        
C  E IN MEV GIVES RANGE IN GM/CM**2
      CALL INPUT
      DO 552 I=1,IMAX
552   EO(I)=E(I)
      DO 555 J=1,JMAXL
        ZAVE(J)=(Z(J+1)+Z(J))/2.
        AVEDM(J)=SQRT(MASS(J)*MASS(J+1))
        RHOA(J)=SQRT(RHO(J)*RHO(J+1))
555   CONTINUE
C  SET UP AVERAGE ALTITUDE(KM) AND COLUMN MASS DENSITY(GM/CM**2)
C
C
      ZAVE(JMAX)=0.0E0
C  GET THE NAMES OF THE INPUT AND OUTPUT FILES
      CALL GETCMDLINE(INFILE, OUTFILE, FMAT)
      PRINT *,'INPUT FILE IS ', TRIM(INFILE)
      PRINT *,'OUTPUT FILE IS ', TRIM(OUTFILE)
      PRINT *,'FORMAT IS ', FMAT

C OPEN THE INPUT FILE AND DETERMINE THE NUMBER TIMES TO PROCESS        
      HIN  = RDOPEN(INFILE)      
      NRUN = RDNFLUX(HIN)
      PRINT *,'PROCESSING ', NRUN, ' EVENTS'
      
C CREATE THE OUTPUT FILE
      HOUT = WRCREATE(OUTFILE, JMAX-1, FMAT)
      CALL WRPRES(HOUT, FMAT, JMAX-1, AVEDM(1:JMAX-1))

C GET THE ENERGY LEVELS FOR THE PROTON FLUXES
      CALL RDENERGY(HIN, PENERGY)
      PRINT *,'FLUX ENERGIES', PENERGY
            
      ADV(1) = 0.
      ADV(2) = 0.
      ADV(3) = 0.
      
      DO 2050 JB=1,NRUN

        CALL RDDATE(HIN,JB,DATE,TIME)
        PRINT *,'DATE',DATE,'TIME',TIME

        CALL RDFLUX(HIN,JB,PFLUX)
        PRINT *, 'FLUXES', PFLUX
      
C FIT POWER LAW COEFFICIENTS TO THE FLUXES
C        CALL FITPFLUX(PENERGY,PFLUX,IDIVD,ACF(1:IDIVD),AEX(1:IDIVD))
        CALL FITPFLUX(PENERGY,PFLUX,IDIVD,AEX(1:IDIVID),ACF(1:IDIVID))
        PRINT *, 'COEFFICIENTS'
        DO 77, II = 1, IDIVID
          PRINT *, II, ACF(II), AEX(II), ADV(II)
77      CONTINUE
        PRINT *, ''

2070    IF(ACF(1).LT.1.E-28)GO TO 7788
        CALL SPECTR
C  SET UP ELECTRON SPECTRUM AS IMAX-1 MONOENERGETIC IMPINGING BEAMS
C
7788    DO 550 J=1,JMAX
          HMAS(J)=0.0E0
          HVOL(J)=0.0E0
C          ELSAT(J)=0.0E0
550     CONTINUE
        IF(ACF(1).LT.1.E-28)GO TO 7799
C  INITIALIZE   HMAS(J) = ENERGY DEPOSITED PER GM IN JTH SLAB.
C  INITIALIZE   HVOL(J) = ENERGY DEPOSITED PER CM**3 IN THE JTH SLAB
C      BY ELECTRONS.
C  INITIALIZE   ELSAT(J) = ENERGY DEPOSITED IN JTH SLAB.
C
        KMAX1=KMAX-1
        IMAX1=IMAX-1
        DO 13 I=1,IMAX1
          IF(FLUX(I) .LT. 1.E-30)GO TO 13
          DO 600 K=1,KMAX
600       EK(K)=E(I)
          DO 14 J=1,JMAXL
            DM=MASS(J+1)-MASS(J)
            IF(EK(1) .LT. 1.E-25)GO TO 13
            DO 15 K=1,KMAX1
              CS=SQRT(COS(ALPHA(K))*COS(ALPHA(K+1)))
              IF(EK(K) .LT. 1.E-25)GO TO 15
              IF(EK(K) .GT. ERNG1)GO TO 1200
              ECHEQ=DM/CS/RK1
              ELOG1=ALOG(EK(K))
              EIB=EXP(ELOG1*RB1)
              IF(ECHEQ .GT. EIB)GO TO 1000
              ELOG2=ALOG(-ECHEQ+EIB)
              EFIN=EXP(ELOG2/RB1)
              IF(EFIN .GT. EK(K))GO TO 4115
              ELOS=EK(K)-EFIN
              EK(K)=EFIN
              GO TO 1001
4115          ELOS=0.0
              GO TO 1001
1200          CHEM=CS*RK2*(EK(K)**RB2-ERNG1**RB2)
              IF(CHEM .GT. DM)GO TO 1500
              DMLEFT=DM-CHEM
              ECHEQ=DMLEFT/CS/RK1
              ELOG1=ALOG(ERNG1)
              EIB=EXP(ELOG1*RB1)
              IF(ECHEQ .GT. EIB)GO TO 1000
              ELOG2=ALOG(-ECHEQ+EIB)
              EFIN=EXP(ELOG2/RB1)
              ELOS=EK(K)-EFIN
              EK(K)=EFIN
              GO TO 1001
1500          ECHEQ=DM/CS/RK2
              ELOG1=ALOG(EK(K))
              EIB=EXP(ELOG1*RB2)
              IF(ECHEQ .GT. EIB)GO TO 1000
              ELOG2=ALOG(-ECHEQ+EIB)
              EFIN=EXP(ELOG2/RB2)
              IF(EFIN .GT. EK(K))GO TO 1650
              ELOS=EK(K)-EFIN
              GO TO 1675
1650          ELOS=0.00E0
1675          EK(K)=EFIN
              GO TO 1001
1000          ELOS=EK(K)
              EK(K)=0.0E0
1001          CONTINUE
C              ADD=ELOS*FLUX(I)*ABS(ALPHA(K+1)-ALPHA(K))*GSUB/1.571
C              ELSAT(J)=ELSAT(J)+ADD
              SN1SQ=SIN(ALPHA(K))*SIN(ALPHA(K))
              SN2SQ=SIN(ALPHA(K+1))*SIN(ALPHA(K+1))
              AD1=ELOS*FLUX(I)*.5*ABS(SN2SQ-SN1SQ)/DM
              HMAS(J)=HMAS(J)+AD1
              HVOL(J)=HVOL(J)+AD1*SQRT(RHO(J)*RHO(J+1))
15          CONTINUE
14        CONTINUE
13      CONTINUE

18      FORMAT('1',////)
7799    CONTINUE
C       WRITE(2,41)
41      FORMAT(2X,50HSAMPLE PARTICLE FLUX AT VARIOUS ATMOSPHERIC DEPTHS//)        
C       WRITE(2,42)                                                               
42      FORMAT(3X,26HFLUX AT GIVEN ENERGY (MEV),5X,             
     *    '(NO./CM**2-SEC)'/)
C       WRITE(2,43) E(1),E(5),E(8),E(15),E(20),E(30),E(50),E(80)                  
43      FORMAT(3X,9(1PE10.3))                                                    
C       WRITE(2,43)FLUX(1),FLUX(5),FLUX(8),FLUX(15),
C    *    FLUX(20),FLUX(30),FLUX(50),FLUX(80)
C       WRITE(2,78)                                                               
78      FORMAT(7X,5HZ(KM),3X,10HATMOS MASS,6X,14HH(MEVS/GR-SEC),4X,               
     1    16HH(MEVS/CM*3-SEC)/)                                                    
        DO 79 J=1,JMAXL                                                            
C         WRITE(2,80) ZAVE(J),AVEDM(J),HMAS(J),HVOL(J)                                    
79      CONTINUE                                                                  
80      FORMAT(F12.2,1PE15.4,2(1PE20.4))                                          
72      FORMAT(2X,'HEIGHT(KM)',3X,'ATMOS MASS',6X,'Q(IONS)',7X,'Q(NO)',/)
        DO 703 J=1,JMAXL
          DO 710 I=1,IDIS
            QM(I)=HMAS(J)/DISS(I)
710       CONTINUE
74        FORMAT(F12.1,1PE14.3,7(1PE13.3))                                          
703     CONTINUE
C        WRITE(2,18)                                                               
C        WRITE(6,70)
70      FORMAT(////)
C        WRITE(2,75)                                                               
75      FORMAT(5X,47HPARTICLE IMPACT PRODUCTION RATES (NO./CM*3-SEC)//)           
C        WRITE(2,72)                                                               
        PROD(JMAX)=0.0E0
        DO 803 J=1,JMAXL
          QV(1)=HVOL(J)/DISS(1)
          QV(2)=1.25*QV(1)
          PROD(J)=QV(1)
C          WRITE(2,74) ZAVE(J),AVEDM(J),QV(1),QV(2)
803     CONTINUE
        EZE=0.0E0
        DO 852 J=1,JMAX
          EZE=EZE+ELSAT(J)
852     CONTINUE
        ECOL=0.0E0
        DO 2300 J=1,JMAXL
2300    ECOL=HVOL(J)*(Z(J)-Z(J+1))*1.E+05+ECOL
C        WRITE(2,860)EZE,ECOL
C        WRITE(6,860)EZE,ECOL
860     FORMAT(////,' TOTAL ENERGY DEPOSITED IN SLABS=',E12.4,' MEV',
     *    //,' TOTAL ENERGY DEPOSITED IN A COLUMN=',E12.4,' MEV',//)
        
C WRITE TO NETCDF FILE
        CALL WRDATE(HOUT,FMAT,JB,DATE,TIME,HIN)
        CALL WRPROD(HOUT,FMAT,JB,JMAX-1,PROD(1:JMAX-1),RHOA(1:JMAX-1))
2050  CONTINUE

C Close the NETCDF files.
      CALL RDCLOSE(HIN)
      CALL WRCLOSE(HOUT)
      
8899  STOP                                                                      
      END                                                                       
