      SUBROUTINE SPECTR                                            
        REAL MO,MASS
        COMMON/DIMVAR/ACF(10),AEX(10),ADV(10),ALPHA(61),DISS(1),E(160),
     *    EN(10),EO(160),FLUX(160),MASS(80),RHO(80),Z(80),GLAT(15),
     *    GCUT(15),GFRC(15)
        COMMON/VAR/C,EFLUX,ERGMEV,ETOT,IBTA,IDIS,IDIVID,IMAX,
     *    ISPCTR,JMAX,JMAXL,KMAX,MO,NENX,NEVNT,PI,Y,ERNG0,
     *    ERNG1,ERNG2,RK1,RK2,RB1,RB2,NEFL,NLAT,ICHQ,NRUN,ILAT
        IMAX1=IMAX-1
        IF(ICHQ .EQ. 1)GO TO 3010
        DO 2010 I=1,IMAX1
          E(I)=SQRT(EO(I)*EO(I+1))
2010    CONTINUE
3010    IF(ISPCTR .EQ. 1)GO TO 200
        IF(ISPCTR .EQ. 2)GO TO 500
C  POWER LAW ENERGY SPECTRUM                                                    
        DO 26 I=1,IMAX1                                                            
          DO 27 II=1,IDIVID                                                         
            IF(EO(I).GE.EN(II).AND.EO(I).LE.EN(II+1)) GO TO 28                    
            GO TO 27                                                                  
28        CONTINUE                                                                  
            TEMM=-AEX(II)*ALOG(E(I)/ADV(II))
            FLUX(I)=ACF(II)*EXP(TEMM)*(EO(I+1)-EO(I))*2.*PI
27        CONTINUE                                                                  
26      CONTINUE                                                                  
        TEMM=-AEX(1)*ALOG(E(1)/ADV(1))
        FLUX(1)=ACF(1)*EXP(TEMM)*(EO(2)-EO(1))*2.*PI
        GO TO 300
200     DO 226 I=1,IMAX1
          DO 227 II=1,IDIVID
            IF(EO(I) .GE. EN(II) .AND. EO(I) .LE. EN(II+1))GO TO 228
            GO TO 227
228         FLUX(I)=ACF(II)*EXP(-E(I)/AEX(II))*(EO(I+1)-EO(I))*2.*PI
227       CONTINUE
226     CONTINUE
        FLUX(1)=ACF(1)*EXP(-E(1)/AEX(1))*(EO(2)-EO(1))*2.*PI
        GO TO 300
500     DO 526 I=1,IMAX1
          DO 527 II=1,IDIVID
            IF(EO(I) .GE. EN(II) .AND. EO(I) .LE. EN(II+1))GO TO 528
            GO TO 527
528         IF(II .EQ. 2)GO TO 529
            FLUX(I)=ACF(II)*EXP(-E(I)/AEX(II))*(EO(I+1)-EO(I))*2.*PI
            GO TO 527
529         TEMM=-AEX(II)*ALOG(E(I))
            FLUX(I)=ACF(II)*EXP(TEMM)*(EO(I+1)-EO(I))*2.*PI
527       CONTINUE
526     CONTINUE
        FLUX(1)=ACF(1)*EXP(-E(1)/AEX(1))*(EO(2)-EO(1))*2.*PI
300     ETOT=0.0E0
        DO 30 I=1,IMAX1
30      ETOT=ETOT+FLUX(I)*E(I)
        ETOT=ETOT*.5
        ICHQ=1
        IF(NEFL .EQ. 0)GO TO 400
C        WRITE(2,100)EFLUX
100     FORMAT(////,' ENERGY TO BE DEPOSITED IS',E12.3,' MEV',/,
     *    ' THIS CAN BE USED AS A CHECK FOR ENERGY CONSERVATION',/,
     *    ' IF ONLY ONE LATITUDE INTERVAL IS USED AND IF THERE IS',
     *    ' NO ENERGY CUTOFF',//)
        FAC=EFLUX/ETOT
        DO 40 I=1,IMAX1
40      FLUX(I)=FLUX(I)*FAC
        RETURN
400     CONTINUE
C       WRITE(2,100)ETOT
        RETURN                                                                    
      END                                                                       
