C  COMPUTES ATMOSPHERIC MASS ABOVE GIVEN ALTITUDE FOR AN INPUT MODEL ATMOSPHERE 
      SUBROUTINE ATMASS
                                                           
        CHARACTER*4 SOURCE(20)
        REAL MBAR,MINF,MSINF,MASS,MO,LATIT,MONTH
        DIMENSION T(80),G(80),MBAR(80),TERM(80)               
        COMMON/DIMVAR/ACF(10),AEX(10),ADV(10),ALPHA(61),DISS(1),E(160),
     *    EN(10),EO(160),FLUX(160),MASS(80),RHO(80),Z(80),GLAT(15),
     *    GCUT(15),GFRC(15)
        COMMON/VAR/C,EFLUX,ERGMEV,ETOT,IBTA,IDIS,IDIVID,IMAX,
     *    ISPCTR,JMAX,JMAXL,KMAX,MO,NENX,NEVNT,PI,Y,ERNG0,
     *    ERNG1,ERNG2,RK1,RK2,RB1,RB2,NEFL,NLAT,ICHQ,NRUN,ILAT
        DIMENSION TEMP(59)
        DATA TEMP/312.,288.,264.,240.,223.3,212.9,205.3,199.5,195.1,
     *    191.7,189.3,187.7,187.0,186.9,186.9,186.9,190.8,194.7,
     *    198.6,202.5,206.4,210.4,214.3,219.6,225.1,230.5,236.0,
     *    241.5,247.0,252.5,258.0,263.5,269.0,270.7,270.7,266.9,
     *    261.4,255.9,250.4,244.8,239.3,233.7,228.5,226.5,224.5,
     *    222.5,220.6,218.6,216.7,216.7,216.7,216.7,216.7,223.3,
     *    236.2,249.2,262.2,275.2,288.2/
        RD=2.87E6
        DO 16 JZ=1,JMAX
          JUSE=JMAX-JZ + 1
          ZUSE=(JZ-1)*2
          Z(JUSE)=ZUSE
          DJZ=(JZ-1)
          IF(JZ.EQ.1)MASS(JUSE)=1013.0E0
          IF(JZ.NE.1)MASS(JUSE)=1013.0E0 * EXP(-0.2844*DJZ)
          RHO(JUSE) = MASS(JUSE) * 1.E3 /RD/TEMP(JZ)
c          print *,' juse=',juse,' z=',z(juse),' m=',mass(juse)
  16    CONTINUE
        DO 30 JZ=1,JMAX
c         print *,' jz=',jz,' z=',z(jz),' m=',mass(jz)
 30     CONTINUE
        RETURN
      END
                                                                       
