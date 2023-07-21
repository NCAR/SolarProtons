C Fits the observed proton fluxes (pflux) with a power law.
C Each flux is the total flux greater than the specified energy.
      SUBROUTINE FITPFLUX(PENERGY,PFLUX,NCOEFF,E0,F0)
        IMPLICIT NONE
        
        REAL    PENERGY(6)
        REAL    PFLUX(6)
        INTEGER NCOEFF
        REAL    E0(NCOEFF)
        REAL    F0(NCOEFF)
        INTEGER I
        INTEGER I1
        INTEGER I2
        REAL    DE
        CHARACTER*40 TSTSTR

        DO I = 1,3
          I1 = 2*I-1
          I2 = 2*I 
          DE = PENERGY(I2)-PENERGY(I1)
          E0(I)=(DE)/ALOG(PFLUX(I1)/PFLUX(I2))	
          F0(I)=(PFLUX(I1)*ALOG(PFLUX(I1)/PFLUX(I2)))/(DE)/
     *    EXP(-(PENERGY(I1))*ALOG(PFLUX(I1)/PFLUX(I2))/(DE))
  
          IF(E0(I) .LE. 0.0E0)F0(I)=F0(I)*1.E-10
          IF(E0(I) .LE. 0.0E0)E0(I)=-E0(I)
          
C Charley outputs these and reads them in, so there is roundoff
C happening in his code. Add the roundoff here if you want to
C match his ion production values.
C          WRITE(TSTSTR, '(E12.3)') E0(I)
C          READ(TSTSTR, '(E12.3)') E0(I)
                 
C          WRITE(TSTSTR, '(E12.3)') F0(I)
C          READ(TSTSTR, '(E12.3)') F0(I)         
        END DO
      END

