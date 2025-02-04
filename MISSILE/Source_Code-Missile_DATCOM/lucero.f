      SUBROUTINE LUCERO(BETA,X,CNA)
C
C     THIS ROUTINE USES THE METHODS FROM AIAA PAPER
C     AIAA-84-0575 BY E.F. LUCERO, TO CALCULATE
C     THE NORMAL FORCE CURVE SLOPE CN/ALPHA FOR 2-PANEL
C     FINSETS REFERENCED TO WING PLANFORM AREA
C
C  WRITTEN BY K. DETERS, MCDONNELL DOUGLAS
C
C     INPUTS
C
C     BETA = SQRT(M**2-1.0)
C
C     X  - DEPENDANT ON PLANFORM 
C        = 1./(BETA*AR) FOR NEARLY RECTANGULAR WINGS
C        = 1./(COT(L.E. SWEEP)*BETA) FOR DELTA WINGS
C
C     OUTPUT
C
C     CNA - FIN ALONE CN/ALPHA REFERENCED TO PLANFORM AREA
C
C     PROGRAM AREA
      XX=1/X
      IF (XX .GE. 1.35) BCNA=4.*(1.-.5*X)
      IF (XX .GE. .1 .AND. XX .LT. 1.35) BCNA=3.406-1.4778*X
     &+.48775*X**2-.08233*X**3+6.7867E-3*X**4-2.1626E-4*X**5
      IF (XX .LT. .1) BCNA=4./3.
C
C***  THEORECTICAL CN/ALPHA VALUE FOR 2 - PANELS
      CNA=BCNA/(57.3*BETA)
      RETURN
      END
