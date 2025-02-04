      SUBROUTINE FINCAP(AMACH,XOVC,TOC,CAF,CAP)
C
C***  FIN ALONE SUBSONIC PRESSURE DRAG
C***  METHOD FROM S.F. HOERNER, FLUID DYNAMIC DRAG
C
C***  INPUTS
C
C  MACH -- MACH NUMBER
C  XOVC -- POSITION OF MAXIMUM AIRFOIL THICKNESS, FRAC. C
C   TOC -- FIN MAXIMUM THICKNESS TO CHORD FRACTION
C   CAF -- SKIN FRICTION INCREMENT TO AXIAL FORCE
C
C***  OUTPUT
C
C   CAP -- SUBSONIC PRESSURE DRAG INCREMENT TO AXIAL FORCE
C
      XCMAX=XOVC
C
C***  XOVC MUST BE IN THE RANGE 0.3 TO 0.5
C
      IF(XOVC .LT. 0.3)XCMAX=0.3
      IF(XOVC .GT. 0.5)XCMAX=0.5
C
      AK=4.*(0.3-XCMAX)
      CAP=CAF*((2.+AK)*TOC+60.*TOC**4)
C
C***  VARY CAP BETWEEN MACH 1.0 AND 1.2 TO ZERO AT MACH 1.2
C
      IF(AMACH .LE. 1.0)GO TO 1000
C
      IF(AMACH .GE. 1.2)CAP=0.
      IF(AMACH .GE. 1.2)GO TO 1000
C
      CAP=5.*CAP*(1.2-AMACH)
C
 1000 CONTINUE
C
      RETURN
      END
