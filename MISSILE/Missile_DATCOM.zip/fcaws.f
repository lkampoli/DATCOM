      SUBROUTINE FCAWS(MACH,KSHARP,TOCEFF,SPLAN,SREF,SWEP00,CAW)
C
C***  SUPERSONIC FIN WAVE DRAG INCREMENT TO AXIAL FORCE
C***  METHOD FROM DATCOM, SECTION 4.1.5.1
C
C***  INPUTS
C
C   MACH -- MACH NUMBER
C KSHARP -- FIN SHAPE FACTOR
C TOCEFF -- EFFECTIVE THICKNESS TO CHORD RATIO
C  SPLAN -- PLANFORM AREA FOR ONE PANEL
C   SREF -- REFERENCE AREA
C SWEP00 -- LEADING EDGE SWEEP ANGLE OF PANEL, DEG.
C
C***  OUTPUT
C
C    CAW -- WAVE DRAG INCREMENT TO AXIAL FORCE
C
      COMMON /CONST/ PI,RAD,UNUSED,KAND
C
      REAL MACH,KSHARP
C
      IF(MACH .LE. 1.)CAW=0.
      IF(MACH .LE. 1.)GO TO 1000
C
      BETA=SQRT(MACH**2-1.)
C
C***  SUPERSONIC LEADING EDGE
C
      CAW=KSHARP/BETA*TOCEFF**2*SPLAN/SREF
      IF(SWEP00 .EQ. 0.)GO TO 1000
C
C***  SUBSONIC LEADING EDGE
C
      IF(ABS(BETA/TAN(SWEP00/RAD)) .LT. 1.) CAW=KSHARP/
     1   ABS(TAN(SWEP00/RAD))*TOCEFF**2*SPLAN/SREF
C
 1000 CONTINUE
C
      RETURN
      END
