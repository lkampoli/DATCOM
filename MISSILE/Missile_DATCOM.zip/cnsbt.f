      SUBROUTINE CNSBT(A,B,PHI,CNCNO)
C
C ** SLENDER BODY CORRECTION FACTOR FOR ELLIPTICAL BODY
C
C ** INPUTS
C
C       A -- MAJOR AXIS
C       B -- MINOR AXIS
C     PHI -- BODY ROLL ANGLE, DEGREES
C            PHI= 0 IF MAJOR AXIS PERPENDICULAR TO CROSS FLOW
C            PHI=90 IF MINOR AXIS PERPENDICULAR TO CROSS FLOW
C
C ** OUTPUT
C
C   CNCNO -- CORRECTION FACTOR FOR ELLIPTICAL SEGMENT
C
      COMMON /CONST/ PI,RAD,UNUSED,KAND
C
      CNCNO=1.
      IF(A .EQ. 0. .OR. B .EQ. 0.)GO TO 1000
      IF(A .EQ. B)GO TO 1000
C
      CNCNO=(A/B)*COS(PHI/RAD)**2+(B/A)*SIN(PHI/RAD)**2
C
 1000 CONTINUE
C
      RETURN
      END
