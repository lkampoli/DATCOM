      SUBROUTINE BDCAWC(THEOFR,MACH,CAP)
C
C***  CALCULATE THE PRESSURE/WAVE DRAG INCREMENT OF A CONICAL NOSE
C***  CYLINDER AT TRANSONIC SPEEDS USING THE METHOD OF MOORE,
C***  NSWC-TR-80-346, P.29-30
C
C***  INPUTS
C
C THEOFR -- THEORETICAL CONICAL NOSE FINENESS RATIO
C   MACH -- FREESTREAM MACH NUMBER
C
C***  OUTPUT
C
C    CAP -- PRESSURE/WAVE DRAG INCREMENT
C
      COMMON /CONST/ PI,RAD,UNUSED,KAND
C
      REAL MACH
C
      DIMENSION DELTA(6),CAPO(6),CAPL(6),A(6)
C
      DATA DELTA / 0.,5.,10.,15.,20.,25. /
      DATA CAPO / 0.,0.00041,0.00780,0.02464,0.04871,0.08037 /
      DATA CAPL / 0.,1081.2,13.506,75.810,46.728,22.119 /
      DATA A / 100.,9.8,5.381,6.468,5.665,4.760 /
C
      SLOPE=ATAN(1./(2.*THEOFR))*RAD
      IF(SLOPE .GT. 25.)SLOPE=25.
      IF(SLOPE .LT. 0.)SLOPE=0.
C
           IF (SLOPE .GE. 0. .AND. SLOPE .LE. 5.)THEN
              SLOPE1=0.
              SLOPE2=5.
              CAPASO=0.068
              CAPBSO=0.000
              GOTO 100
C
      ELSE IF (SLOPE .GT. 5. .AND. SLOPE .LE. 10.)THEN
              SLOPE1=5.
              SLOPE2=10.
              CAPASO=0.180
              CAPBSO=0.068
              GOTO 100
C
      ELSE IF (SLOPE .GT. 10. .AND. SLOPE .LE. 15.)THEN
              SLOPE1=10.
              SLOPE2=15.
              CAPASO=0.291
              CAPBSO=0.180
              GOTO 100
C
      ELSE IF (SLOPE .GT. 15. .AND. SLOPE .LE. 20.)THEN
              SLOPE1=15.
              SLOPE2=20.
              CAPASO=0.455
              CAPBSO=0.291
              GOTO 100
C
      ELSE IF (SLOPE .GT. 20. .AND. SLOPE .LE. 25.)THEN
              SLOPE1=20.
              SLOPE2=25.
              CAPASO=0.58
              CAPBSO=0.455
      ENDIF
C
  100 CONTINUE
C
      CALL LNTRP(DELTA,CAPO,6,SLOPE1,CAP1)
      CALL LNTRP(DELTA,CAPL,6,SLOPE1,CAP2)
      CALL LNTRP(DELTA,A   ,6,SLOPE1,CAP3)
C
      CAPA=CAP1+CAP2*EXP(-CAP3/MACH)
C
C
      CALL LNTRP(DELTA,CAPO,6,SLOPE2,CAP4)
      CALL LNTRP(DELTA,CAPL,6,SLOPE2,CAP5)
      CALL LNTRP(DELTA,A   ,6,SLOPE2,CAP6)
C
      CAPB=CAP4+CAP5*EXP(-CAP6/MACH)
C
      CAP=CAPA+(CAPA-CAPB)*(SLOPE-SLOPE1)/(SLOPE1-SLOPE2)
C
      IF (MACH .GT. 1.)THEN
         SONICM=(CAP+1.71)/1.71
         IF (SONICM .LT. MACH)
     &    CAP=CAPASO+(CAPASO-CAPBSO)*(SLOPE-SLOPE1)/(SLOPE1-SLOPE2)        
C
      ENDIF
C
      RETURN
      END
