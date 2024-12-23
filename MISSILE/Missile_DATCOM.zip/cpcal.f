      SUBROUTINE CPCAL(MACH,DELTA,IBOAT,CPMDB)
C
C *** CALCULATES PRESSURE COEFFICIENT BASED ON PANEL FLOW
C *** DEFLECTION ANGLE AND EMPIRICAL PRESSURE METHODS
C *** LEESIDE = ACM EMPIRICAL
C *** WINDWARD = DAHLEM-BUCK EMPIRICAL
C
      COMMON /CONST/PI,RAD,UNUSED,KAND
C
      REAL MACH,LMACH
      DATA DELCHK/.3926990816/
      DATA DELCH1/.039537137/
      DATA RAD16/.2792526803/
C
      LMACH=ALOG(MACH)
      UPMACH=1.0/(MACH*MACH*RAD16)
      IF (DELTA.LE.0.0) GO TO 1030
      IF (DELTA.LE.DELCH1) GO TO 1000
      IF (DELTA.GE.DELCHK) GO TO 1010
      CP=1.0/SIN(4.0*DELTA)**0.75+1.0
      GO TO 1020
 1000 CONTINUE
      CP=5.0
      GO TO 1020
 1010 CONTINUE
      CP=2.0
 1020 CONTINUE
      SINDEL=SIN(DELTA)
      CALL SRATIO(MACH,LMACH,DELTA,RATIO,A2)
      CPMDB=CP*SINDEL*SINDEL*RATIO
      GO TO 1050
 1030 CONTINUE
      DELCRT=(4.*MACH-16.)/RAD
      IF (MACH.GT.4.) DELCRT=0.0
      IF (IBOAT.EQ.0) DELCRT=0.0
      IF (DELTA.LE.-RAD16+DELCRT) GO TO 1040
C
C **  HERE IF ABS(DELTA).LE.16-DELCRT DEG.
C
      CPMDB=(DELTA-DELCRT)*UPMACH
      IF (CPMDB.GT.0.0) CPMDB=0.0
      GO TO 1050
 1040 CONTINUE
C
C **  HERE IF ABS(DELTA).GT.16-DELCRT DEG
C
      CPMDB=-RAD16*UPMACH
 1050 CONTINUE
      RETURN
      END
