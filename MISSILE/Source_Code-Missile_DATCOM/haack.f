      SUBROUTINE HAACK(XO,TNOSE,DNOSE,BNOSE,TRUNC,LAC,
     &           DISCON,X,R,NX,AA)
C
C***  COMPUTE THE CHARACTERISTICS OF AN L-V HAACK
C***           OR VON KARMAN SEGMENT
C***  INPUTS
C
C        XO - LONGITUDINAL OFFSET OF NOSE TIP
C     TNOSE - NOSE TYPE: HAACK OR KARMAN
C     DNOSE - NOSE BASE DIAMETER
C     BNOSE - SPHERICAL NOSE BLUNTNESS RADIUS
C     TRUNC - TRUNCATED FLAG (.TRUE. IF TRUNCATED, NOT BLUNTED)
C       LAC - ACTUAL NOSE LENGTH (TIP TO BASE)
C
C***  OUTPUTS
C
C    DISCON - INDICES OF X STATIONS WHERE THE SURFACE
C             SLOPE IS DISCONTINUOUS
C         X - LONGITUDINAL LOCATIONS
C         R - RADIAL LOCATIONS AT EACH X STATION
C        NX - NUMBER OF X AND R POINTS
C        AA - ARRAY OF CHARACTERISTICS TO BE COMPUTED
C             AA(1)=PLANFORM AREA
C             AA(2)=WETTED AREA
C             AA(3)=VOLUME
C             AA(4)=VOLUME CENTROID
C             AA(5)=PLANFORM CENTROID
C             AA(6)=FINENESS RATIO
C             AA(7)=THEORITICAL LENGTH
C             AA(8)=THEORITICAL FINENESS RATIO
C
      COMMON /CONST/ PI,RAD,UNUSED,KAND 
C
      DIMENSION AA(8),X(50),R(50),XX(101),RR(101),DISCON(20)
      REAL K,LTH,LAC,LCALC,NX
      LOGICAL TRUNC
C
C***  CONSTANTS
      NMAX=500
      LMAX=100
      EPS=0.001*BNOSE
      BR=0.5*DNOSE
C
C***  DECIDE IF NOSE TYPE IS L/V HAACK OR KARMAN
      K=0.0
      NTYPE=TNOSE+0.5
      IF (NTYPE.EQ.3) K=1./3.
C
C***  CHECK FOR VALIDITY OF USER INPUTS
      IF (BNOSE.GE.DNOSE/2.) THEN
        IF (TRUNC) THEN
          STOP 1234
        ELSE
C
C***  NOSE IS HEMISPHERICAL CAP
          BNOSE=DNOSE/2.
          LAC=BNOSE
          NX=0.0
          CALL SPHERE(AA,NX,X,R,DNOSE)
          GO TO 140
C***  END OF HEMISPHERICAL CAP CALCULATIONS
C
        ENDIF
      ELSEIF (BNOSE.GT.UNUSED) THEN
        IF (BNOSE.GT.LAC) BNOSE=LAC
      ENDIF
C
C***  CALCULATE GEOMETRY FOR TRUNCATED NOSES
      IF (TRUNC) THEN
      NPTS=101
C
C***  SET THEORETICAL LENGTH TO ACTUAL LENGTH
      LTH=LAC
C
C***  START OF ITERATION FOR THEORETICAL NOSE LENGTH
      ADD=0.0
      LOOP=0
      LCALC=LAC-EPS-1.
 10   IF (ABS(LAC-LCALC).GT.EPS) THEN
        LOOP=LOOP+1
C
C***    CHECK FOR MAXIMUM ITERATIONS
        IF (LOOP.GT.LMAX) THEN
          STOP 1234
        ENDIF
        LTH=LTH+ADD
C
C***    USE NEWTON-RAPHSON METHOD TO SOLVE FOR PHI
C***    MAKE INITIAL GUESS OF PHI
        PHI=ACOS(1.-2.*BNOSE/BR)
        DO 30 I=1,NMAX
          SPHI=SIN(PHI)
          CPHI=COS(PHI)
          S2PHI=SIN(2.*PHI)
          C2PHI=COS(2.*PHI)
          F=PHI-0.5*S2PHI+K*SPHI**3-PI*BNOSE**2/BR**2
          FPRIME=1.-C2PHI+3.*K*SPHI**2*CPHI
          PHINEW=PHI-F/FPRIME
          IF (ABS(PHINEW-PHI).LT.EPS) THEN
            X0=LTH*(1.-CPHI)/2.
            LCALC=LTH-X0
            ADD=LAC-LCALC
            GO TO 10
          ENDIF
          PHI=PHINEW
 30     CONTINUE
        STOP 1234
      ENDIF
C
C***  LONGITUDINAL AND RADIAL LOCATIONS ON HAACK SURFACE
      DO 40 I=1,NPTS
        XX(I)=X0+FLOAT(I-1)*(LTH-X0)/FLOAT(NPTS-1)
        IF (I.EQ.NPTS) THEN
          PHI=PI
        ELSE
          PHI=ACOS(1.-2.*XX(I)/LTH)
        ENDIF
        RR(I)=BR*SQRT((PHI-0.5*SIN(2.*PHI)+K*SIN(PHI)**3)/PI)
 40   CONTINUE
C
C***  SET DISCONTINUOUS POINT
C***  FIRST OF SEGMENT IS DISCONTINUOUS IF TRUNCATED
      DISCON(1)=1.0
      GO TO 110
      ENDIF
C***  END OF TRUNCATED NOSE CALCULATIONS
C
C***  CALCULATE GEOMETRY FOR SHARP NOSES
      IF (BNOSE.LE.UNUSED) THEN
C
C***  LONGITUDINAL AND RADIAL LOCATIONS ON HAACK SURFACE
      NPTS=101
      DO 50 I=1,NPTS
        XX(I)=FLOAT(I-1)*LAC/FLOAT(NPTS-1)
        IF (I.EQ.1) THEN
          PHI=0.0
        ELSEIF (I.EQ.NPTS) THEN
          PHI=PI
        ELSE
          PHI=ACOS(1.-2.*XX(I)/LAC)
        ENDIF
        RR(I)=BR*SQRT((PHI-0.5*SIN(2.*PHI)+K*SIN(PHI)**3)/PI)
 50   CONTINUE
      LTH=LAC
      X0=0.0
      GO TO 110
      ENDIF
C***  END OF SHARP NOSE CALCULATIONS
C
C***  CALCULATE GEOMETRY FOR BLUNTED NOSES
      NPTS=51
C
C***  SET THEORETICAL LENGTH TO ACTUAL LENGTH
      LTH=LAC
C
C***  START OF ITERATION FOR THEORETICAL NOSE LENGTH
      ADD=0.0
      LOOP=0
      LCALC=LAC-EPS-1.
  60  IF (ABS(LAC-LCALC).GT.EPS) THEN
        LOOP=LOOP+1
C
C***    CHECK FOR MAXIMUM ITERATIONS
        IF (LOOP.GT.LMAX) THEN
          STOP 1234
        ENDIF
        LTH=LTH+ADD
        FR=LTH/DNOSE
C
C***    USE NEWTON-RAPHSON METHOD TO SOLVE FOR PHI
C***    MAKE INITIAL GUESS OF PHI
        PHI=ACOS(1.-2.*BNOSE/BR)
        DO 80 I=1,NMAX
          SPHI=SIN(PHI)
          CPHI=COS(PHI)
          S2PHI=SIN(2.*PHI)
          C2PHI=COS(2.*PHI)
          F=PHI-0.5*S2PHI+SPHI**2/(PI*FR**2)
     &      *(1.+3.*K*CPHI+9.*K**2*CPHI**2/4.)
     &      +K*SPHI**3-PI*BNOSE**2/BR**2
          FPRIME=1.-C2PHI+S2PHI/(PI*FR**2)
     &      *(1.+3.*K*CPHI+9.*K**2*CPHI**2/4.)
     &      -SPHI**3/(PI*FR**2)*(3.*K+9.*K**2*CPHI/2.)
     &      +3.*K*SPHI**2*CPHI
          PHINEW=PHI-F/FPRIME
          IF (ABS(PHINEW-PHI).LT.EPS) THEN
            XT=LTH*(1.-CPHI)/2.
            YT=BR*SQRT((PHI-0.5*S2PHI+K*SPHI**3)/PI)
            DELTA=XT+SQRT(BNOSE**2-YT**2)
            X0=DELTA-BNOSE
            LCALC=LTH-X0
            ADD=LAC-LCALC
            GO TO 60
          ENDIF
          PHI=PHINEW
 80     CONTINUE
        STOP 1234
      ENDIF
C
C***  LONGITUDINAL AND RADIAL LOCATIONS ON SPHERICAL CAP
      DO 90 I=1,NPTS
        XX(I)=FLOAT(I-1)*(XT-X0)/FLOAT(NPTS-1)+X0
        IF (I.EQ.1) THEN
          RR(I)=0.0
        ELSE
          RR(I)=SQRT(BNOSE**2-(XX(I)-DELTA)**2)
        ENDIF
 90   CONTINUE
      ILAST=NPTS-1
C
C***  LONGITUDINAL AND RADIAL LOCATIONS ON HAACK SURFACE
      DO 100 I=1,NPTS
        J=I+ILAST
        XX(J)=XT+FLOAT(I-1)*(LTH-XT)/FLOAT(NPTS-1)
        IF ((1.-2.*XX(J)/LTH).LT.-1.0) THEN
          PHI=PI
        ELSE
          PHI=ACOS(1.-2.*XX(J)/LTH)
        ENDIF
        RR(J)=BR*SQRT((PHI-0.5*SIN(2.*PHI)+K*SIN(PHI)**3)/PI)
 100  CONTINUE
C
C***  END OF BLUNTED NOSE CALCULATIONS
C
C***  REMOVE X0 OFFSET (THEORETICAL LENGTH -ACTUAL LENGTH)
C***  AND ADD IN XO (USER SPECIFIED LONGITUDINAL OFFSET)
 110  DO 120 I=1,101
        XX(I)=XX(I)-X0+XO
 120  CONTINUE
C
C***  EXTRACT EVERY TENTH POINT FOR OUTPUT AND SUBSEQUENT CALCS
      J=0
      DO 130 I=1,101,10
        J=J+1
        X(J)=XX(I)
        R(J)=RR(I)
 130  CONTINUE
C
C***  UPDATE POINT TOTAL
      NX=FLOAT(J)
C
C***  PLANFORM AREA
      AA(1)=DSPLAN(101,XX,RR)
C
C***  WETTED AREA
      AA(2)=DSWET(101,XX,RR)
C
C***  VOLUME
      AA(3)=DVOL(101,XX,RR)
C
C***  VOLUME CENTROID
      AA(4)=DXCENV(101,XX,RR)
C
C***  PLANFORM AREA CENTROID
      AA(5)=DXCENP(101,XX,RR)
C
C***  FINENESS RATIO
      AA(6)=LAC/DNOSE
C
C***  THEORETICAL NOSE LENGTH
      AA(7)=LTH
C
C***  THEORETICAL FINENESS RATIO
      AA(8)=LTH/DNOSE
C
 140  RETURN
      END
