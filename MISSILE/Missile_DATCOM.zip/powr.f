      SUBROUTINE POWR(AA,NX,X,R,BNOSE,TRUNC,LENGTH,DIAM,EXP,DISCON)
C
C***  COMPUTE THE CHARACTERISTICS OF A POWER SEGMENT
C
C***  INPUTS
C
C     NX -- NUMBER OF INPUT POINTS
C      X -- X STATIONS
C      R -- RADII AT EACH X STATION
C  BNOSE -- FORWARD RADIUS OF SECTION
C  TRUNC -- TRUNCATED FLAG (.TRUE. IF TRUNCATED NOT BLUNTED)
C LENGTH -- SECTION LENGTH
C   DIAM -- SECTION BASE DIAMETER
C    EXP -- EXPONENT OF POWER SHAPE
C
C***  OUTPUTS
C
C     AA -- ARRAY OF CHARACTERISTICS TO BE COMPUTED
C           AA(1)=PLANFORM AREA
C           AA(2)=WETTED AREA
C           AA(3)=VOLUME
C           AA(4)=VOLUME CENTROID
C           AA(5)=PLANFORM CENTROID
C           AA(6)=FINENESS RATIO
C           AA(7)=THEORETICAL LENGTH
C           AA(8)=THEORETICAL FINENESS RATIO
C DISCON -- ELEMENTS WHICH HAVE DISCONTINUOUS SLOPES
C
      COMMON /CONST/ PI,RAD,UNUSED,KAND
C
      DIMENSION AA(8),X(50),R(50),XX(101),RR(101),DISCON(20)
C
      REAL NX,LENGTH
      LOGICAL TRUNC
C
C***  SKIP PRIMARY CALCULATIONS FOR HEMISPHERICAL CAP
C
      IF(2.*BNOSE .GE. DIAM .AND. .NOT. TRUNC)GO TO 1050
C
C***  COMPUTE GEOMETRY AT 101 POINTS
C
      NPTS=101
C
      FR=LENGTH/DIAM
      IF(DIAM .LE. 2.*BNOSE)FR=LENGTH/(2.*BNOSE)
C
C  DO NOT DUPLICATE LAST POINT FOR THIS SEGMENT
C
      IF(NX .GT. UNUSED)NX=NX-1.
      NNX=NX+0.5
      XO=0.
      IF(NX .GT. UNUSED)XO=X(NNX+1)
C
      XT=0.
C
C  THEORETICAL LENGTH OF POWER
C
      THEOL=LENGTH
      FF=THEOL/DIAM
C
C***  SURFACE X AND R COORDINATES
C
      DO 1000 I=1,NPTS
         XI=FLOAT(I-1)*LENGTH/FLOAT(NPTS-1)
         XX(I)=XI
         RR(I)=DIAM/2.*(XI/LENGTH)**EXP
         XX(I)=XX(I)+XO
 1000 CONTINUE
C
C***  EXTRACT EVERY TENTH POINT FOR OUTPUT AND SUBSEQUENT CALCS.
C
      II=0
      DO 1010 I=1,NPTS,10
         II=II+1
         L=II+NNX
         X(L)=XX(I)
         R(L)=RR(I)
 1010 CONTINUE
C
C  UPDATE POINT TOTAL
C
      NX=NX+FLOAT(II)
C
C  PLANFORM AREA
C
      SP=DSPLAN(NPTS,XX,RR)
C
C  WETTED AREA
C
      SW=DSWET(NPTS,XX,RR)
C
C  VOLUME
C
      VOL=DVOL(NPTS,XX,RR)
C
C  VOLUME CENTROID
C
      XCENV=DXCENV(NPTS,XX,RR)
C
C  PLANFORM AREA CENTROID
C
      XCENP=DXCENP(NPTS,XX,RR)
C
C  STORE FOR RETURN TO CALLING ROUTINE
C
      AA(1)=SP
      AA(2)=SW
      AA(3)=VOL
      AA(4)=XCENV
      AA(5)=XCENP
      AA(6)=FR
      AA(7)=THEOL
      AA(8)=FF
C
C  SET DISCONTINUOUS POINTS
C
      IF (TRUNC) DISCON(1)=1.0
      DO 1020 I=1,20
         JJ=I
         IF(DISCON(I).EQ.UNUSED)GO TO 1030
 1020 CONTINUE
      GO TO 1040
C
C  END OF SEGMENT IS DISCONTINUOUS
C
 1030    DISCON(JJ)=NX
 1040 CONTINUE
C
      GO TO 1060
C
C***  HERE FOR SPHERICAL CAP
C
 1050 CONTINUE
C
      IF(LENGTH .GT. BNOSE)LENGTH=BNOSE
C
      CALL SPHERE(AA,NX,X,R,DIAM)
C
 1060 CONTINUE
C
      RETURN
      END
