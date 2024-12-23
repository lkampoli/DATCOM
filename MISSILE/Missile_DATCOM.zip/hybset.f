      SUBROUTINE HYBSET(XX,RR,XO,LNOS,BR,NSHAPE,N1,N2,N,N3,N4,DRB)
C
C***  THIS ROUTINE SETS UP PARAMETERS REQUIRED FOR EXECUTION OF
C***  SUBROUTINES HYBGEO AND VANDYK
C
C  ROUTINE ADAPTED FROM NSWC AEROPREDICTION CODE
C  MODIFIED BY J. JENKINS, WL/FIGC
C
C***  INPUT PARAMETERS
C       XX     - ARRAY OF BODY XX OORDINATES, REFERED TO SHIFTED
C                ORIGIN
C       RR     - ARRAY OF BODY RR COORDINATES
C       LNOS   - (LNOSE/DMAX)
C       BR     - NOSE BLUNTNESS RATIO (BNOSE/DMAX)
C       N      - NUMBER OF LONGITUDINAL STATIONS
C
C***  OUTPUT VARIABLES
C       BR     - SET TO ZERO IF NOSE IS POINTED
C       N1     - NUMBER OF STATIONS FROM NOSE TIP TO END OF NOSE
C                SEGMENT
C       N2     - NUMBER OF STATIONS FROM NOSE TIP TO END OF CYLINDRICAL
C                BODY SEGMENT
C       N3     - EQUALS 1 FOR CONICAL TAIL
C              - EQUALS 2 FOR OGIVAL TAIL
C       N4     - EQUALS 0 FOR CONICAL NOSE
C              - EQUALS 1 FOR ALL OTHER NOSES
C       NSHAPE - EQUALS 1 FOR NOSE ALONE
C              - EQUALS 2 FOR NOSE + BODY
C              - EQUALS 3 FOR NOSE + TAIL
C              - EQUALS 4 FOR NOSE + BODY + TAIL
C       NTAIL  - EQUALS 1 FOR BOATTAIL
C              - EQUALS 2 FOR FLARE
C       DRB    - SLOPE OF MERIDIAN CURVE AT NOSE STATION NR. 1
C
      COMMON /CONST/ PI,RAD,UNUSED,KAND
      COMMON /ABODIN/ NX,X0,X(50),R(50),TNOSE,LNOSE,DNOSE,BNOSE,TRUNC,
     1                LCENTR,DCENTR,TAFT,LAFT,DAFT,POWER,DISCON(20),
     2                ADUM(748)
      DIMENSION XX(50),RR(50)
      LOGICAL TRUNC,FLAG
      REAL NX,LNOS,LNOSE,LCENTR,LAFT
C
C***  INITIALIZE
C
      N1=0
      N2=0
      NSHAPE=0
      IF(LNOSE.GT.UNUSED .AND. LCENTR.LE.UNUSED .AND. LAFT.LE.UNUSED)
     1 NSHAPE=1
      IF(LNOSE.GT.UNUSED .AND. LCENTR.GT.UNUSED .AND. LAFT.LE.UNUSED)
     1 NSHAPE=2
      IF(LNOSE.GT.UNUSED .AND. LCENTR.LE.UNUSED .AND. LAFT.GT.UNUSED)
     1 NSHAPE=3
      IF(LNOSE.GT.UNUSED .AND. LCENTR.GT.UNUSED .AND. LAFT.GT.UNUSED)
     1 NSHAPE=4
C
C***  COUNT STATIONS TO END OF NOSE SEGMENT
C
        I=1
 1010    CONTINUE
        IF(XX(I) .GE. (LNOS-XO-.00001)) GO TO 1020
          CALL LOOP(I,N,FLAG)
          GO TO 1030
C
 1020     FLAG=.FALSE.
 1030   CONTINUE
        IF(FLAG) GO TO 1010
        N1=I
        IF(NSHAPE .EQ. 1 .OR. NSHAPE .EQ. 3) GO TO 1080
C
C***  COUNT STATIONS TO END OF BODY SEGMENT
C
 1040     CONTINUE
          IF(ABS(RR(I)-RR(N1)) .GT. .000001) GO TO 1050
            CALL LOOP(I,N,FLAG)
            GO TO 1060
C
 1050       FLAG=.FALSE.
 1060     CONTINUE
          IF(FLAG) GO TO 1040
          N2=I-1
C
C***  INITIALIZE N3,N4
C
 1080   N3=0
        IF(TAFT .EQ. 0) N3=1
        IF(TAFT .EQ. 1) N3=2
        N4=1
        IF(TNOSE .EQ.0) N4=0
C
C***  COMPUTE SLOPE OF MERIDAN CURVE AT FIRST STATION
C
        IF(N1 .NE. 0) GO TO 1090
          DRB=0.
          GO TO 1110
C
 1090     IF(N1 .EQ. 2) GO TO 1100
            CALL FDP5(XX,RR,XX(1),DRB,N1,1)
            GO TO 1110
C
 1100       DRB=(RR(N1)-RR(1))/(XX(N1)-XX(1))
 1110 CONTINUE
C
      RETURN
      END
