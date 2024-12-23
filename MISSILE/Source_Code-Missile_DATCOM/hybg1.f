      SUBROUTINE HYBG1(N4,N3,IERROR,BETA,RP,X,R,N,NSHAPE,N1,N2,
     1 NN1,NN2,NN3,NN,VOVS,DRB)
C
C  ROUTINE ADAPTED FROM NSWC AEROPREDICTION CODE
C  ORIGINAL ROUTINE WRITTEN BY F. MOORE, NSWC
C  MODIFIED BY J. JENKINS, WL/FIGC
C
C***  THIS ROUTINE FOR CONFIGURATIONS HAVING POINTED NOSES
C
C***  INPUT PARAMETERS
C       BETA   - MACH PARAMETER, SQRT(M**2-1)
C       X      - ARRAY OF BODY X COORDINATES REFERED TO SHIFTED
C                ORIGIN
C       R      - ARRAY OF BODY RADIAL COORDINATES
C       N      - NUMBER OF LONGITUDINAL STATIONS
C       NSHAPE - EQUALS 1 FOR NOSE ALONE
C              - EQUALS 2 FOR NOSE + BODY
C              - EQUALS 3 FOR NOSE + TAIL
C              - EQUALS 4 FOR NOSE + BODY + TAIL
C       N1     - NUMBER OF STATIONS FROM NOSE TIP TO END OF NOSE SEGMENT
C       N2     - NUMBER OF STATIONS FROM NOSE TIP TO END OF BODY SEGMENT
C       N3     - EQUALS 1 FOR CONICAL TAIL SEGMENT
C              - EQUALS 2 FOR OGIVAL TAIL SEGMENT
C       N4     - EQUALS 0 FOR CONICAL NOSE SEGMENT
C              - EQUALS 1 FOR ALL OTHER NOSE SEGMENTS
C       VOVS   - MACH NUMBER
C       DRB    - SLOPE OF MERIDIAN CURVE AT NOSE TIP
C
C***  OUTPUT VARIABLES
C       IERROR - EQUALS 0, IF NO ERRORS ENCOUNTERED
C              - EQUALS 1, IF NOSE TIP ANGLE .GT. MACH ANGLE
C       RP     - SLOPE AT TIP OF NOSE
C       NN1    - NUMBER OF GRID POINTS FROM NOSE TIP TO END OF
C                NOSE SEGMENT
C       NN2    - NUMBER OF GRID POINTS FROM NOSE TIP TO END OF
C                BODY SEGMENT
C       NN3    - NUMBER OF GRID POINTS FROM NOSE TIP TO END OF
C                TAIL SEGMENT
C       NN     - TOTAL NUMBER OF GRID POINTS
C
C
      COMMON /PARTF/ PARTS(19)
      COMMON /VDARY/ XB(220),RB(220),RBP(220),C(220),C1(220),RB1(220),
     1  RBP1(220),B(220),PSI(220),ZE0X(220),PSIR(220),PHIX(220)
      DIMENSION X(50),R(50)
      LOGICAL PARTS,FLAG
C
      NN1=0
      NN2=0
      NN3=0
      IERROR=0
      BETA=SQRT(VOVS**2-1.0)
      C2=0.9
      C4=20.0
      C3=C2/C4
      CZE=BETA**3
      BET1=BETA
      IF(BET1 .GT. 1.0) BET1=1.
      IF(CZE .GT. 10.) CZE=10.
C
C***  TEST FOR NOSE ANGLE .LE. MACH ANGLE
C
      IF(DRB .LE. TAN(ASIN(1./VOVS))) GO TO 1000
        IF(PARTS(18))WRITE(6,1130)
        IERROR=1
        GO TO 1120
C
C***  HERE FOR NOSE SEGMENT
C
 1000   IF(N4 .NE. 0.) GO TO 1020
C
C***  HERE FOR POINTED CONE
C
          XB(1)=X(1)
          RB(1)=R(1)
          RBP(1)=DRB
          RP=DRB
          DLX=X(N1)/10.
          DLR=R(N1)/10.
          K=0
 1010     CONTINUE
          K=K+1
          XB(K+1)=XB(K)+DLX
          RB(K+1)=RB(K)+DLR
          RBP(K+1)=DRB
          IF(XB(K+1) .LT. (X(N1)-.0001)) GO TO 1010
          XB(K+1)=X(N1)
          RB(K+1)=R(N1)
          NN=K+1
          NN1=K+1
          GO TO 1060
C
C***  HERE FOR POINTED NOSE OF GENERAL SHAPE
C
 1020     XB(1)=X(1)
          RB(1)=R(1)
          RBP(1)=DRB
          RP=DRB
          RHOB=ABS((1.+DRB**2)**1.5/DRB)
          XB(2)=XB(1)+.025*RHOB/BETA**1.5
          IF(XB(2) .GT. (X(N1)/5.))  XB(2)=X(N1)/5.
          K=2
 1030     CONTINUE
          CALL INTERP(X,R,XB(K),RB(K),N1,3)
          CALL FDP5(X,R,XB(K),RBP(K),N1,1)
          XB(K+1)=XB(K)+C2*BET1*(RB(K)-RB(1))
      IF(XB(K+1) .GE. (X(N1)-.0001)) GO TO 1040
            CALL LOOP(K,100,FLAG)
        GO TO 1050
C
 1040       FLAG=.FALSE.
 1050     CONTINUE
          IF(FLAG) GO TO 1030
          XB(K+1)=X(N1)
          RB(K+1)=R(N1)
          CALL FDP5(X,R,XB(K+1),RBP(K+1),N1,1)
          NN=K+1
          NN1=K+1
 1060   CONTINUE
        IF(NSHAPE .EQ. 1 .OR. NSHAPE .EQ. 3) GO TO 1090
C
C***  HERE FOR CYLINDRICAL BODY SEGMENT
C
          XB(K+2)=XB(K+1)
          RB(K+2)=RB(K+1)
          RBP(K+2)=0.
          XB(K+3)=XB(K+2)+C3*BET1*RB(K+2)
          RB(K+3)=RB(K+2)
          RBP(K+3)=0.
          IJ=1
 1070     CONTINUE
          K=K+1
          IJ=IJ+1
          A=IJ
          C5=A*C3
          XB(K+3)=XB(K+2)+C5*BET1*RB(K+2)
          IF((XB(K+3)-X(N2)) .LT. 2.) GO TO 1080
            DX2=BETA*RB(K+2)
            IF(DX2 .GT. 0.5) DX2=0.5
            XB(K+3)=XB(K+2)+DX2
            DX1=XB(K+2)-XB(K+1)
            AR=DX2/DX1
            IF(AR .GT. 1.25) XB(K+3)=XB(K+2)+1.25*DX1
 1080     CONTINUE
          RB(K+3)=RB(K+2)
          RBP(K+3)=0.
          IF(XB(K+3) .LT. (X(N2)-.0001)) GO TO 1070
          XB(K+3)=X(N2)
          RB(K+3)=R(N2)
          NN=K+3
          NN2=K+3
 1090   CONTINUE
        IF(NSHAPE .EQ. 1 .OR. NSHAPE .EQ. 2) GO TO 1120
C
C***  HERE FOR TAIL SEGMENT
C
          IF(N3 .EQ. 2) GO TO 1100
C
C***  HERE FOR CONICAL TAIL SEGMENT
C
            IF(NSHAPE .EQ. 4) K=K+2
            XB(K+2)=XB(K+1)
            RB(K+2)=RB(K+1)
            RBP(K+2)=(R(N)-RB(K+2))/(X(N)-XB(K+2))
            XB(K+3)=X(N)
            RB(K+3)=R(N)
            RBP(K+3)=RBP(K+2)
            NN=K+3
            NN3=K+3
            GO TO 1120
C
C***  HERE FOR TAIL SEGMENT OF GENERAL SHAPE
C
 1100       IF(NSHAPE .EQ. 3) NA=N1
            IF(NSHAPE .EQ. 4) NA=N2
            IF(NSHAPE .EQ. 4) K=K+2
            XB(K+2)=XB(K+1)
            RB(K+2)=RB(K+1)
            CALL FDP5(X,R,XB(K+2),RBP(K+2),N,NA)
            XB(K+3)=XB(K+2)+C3*BET1*RB(K+2)
            CALL INTERP (X,R,XB(K+3),RB(K+3),N,NA)
            CALL FDP5(X,R,XB(K+3),RBP(K+3),N,NA)
            IJ=1
 1110       CONTINUE
            K=K+1
            IJ=IJ+1
            A=IJ
            C5=A*C3
            XB(K+3)=XB(K+2)+C5/CZE*BET1*RB(K+2)
            CALL INTERP (X,R,XB(K+3),RB(K+3),N,NA)
            CALL FDP5(X,R,XB(K+3),RBP(K+3),N,NA)
            IF(XB(K+3) .LT. (X(N)-.0001)) GO TO 1110
            XB(K+3)=X(N)
            RB(K+3)=R(N)
            CALL FDP5(X,R,XB(K+3),RBP(K+3),N,NA)
            NN=K+3
            NN3=K+3
 1120 CONTINUE
C
 1130 FORMAT(4X,'*** NOSE TIP ANGLE GREATER THAN MACH ANGLE,',
     1 ' HYBRID THEORY INVALID',/,8X,'SECOND ORDER SHOCK EXPANSION',
     2 ' TO BE USED',/)
C
      RETURN
      END
