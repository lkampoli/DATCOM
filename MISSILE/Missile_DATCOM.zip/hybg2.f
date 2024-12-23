      SUBROUTINE HYBG2(N3,IERROR,BETA,RP,X,R,N,NSHAPE,N1,N2,NN1,
     1 NN2,NN3,NN,VOVS,BR,DRB)
C
C  ROUTINE ADAPTED FROM NSWC AEROPREDICTION CODE
C  ORIGINAL ROUTINE WRITTEN BY F. MOORE, NSWC
C  MODIFIED BY J. JENKINS, WL/FIGC
C
C***  THIS ROUTINE FOR CONFIGURATIONS HAVING BLUNTED NOSES
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
C       N1     - NUMBER OF POINTS FROM NOSE TIP TO END OF NOSE SEGMENT
C       N2     - NUMBER OF POINTS FROM NOSE TIP TO END OF BODY SEGMENT
C       N3     - EQUALS 0, IF NO TAIL SEGMENT
C              - EQUALS 1, IF TAIL SEGMENT IS CONICAL
C              - EQUALS 2, IF TAIL SEGMENT IS OGIVAL
C       VOVS   - MACH NUMBER
C       BR     - NOSE BLUNTNESS RATIO
C       DRB    - SLOPE OF MERIDIAN CURVE AT NOSE TIP
C
C***  OUTPUT VARIABLES
C       IERROR - EQUALS 0, IF NO ERRORS ENCOUNTERED
C              - EQUALS 1, IF ANY ERRORS ENCOUNTERED
C       RP     - SLOPE AT FIRST STATION
C       NN1    - NUMBER OF GRID POINTS ALONG NOSE
C       NN2    - NUMBER OF GRID POINTS ALONG BODY SEGMENT
C       NN3    - NUMBER OF GRID POINTS ALONG TAIL SEGMENT
C       NN     - TOTAL NUMBER OF GRID POINTS
C
      COMMON /VDARY/ XB(220),RB(220),RBP(220),C(220),C1(220),RB1(220),
     1  RBP1(220),B(220),PSI(220),ZE0X(220),PSIR(220),PHIX(220)
      DIMENSION X(50),R(50)
C
      NN1=0
      NN2=0
      NN3=0
      IERROR=0
      BETA=SQRT(VOVS**2-1.0)
      C2=.05
      C4=1.0
      C3=C2/C4
      CZE=BETA**3
      BET1=BETA
      IF(BET1 .GT. 1.0) BET1=1.
      IF(CZE .GT. 10.) CZE=10.
C
C***  GENERATE PSEUDO-NOSE CAP
C
      CALL BLUNT(BETA,IERROR,RP,X,R,NN1,VOVS,K,BR,DRB)
      IF(IERROR .EQ. 1) GO TO 1060
C
***   NOW FINISH NOSE
C
        XB(K+2)=X(1)
        RB(K+2)=R(1)
        CALL FDP5(X,R,XB(K+2),RBP(K+2),N1,1)
        XB(K+3)=XB(K+2)+C3*BET1*RB(K+2)
        CALL INTERP(X,R,XB(K+3),RB(K+3),N1,3)
        CALL FDP5(X,R,XB(K+3),RBP(K+3),N1,1)
        IJ=1
 1000   CONTINUE
        K=K+1
        IJ=IJ+1
        A=IJ
        C5=A*C3
        XB(K+3)=XB(K+2)+C5*BET1*RB(K+2)
        IF(NSHAPE .EQ. 1 .AND. XB(K+3) .GT. X(N1)) XB(K+3)=X(N1)
        CALL INTERP(X,R,XB(K+3),RB(K+3),N1,3)
        CALL FDP5(X,R,XB(K+3),RBP(K+3),N1,1)
        IF(XB(K+3) .LT. (X(N1)-.0001)) GO TO 1000
        XB(K+3)=X(N1)
        RB(K+3)=R(N1)
        CALL FDP5(X,R,XB(K+3),RBP(K+3),N1,1)
        NN1=K+3
        NN=K+3
        IF(NSHAPE .EQ. 1) GO TO 1060
          IF(NSHAPE .EQ. 3) GO TO 1030
C
C***  HERE FOR CYLINDRICAL BODY SEGMENT
C
            XB(K+4)=XB(K+3)
            RB(K+4)=RB(K+3)
            RBP(K+4)=0.
            XB(K+5)=XB(K+4)+C3*BET1*RB(K+4)
            RB(K+5)=RB(K+4)
            RBP(K+5)=0.
            IJ=1
 1010       CONTINUE
            K=K+1
            IJ=IJ+1
            A=IJ
            C5=A*C3
            XB(K+5)=XB(K+4)+C5*BET1*RB(K+4)
            IF((XB(K+5)-X(N2)).LT.2.) GO TO 1020
              DX2=BETA*RB(K+4)
              IF(DX2.GT..5) DX2=.5
              XB(K+5)=XB(K+4)+DX2
              DX1=XB(K+4)-XB(K+3)
              AR=DX2/DX1
              IF(AR.GT.1.25) XB(K+5)=XB(K+4)+1.25*DX1
 1020       CONTINUE
            RB(K+5)=RB(K+4)
            RBP(K+5)=0.
            IF(XB(K+5) .LT. (X(N2)-.0001)) GO TO 1010
            XB(K+5)=X(N2)
            RB(K+5)=R(N2)
            NN2=K+5
            NN=K+5
 1030     CONTINUE
          IF(NSHAPE .EQ. 2) GO TO 1060
C
C***  HERE FOR TAIL SEGMENT
C
            IF(N3 .EQ. 2) GO TO 1040
C
C***  HERE FOR CONICAL TAIL SEGMENT
C
              IF(NSHAPE .EQ. 4) K=K+2
              XB(K+4)=XB(K+3)
              RB(K+4)=RB(K+3)
              RBP(K+4)=(R(N)-RB(K+4))/(X(N)-XB(K+4))
              XB(K+5)=X(N)
              RB(K+5)=R(N)
              RBP(K+5)=RBP(K+4)
              NN3=K+5
              NN=K+5
              GO TO 1060
C
C***  HERE FOR TAIL SEGMENT OF GENERAL SHAPE
C
 1040         IF(NSHAPE .EQ. 3) NA=N1
              IF(NSHAPE .EQ. 4) NA=N2
              IF(NSHAPE .EQ. 4) K=K+2
              XB(K+4)=XB(K+3)
              RB(K+4)=RB(K+3)
              CALL FDP5(X,R,XB(K+4),RBP(K+4),N,NA)
              XB(K+5)=XB(K+4)+C3*BET1*RB(K+4)
              CALL INTERP (X,R,XB(K+5),RB(K+5),N,NA)
              CALL FDP5(X,R,XB(K+5),RBP(K+5),N,NA)
              IJ=1
 1050         CONTINUE
              K=K+1
              IJ=IJ+1
              A=IJ
              C5=A*C3
              XB(K+5)=XB(K+4)+C5/CZE*BET1*RB(K+4)
              CALL INTERP (X,R,XB(K+5),RB(K+5),N,NA)
              CALL FDP5(X,R,XB(K+5),RBP(K+5),N,NA)
              IF(XB(K+5) .LT. (X(N)-.0001)) GO TO 1050
              XB(K+5)=X(N)
              RB(K+5)=R(N)
              CALL FDP5(X,R,XB(K+5),RBP(K+5),N,NA)
              NN3=K+5
              NN=K+5
 1060 CONTINUE
      RETURN
      END
