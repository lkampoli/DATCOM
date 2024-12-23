      SUBROUTINE BLUNT(BETA,IERROR,RP,X,R,NN1,VOVS,K,RR,DRB)
C
C  ROUTINE ADAPTED FROM NSWC AEROPREDICTION CODE
C  ORIGINAL ROUTINE WRITTEN BY F. MOORE, NSWC
C  MODIFIED BY J. JENKINS, WL/FIGC
C
C***  THIS ROUTINE COMPUTES THE GRID POINTS AND SLOPES FOR
C***  BLUNTED OR TRUNCATED NOSES
C
C***  INPUT PARAMETERS
C       BETA - MACH PARAMETER, SQRT(M**2-1)
C       X    - ARRAY OF BODY AXIAL COORDINATES, REFERED TO SHIFTED
C              ORIGIN
C       R    - ARRAY OF BODY RADIAL COORDINATES
C       VOVS - MACH NUMBER
C       DRB  - SLOPE OF NOSE CAP SHOULDER CONTOUR
C
C***  OUTPUT VARIABLES
C       IERROR - SET TO 1, IF NOSE CAP SHOULDER ANGLE IS GREATER
C                THAN MACH ANGLE
C       RP     - SLOPE AT TIP OF PSEUDO NOSE CAP
C       NN1    - NUMBER OF COORDINATE SETS ALONG THE PSEUDO NOSE
C       K      - EQUALS NN1-1
C       RR     - EQUALS R(1)/COS(TH1)
C
      COMMON /CONST/ PI,RAD,UNUSED,KAND
      COMMON /VDARY/ XB(220),RB(220),RBP(220),C(220),C1(220),
     1 RB1(220),RBP1(220),B(220),PSI(220),ZE0X(220),PSIR(220),PHIX(220)
      COMMON /PARTF/ PARTS(19)
      LOGICAL PARTS
      DIMENSION RP(6),X(50),R(50)
      LOGICAL FLAG1,FLAG2
C
      F=1.0
      VOV=VOVS
      IF(VOVS.LE.1.19) VOV=1.0001
      AMU=TAN(ASIN(1./VOV))*F
      IF(DRB.LE.AMU) GO TO 1000
C
C***  SLOPE AT SHOULDER IS .GT. MACH SLOPE ---  RETURN.
C
        IF(PARTS(18))WRITE(6,1090)
        IERROR=1
        GO TO 1070
C
 1000   IF(VOVS .GT. 2.1) GO TO 1020
C
C***  HERE FOR 1.1 .LT. MACH .LT. 2.1
C
          TH1=ATAN(DRB)
          THET1=27.5/RAD
          IF(THET1 .LE. TH1) GO TO 1010
C
C***  HERE FOR NON-CONICAL PSEUDO NOSE
C
            RR=R(1)/COS(TH1)
            XBR=R(1)*DRB
            XB(1)=XBR-RR/SIN(THET1)
            RB(1)=0.
            RBP(1)=TAN(THET1)
            RP(1)=RBP(1)
            XB(2)=XBR-RR*SIN(THET1)
            RB(2)=RR*COS(THET1)
            RBP(2)=RBP(1)
            FLAG1=.FALSE.
            GO TO 1030
C
C***  HERE FOR CONICAL PSEUDO-NOSE
C
 1010       XB(1)=-R(1)/DRB
            RB(1)=0.
            RBP(1)=DRB
            XB(2)=0.
            RB(2)=R(1)
            RBP(2)=DRB
            K=1
            NN1=2
            FLAG1=.TRUE.
            GO TO 1030
C
C***  HERE FOR MACH .GT. 2.1
C
 1020     TH1=ATAN(DRB)
          RR=R(1)/COS(TH1)
          XBR=R(1)*DRB
          XB(1)=XBR-RR*VOVS
          RB(1)=0.
          RBP(1)=1./BETA
          RP(1)=RBP(1)
          XB(2)=XBR-RR/VOVS
          RB(2)=RR*BETA/VOVS
          RBP(2)=RBP(1)
          FLAG1=.FALSE.
 1030   CONTINUE
        IF(FLAG1) GO TO 1070
C
C***  NOW CALCULATE DATA TO END OF PSEUDO NOSE
C
          DXB=-XB(2)/100.
          K=2
 1040     CONTINUE
          XB(K+1)=XB(K)+DXB
          RB(K+1)=SQRT(RR**2-(XBR-XB(K+1))**2)
          RBP(K+1)=(XBR-XB(K+1))/RB(K+1)
          IF(XB(K+1).GE.X(1)) GO TO 1050
            CALL LOOP(K,100,FLAG2)
            GO TO 1060
C
 1050       FLAG2=.FALSE.
 1060     CONTINUE
          IF(FLAG2) GO TO 1040
C
C***  EQUATE LAST DATA ON PSEUDO NOSE TO DATA AT START OF NOSE SECTION
C
          XB(K+1)=X(1)
          RB(K+1)=R(1)
          RBP(K+1)=DRB
          NN1=K+1
 1070   CONTINUE
C
 1090 FORMAT(4X,'*** NOSE CAP SHOULDER ANGLE GREATER THAN MACH ANGLE',
     1 ' HYBRID THEORY INVALID.',/,8X,'SECOND ORDER SHOCK EXPANSION',
     2 ' TO BE USED.')
C
      RETURN
      END
