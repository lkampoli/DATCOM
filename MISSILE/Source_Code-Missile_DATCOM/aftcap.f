      SUBROUTINE AFTCAP(L, YP, XLE, B, XM, XW, CS, CP, DCA, FAIL)
C      
C     CALCULATE CP AND DCA/DY OF AN AFT SEGMENT OF NOSE CAP
C      
C     INPUT VARIABLES
C          L - NUMBER OF POINTS ARROUND NOSE CAP
C         YP - Y-POSITION ON FIN
C        XLE - L.E. LOCATION AT YP
C          B - SPAN OF TWO PANELS ALONE
C         XM - MACH NUMBER
C         CS - COSINE OF LE SWEEP
C      
C     INPUT VARIABLES FROM COMMON
C         XO - ROOT LOCATIONS OF SOSL
C         XT - TIP LOCATIONS OF SOSL
C      DZDXO - SURFACE SLOPE AT XO
C      DZDXT - SURFACE SLOPE AT XT
C        CPO - MAX. PRESSURE COEFFICIENT
C      
C     OUTPUT VARIABLES
C         XW - LOCATIONS OF PRESSURES
C         CP - CP AT XW AND YP
C        DCA - D(CA)/DY FOR AFT NOSE CAP
C      
C     OUTPUT VARIABLES TO COMMON
C       DZDX - SURFACE SLOPES AT XW AND YP
C      
      DIMENSION XW(60), CP(60), CPN(60)
C      
      LOGICAL FAIL
C      
      COMMON /CAFD/  XO(60), XT(60), DZDXO(60), DZDXT(60), TGA(60),
     1               ETA(60), DZDX(60), NS, CPO
C     ...
C     ... CALCULATE NEWTONIAN AND POTENTIAL CP AT EACH XW
C     ...
      N1 = L+1
      AJ = YP/(B/2.0)
      DO 1000 I=1,N1
        XW(I) = XO(I)+AJ*(XT(I)-XO(I))
        XP = XW(I)
        IF(I .EQ. N1) XP = XP-0.00001
        DELT  = ATAN(DZDX(I+1))
        SIND  = SIN(DELT)
        CPN(I)= CPO*SIND*SIND*CS*CS
        CALL CP3DW(XP, YP, B, XM, CP(I),FAIL)
 1000 CONTINUE
      ICASE = 2
      IF(CP(2) .LE. CPN(2)) GO TO 1040
C     ...
C     ... CASE 1 - CP(2) .GT. CPN(2)
C     ...
        ICASE = 1
        DO 1010 I=3,L
          IF(CP(I) .LT. CPN(I)) GO TO 1020
 1010   CONTINUE
C       ...
C       ... CASE 1A - USE POTENTIAL CP AT ALL XW
C       ...
        GO TO 1040
 1020   CONTINUE
C       ...
C       ... CASE 1B - USE CPN UP TO XW(KLL)
C       ...
        KLL = I-1
        DO 1030 I=2,KLL
          CP(I) = CPN(I)
 1030   CONTINUE
 1040 CONTINUE
      IF(CP(2) .GT. CPN(2)) GO TO 1090
      IF(ICASE .EQ. 1) GO TO 1090
C       ...
C       ... CASE 2 - CP(2) .LE. CPN(2)
C       ...
        DO 1050 I=3,L
          IF(CP(I) .GT. CPN(I)) GO TO 1070
          IF(CP(I) .LT. 0.0) GO TO 1060
 1050   CONTINUE
 1060   CONTINUE
C       ...
C       ... CASE 2A - USE POTENTIAL CP AT ALL XW
C       ...
        GO TO 1090
 1070   CONTINUE
C       ...
C       ... CASE 2B AND 2C - USE CPN UP TO XW(KLL)
C       ...
        KLL = I-1
        DO 1080 I=2,KLL
          CP(I) = CPN(I)
 1080   CONTINUE
 1090 CONTINUE
C     ...
C     ... INTEGRATE FOR DCA USING TRAPAZOIDAL RULE FOR
C     ... POINTS 2 TO L+1
C     ...
      DCA = 0.0
      DD1 = CP(2)*DZDX(2)
      DO 1100 I=2,L
        DD2 = CP(I+1)*DZDX(I+1)
        DX  = XW(I+1)-XW(I)
        DCA = DCA+(DD1+DD2)*DX/2.0
        DD1 = DD2
 1100 CONTINUE
C      
      RETURN
      END
