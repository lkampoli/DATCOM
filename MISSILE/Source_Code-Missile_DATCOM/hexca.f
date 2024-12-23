      SUBROUTINE HEXCA(L, YP, B, XM, XW, CP, NF, DCA, FAIL)
C
C     CALCULATE CP AND D(CA)/DY ARROUND HEX AIRFOIL
C
C     INPUT VARIABLES
C          L - NUMBER OF POINTS ON NOSE CAP
C         YP - Y-POSITION ON FIN
C          B - SPAN OF TWO PANELS ALONE
C         XM - MACH NUMBER
C
C     INPUT VARIABLES FROM COMMON
C         XO - ROOT LOCATIONS OF SOSL
C         XT - TIP LOCATIONS OF SOSL
C
C     OUTPUT VARIABLES
C         XW - LOCATIONS OF PRESSURES
C         CP - PRESSURE COEFFICIENT AT XW
C         NF - NUMBER OF FINAL POINT ON AIRFOIL
C        DCA - D(CA)/DY
C
      COMMON /CAFD/  XO(60), XT(60), DZDXO(60), DZDXT(60), TGA(60),
     1               ETA(60), DZDX(60), NS, CPO
C
      LOGICAL FAIL
      DIMENSION XW(60), CP(60), DZ(60)
C     ...
C     ... CALCULATE AIRFOIL GEOMETRY
C     ...
      AJ  = 2.0*YP/B
      X11 = XO(L+1)+AJ*(XT(L+1)-XO(L+1))
      X12 = XO(L+2)+AJ*(XT(L+2)-XO(L+2))
      X13 = XO(L+3)+AJ*(XT(L+3)-XO(L+3))
      X14 = XO(L+4)+AJ*(XT(L+4)-XO(L+4))
      DXN = (X14-X11)/32.
      XL1 = X12-X11
      XL2 = X13-X12
      XL3 = X14-X13
      IF(XL2 .LT. 1.E-4) XL2 = 0.0
      IF(XL3 .LT. 1.E-4) XL3 = 0.0
      CALL NDIV(XL1, DXN, N1)
      CALL NDIV(XL2, DXN, N2)
      CALL NDIV(XL3, DXN, N3)
      DCA = 0.0
C     ...
C     ... FWD FLAT SURFACE CP AND DCA
C     ...
      DX = XL1/N1
      NI = L+2
      NF = NI+N1
      DO 10 I=NI,NF
        XW(I) = X11+(I-NI)*DX
        DZ(I) = DZDX(L+2)
        XP = XW(I)
        IF(I .EQ. NI) XP = X11+0.00001
        IF(I .EQ. NF) XP = X12-0.00001
        CALL CP3DW(XP, YP, B, XM, CP(I),FAIL)
        IF(FAIL)GO TO 60
   10 CONTINUE
      CALL SIMPW(CP, DZ, DX, NI, NF, DCA)
C     ...
C     ... CENTER SECTION CP
C     ...
      IF(N2 .EQ. 0) GO TO 30
        DX = XL2/N2
        NI = NF+1
        NF = NI+N2
        DO 20 I=NI,NF
          XW(I) = X12+(I-NI)*DX
          XP = XW(I)
          IF(I .EQ. NI) XP = X12+0.00001
          IF(I .EQ. NF) XP = X13-0.00001
          CALL CP3DW(XP, YP, B, XM, CP(I),FAIL)
          IF(FAIL)GO TO 60
   20   CONTINUE
   30 CONTINUE
C     ...
C     ... AFT WEDGE CP AND DCA
C     ...
      IF(N3 .EQ. 0) GO TO 50
        DX = XL3/N3
        NI = NF+1
        NF = NI+N3
        DO 40 I=NI,NF
          XW(I) = X13+(I-NI)*DX
          DZ(I) = DZDX(L+4)
          XP = XW(I)
          IF(I .EQ. NI) XP = X13+0.00001
          IF(I .EQ. NF) XP = X14-0.00001
          CALL CP3DW(XP, YP, B, XM, CP(I),FAIL)
          IF(FAIL)GO TO 60
   40   CONTINUE
        CALL SIMPW(CP, DZ, DX, NI, NF, DCA)
   50 CONTINUE
C
   60 CONTINUE
C
      RETURN
      END
