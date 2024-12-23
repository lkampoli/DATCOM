      SUBROUTINE HSOSL(CR, CT, B, TR, TT, RLER, RLET, RTER,
     1                 RTET, SWP1, CR1, CR2, CT1, CT2, MACH,
     2                 XM, ZM, XU, ZU, L, FAIL)
C
C     CONSTRUCT ROOT AND TIP LOCATIONS OF SOURCE OR SINK LINES
C     (SOSL), SURFACE SLOPES, AND ASSOCIATED GEOMETRY FOR
C     HEX AIRFOILS
C
C     INPUT VARIABLES
C            CR - ROOT CHORD
C            CT - TIP CHORD
C             B - SPAN OF TWO PANELS ALONE
C            TR - ROOT THICKNESS
C            TT - TIP THICKNESS
C          RLER - ROOT L.E. RADIUS
C          RLET - TIP L.E. RADIUS
C          RTER - ROOT T.E. RADIUS
C          RTET - TIP T.E. RADIUS
C          SWP1 - L.E. SWEEP, DEG.
C           CR1 - ROOT CHORD OF FWD WEDGE
C           CR2 - ROOT CHORD OF AFT WEDGE
C           CT1 - TIP CHORD OF FWD WEDGE
C           CT2 - TIP CHORD OF AFT WEDGE
C          MACH - MACH NUMBER
C
C     OUTPUT VARIABLES
C            XM - X-LOCATIONS OF INTERSECTION OF RLE AND FWD WEDGE
C            ZM - Z-LOCATIONS OF INTERSECTION OF RLE AND FWD WEDGE
C            XU - X-LOCATIONS ON RLE WHERE SLOPE IS 15 DEG.
C            ZU - Z-LOCATIONS ON RLE WHERE SLOPE IS 15 DEG.
C             L - NUMBER OF SOSL POINTS ON BLUNT L.E.
C
C     OUTPUT VARIABLES - COMMON /CAFD/
C            XO - ROOT LOCATIONS OF SOSL
C            XT - TIP LOCATIONS OF SOSL
C         DZDXO - ROOT SURFACE SLOPES FWD OF XO
C         DZDXT - TIP SURFACE SLOPES FWD OF XT
C           TGA - TAN OF SOSL SWEEP BACK
C           ETA - TGA/BETA WHERE BETA = SQRT(MACH**2-1)
C            NS - NUMBER OF SOSL POINTS
C
      REAL MACH
      DIMENSION XM(2), ZM(2), XU(2), ZU(2)
C
      COMMON /CONST/ PI,RAD,UNUSED,KAND
      COMMON /PARTF/ PARTS(19)
      LOGICAL PARTS
      COMMON /CAFD/  XO(60), XT(60), DZDXO(60), DZDXT(60), TGA(60),
     1               ETA(60), DZDX(60), NS, CPO
C
      LOGICAL FAIL
C
C     ...
C     ... SET FIN GEOMETRIC CONSTANTS
C     ...
      B2   = B/2.0
      SWP4 = (B2*TAN(SWP1/RAD)+CT-CR)/B2
      SWP4 = RAD*ATAN(SWP4)
      CS1  = COS(SWP1/RAD)
      RLE  = (RLER+RLET)/2.0
      XT1  = B2*TAN(SWP1/RAD)
      IF(RLE .GE. 1.E-4) GO TO 10
C       ...
C       ... SHARP L.E. AT ROOT AND TIP
C       ...
        XO(1) = 0.0
        XO(2) = CR1
        XT(1) = XT1
        XT(2) = XT(1)+CT1
        XM(1) = 0.0
        ZM(1) = 0.0
        XM(2) = 0.0
        ZM(2) = 0.0
        XU(1) = 0.0
        ZU(1) = 0.0
        XU(2) = 0.0
        ZU(2) = 0.0
        DZDXO(1) = 0.0
        DZDXO(2) = (TR/2.0)/CR1
        DZDXT(1) = 0.0
        DZDXT(2) = (TT/2.0)/CT1
C
        L  = 0
   10 CONTINUE
      IF(RLE .LT. 1.E-4) GO TO 30
C       ...
C       ... BLUNT L.E. AT ROOT AND/OR TIP
C       ...
        IF(RLER .LT. 1.E-4) RLER = 1.E-4
        IF(RLET .LT. 1.E-4) RLET = 1.E-5
        CALL HXZU(RLER, TR, CR1, SWP1, XU(1), ZU(1), DZDXUR,
     1            XM(1), ZM(1))
        CALL HXZU(RLET, TT, CT1, SWP1, XU(2), ZU(2), DZDXUT,
     1            XM(2), ZM(2))
C
        DELR = RAD*ATAN(DZDXUR)
        DELT = RAD*ATAN(DZDXUT)
C
        LR = 17.0-DELR
        LT = 17.0-DELT
        L  = LR
        IF(LT .GT. LR) L = LT
        IF(L .LE. 1) FAIL=.TRUE.
        IF(FAIL .AND. PARTS(18)) WRITE(6,80)
        IF(FAIL) GO TO 70
        N1 = L+1
        XO(1)    = XM(1)-ZM(1)**2/(CS1*(RLER-XM(1)*CS1))
        XT(1)    = XM(2)-ZM(2)**2/(CS1*(RLET-XM(2)*CS1))
     1             +B2*TAN(SWP1/RAD)
        DZDXO(1) = 0.0
        DZDXT(1) = 0.0
        DZDXO(2) = TAN(15.0/RAD)
        DZDXT(2) = DZDXO(2)
        DO 20 I=2,N1
          DELTA = 17-I
C         ...
C         ... ROOT
C         ...
          TAND = TAN(DELTA/RAD)
          IF(DELTA .LT. DELR) TAND = DZDXUR
          GG       = SQRT(TAND*TAND/(CS1*CS1+TAND*TAND))
          XO(I)    = RLER*(1.0-GG)/CS1
          DZDXO(I+1) = TAND
C         ...
C         ... TIP
C         ...
          TAND = TAN(DELTA/RAD)
          IF(DELTA .LT. DELT) TAND = DZDXUT
          GG       = SQRT(TAND*TAND/(CS1*CS1+TAND*TAND))
          XT(I)    = XT1+RLET*(1.0-GG)/CS1
          DZDXT(I+1) = TAND
   20   CONTINUE
        XO(L+2) = CR1
        XT(L+2) = B2*TAN(SWP1/RAD)+CT1
   30 CONTINUE
C     ...
C     ... REMAINDER OF AIRFOIL
C     ...
      XO(L+3) = CR-CR2
      XT(L+3) = XT(L+2)+(CT-CT1-CT2)
      IF(XO(L+3) .LT. XO(L+2)) XO(L+3) = XO(L+2)
      IF(XT(L+3) .LT. XT(L+2)) XT(L+3) = XT(L+2)
      DZDXO(L+3) = 0.0
      DZDXT(L+3) = 0.0
C
      RTE = (RTER+RTET)/2.0
      IF(CR2 .GT. 0.)GO TO 35
         XO(L+4)    = CR
         XT(L+4)    = CT+B2*TAN(SWP1/RAD)
         DZDXO(L+4) = 0.0
         DZDXT(L+4) = 0.0
         DZDXO(L+5) = 0.0
         DZDXT(L+5) = 0.0
         NS = L+4
   35 CONTINUE
      IF(RTE .GE. 1.E-4 .OR. CR2 .LE. 0.0) GO TO 40
C       ...
C       ... SHARP T.E.
C       ...
        XO(L+4) = CR
        XT(L+4) = CT+B2*TAN(SWP1/RAD)
        DZDXO(L+4) = -(TR/2.0)/CR2
        DZDXT(L+4) = -(TT/2.0)/CT2
        DZDXO(L+5) = 0.0
        DZDXT(L+5) = 0.0
        NS = L+4
   40 CONTINUE
      IF(RTE .LT. 1.E-4 .OR. CR2 .LE. 0.0) GO TO 50
C       ...
C       ... BLUNT T.E.
C       ...
        IF(RTER .LT. 1.E-4) RTER = 1.E-4
        IF(RTET .LT. 1.E-4) RTET = 1.E-4
        CALL HXZU(RTER, TR, CR2, SWP4, XUT, ZUT, DZDXUR,
     1            XMT, ZMT)
        XO(L+4) = CR-XUT
        IF(DZDXUR .GT. 0.0) XO(L+5) = CR-XUT+ZUT/DZDXUR
        IF(DZDXUR .LE. 0.0) XO(L+5) = CR
        DZDXO(L+4) = -DZDXUR
        DZDXO(L+4) = -DZDXUR
        DZDXO(L+5) = -DZDXUR
        DZDXO(L+6) = 0.0
C
        CALL HXZU(RTET, TT, CT2, SWP4, XUT, ZUT, DZDXUT,
     1            XMT, ZMT)
        XT(L+4) = CT+B2*TAN(SWP1/RAD)-XUT
        IF(DZDXUT .GT. 0.0) XT(L+5) = XT(L+4)+ZUT/DZDXUT
        IF(DZDXUT .LE. 0.0) XT(L+5) = XT(L+4)+XUT
        DZDXT(L+4) = -DZDXUT
        DZDXT(L+5) = -DZDXUT
        DZDXT(L+6) = 0.0
C
        NS = L+5
   50 CONTINUE
C     ...
C     ... CONSTRUCT TGA AND ETA
C     ...
      BETA = SQRT(MACH**2-1.0)
      DO 60 I=1,NS
        TGA(I) = (XT(I)-XO(I))/B2
        ETA(I) = TGA(I)/BETA
   60 CONTINUE
C
   70 CONTINUE
C
   80 FORMAT(4X,'FIN POTENTIAL FLOW METHOD FAILING.  NOSE CAP/',
     1 ' WEDGE ANGLE > THAN 15 DEGREES.')
      RETURN
      END
