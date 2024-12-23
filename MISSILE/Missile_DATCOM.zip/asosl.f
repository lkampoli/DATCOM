      SUBROUTINE ASOSL(CR, CT, B, TR, TT, RLER, RLET, RTER,
     1                 RTET, SWP1, MACH,
     2                 XM, ZM, XU, ZU, L, FAIL)
C
C     CONSTRUCT ROOT AND TIP LOCATIONS OF SOURCE OR SINK LINES
C     (SOSL), SURFACE SLOPES, AND ASSOCIATED GEOMETRY FOR
C     CIRCULAR ARC AIRFOILS
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
C
      DIMENSION XM(2), ZM(2), XU(2), ZU(2)
C
      COMMON /CONST/ PI,RAD,UNUSED,KAND
      COMMON /PARTF/ PARTS(19)
      LOGICAL PARTS
      COMMON /CAFD/  XO(60), XT(60), DZDXO(60), DZDXT(60), TGA(60),
     1               ETA(60), DZDX(60), NS, CPO
C
      LOGICAL FAIL
C     ...
C     ... SET GEOMETRIC CONSTANTS
C     ...
      B2   = B/2.0
      SWP4 = (B2*TAN(SWP1/RAD)+CT-CR)/B2
      SWP4 = RAD*ATAN(SWP4)
      CS1  = COS(SWP1/RAD)
      RLE  = (RLER+RLET)/2.0
      RTE  = (RTER+RTET)/2.0
      XT1  = B2*TAN(SWP1/RAD)
      IF(RLE .GE. 1.E-4) GO TO 10
C       ...
C       ... SHARP L.E.
C       ...
        XO(1) = 0.0
        XT(1) = XT1
        XM(1) = 0.0
        ZM(1) = 0.0
        XM(2) = 0.0
        ZM(2) = 0.0
        XU(1) = 0.0
        ZU(1) = 0.0
        XU(2) = 0.0
        ZU(2) = 0.0
        RRF = TR*((0.5*CR/TR)**2+0.25)
        RTF = TT*((0.5*CT/TT)**2+0.25)
        DRF = ASIN(0.5*CR/RRF)
        DTF = ASIN(0.5*CT/RTF)
C
        DZDXO(1) = 0.0
        DZDXO(2) = TAN(DRF)
        DZDXT(1) = 0.0
        DZDXT(2) = TAN(DTF)
        L  = 0
   10 CONTINUE
      IF(RLE .LT. 1.E-4) GO TO 30
C       ...
C       ... BLUNT L.E.
C       ...
        IF(RLER .LT. 1.E-4) RLER = 1.E-4
        IF(RLET .LT. 1.E-4) RLET = 1.E-4
        CALL AXZU(RLER, TR, CR, SWP1, XU(1), ZU(1), DZDXUR,
     1            XM(1), ZM(1), RRF)
        CALL AXZU(RLET, TT, CT, SWP1, XU(2), ZU(2), DZDXUT,
     1            XM(2), ZM(2), RTF)
C
        DELR = RAD*ATAN(DZDXUR)
        DELT = RAD*ATAN(DZDXUT)
C
        LR = 17.0-DELR
        LT = 17.0-DELT
        L  = LR
        IF(LT .GT. LR) L = LT
C
        IF(L .LE. 1) FAIL=.TRUE.
        IF(FAIL .AND. PARTS(18)) WRITE(6,110)
        IF(FAIL) GO TO 100
C
        XO(1)    = XM(1)-ZM(1)**2/(CS1*(RLER-XM(1)*CS1))
        XT(1)    = XM(2)-ZM(2)**2/(CS1*(RLET-XM(2)*CS1))+XT1
        DZDXO(1) = 0.0
        DZDXT(1) = 0.0
        DZDXO(2) = TAN(15.0/RAD)
        DZDXT(2) = DZDXO(2)
C
        DO 20 I=2,L
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
          XT(I)    = RLET*(1.0-GG)/CS1+XT1
          DZDXT(I+1) = TAND
   20   CONTINUE
        XO(L+1) = XU(1)
        XT(L+1) = XU(2)+XT1
        DZDXO(L+2) = DZDXUR
        DZDXT(L+2) = DZDXUT
   30 CONTINUE
C     ...
C     ... REMAINDER OF AIRFOIL
C     ...
      IF(RTE .GE. 1.E-4) GO TO 40
C       ...
C       ... SHARP T.E.
C       ...
        XUR = 0.0
        ZUR = 0.0
        XUT = 0.0
        ZUT = 0.0
        RRA = TR*((0.5*CR/TR)**2+0.25)
        RTA = TT*((0.5*CT/TT)**2+0.25)
   40 CONTINUE
      IF(RTE .LT. 1.E-4) GO TO 50
C       ...
C       ... BLUNT T.E.
C       ...
        CALL AXZU(RTER, TR, CR, SWP4, XUR, ZUR, DZDXUR,
     1            XMR, ZMR, RRA)
        CALL AXZU(RTET, TT, CT, SWP4, XUT, ZUT, DZDXUT,
     1            XMT, ZMT, RTA)
   50 CONTINUE
C
      DXR = (CR-XU(1)-XUR)/32.0
      DXT = (CT-XU(2)-XUT)/32.0
      CR2 = CR/2.0
      CT2 = CT/2.0+B2*TAN(SWP1/RAD)
      N1  = L+2
      NS  = L+33
      NC  = L+18
      DO 80 I=N1,NS
        XO(I) = XO(I-1)+DXR
        XT(I) = XT(I-1)+DXT
        IF(I .GT. NC) GO TO 60
C         ...
C         ... FWD ARC
C         ...
          SIND       = (CR2-XO(I-1))/RRF
          DZDXO(I+1) = TAN(ASIN(SIND      ))
          SIND       = (CT2-XT(I-1))/RTF
          DZDXT(I+1) = TAN(ASIN(SIND))
   60   CONTINUE
        IF(I .LE. NC) GO TO 70
C         ...
C         ... AFT ARC
C         ...
          SIND       = (CR2-XO(I-1))/RRA
          DZDXO(I+1) = TAN(ASIN(SIND  ))
          SIND       = (CT2-XT(I-1))/RTA
          DZDXT(I+1) = TAN(ASIN(SIND))
   70   CONTINUE
   80 CONTINUE
      DZDXO(NS+1) = 0.0
      DZDXT(NS+1) = 0.0
C     ...
C     ... CONSTRUCT TGA AND ETA
C     ...
      BETA = SQRT(MACH**2-1.0)
      DO 90 I=1,NS
        TGA(I) = (XT(I)-XO(I))/B2
        ETA(I) = TGA(I)/BETA
        IF((ETA(I).GT.0.98) .AND. (ETA(I).LE.1.))ETA(I) = 0.98
   90 CONTINUE
C
  100 CONTINUE
C
  110 FORMAT(4X,'FIN POTENTIAL FLOW METHOD FAILING.  NOSE CAP/',
     1  'WEDGE ANGLE > THAN 15 DEGREES.')
      RETURN
      END
