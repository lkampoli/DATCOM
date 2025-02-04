      SUBROUTINE BASPRS(ISHAPE,XO,BOTLEN,MISDIA,BASDIA,JETDIA,PRAT, 
     1                  FMACH,JMACH,BETAN,TRAT,ALPHA,XB,REOL,
     2                  NFIN,PHIFIN,CPBASE,CABASE,PBASE,TBASE, 
     3                  XSEP,THSEP,BOTDCN,BOTDCM,BOTDCA)
C
C     THIS PROGRAM WILL COMPUTE THE BASE PRESSURE RATIO ON A
C     CYLINDRICAL MISSILE AS A FUNCTION OF SEVERAL PARAMETERS.
C     THE COMPUTED RESULTS ARE BASED ON DATA FROM A PARAMETRIC
C     BASE FLOW ANALYSIS MADE USING A CHAPMAN-KORST COMPONENT
C     FLOW CODE.
C
C  WRITTEN BY A. JENN, MCDONNELL DOUGLAS
C
C     CONSTANTS
      PARAMETER (GAMMA = 1.4, NFINM = 8)
C
C     EXTERNAL CKDAT
C
C     ARRAYS FOR THE CHAPMAN-KORST DATA
      COMMON /CKDATA/ CKPR(5,5,5,5,5), CKFM(5,5,5,5,5), CKJM(5,5,5,5,5),
     1                CKBN(5,5,5,5,5), CKTR(5,5,5,5,5), CKDJ(5,5,5,5,5),
     2                XPR(5,5), XFM(5,5), XJM(5,5), XBN(5,5), XTR(5,5),
     3                XDJ(5,5), PNOM
C
C     ARRAYS FOR THE TABLE LOOK-UP ROUTINE
      COMMON /INTERPP/ NAVALP(5), NAVALF(5), NAVALJ(5), NAVALB(5), 
     1                NAVALT(5), NAVALD(5), NXVALP(5), NXVALF(5),
     2                NXVALJ(5), NXVALB(5), NXVALT(5), NXVALD(5),
     3                IEXTRP(5), NDIM, NXARR
C
      COMMON /REFQN/  SREF,LREF,LATREF,ROUGH,XCG,ZCG,SCALE,BLAYER,RHR
      REAL LREF,LATREF
C
      REAL XLOOK(5), PRMUL(4), FMMUL(4), JMMUL(4), BNMUL(4), TRMUL(4),
     1     DJMUL(4), PRPNOM(4), PRPACT(4), DAOD0W(3), DAOD0L(3),
     2     ALPEXP(3), CZDAT(12), SOD(12), JMACH, MSEP, MISDIA, JETDIA, 
     3     MU, XSEP(NFINM), THSEP(NFINM), PHIFIN(NFINM) 
C
C     BOUNDARY LAYER THICKNESS AT ANGLE OF ATTACK
      DATA DAOD0W /1.00, 0.39, 0.26/
      DATA DAOD0L /1.00, 1.75, 1.93/
      DATA ALPEXP /0.00, 4.00, 8.00/
C
C     SEPARATION LOCATION AS A FUNCTION OF ZUKOWSKI'S CONSTANT
      DATA CZDAT /0.000, 0.050, 0.100, 0.150, 0.200, 0.250, 
     1            0.300, 0.350, 0.400, 0.450, 0.475, 0.500/
      DATA SOD   / 0.00,  0.06,  0.15,  0.25,  0.45,  0.70, 
     1             1.00,  1.45,  2.20,  4.00,  7.00, 10.00/ 
C
C     ASSIGN THE VALUE OF PI
      PI = ACOS(-1.0)
C
C     COMPUTE THE BOATTAIL FINENESS RATIO
      ALOD = BOTLEN / MISDIA
C
C     COMPUTE THE BOATTAIL TERMINAL ANGLE BASED ON SHAPE
      IF (ISHAPE .EQ. 1 .OR. BASDIA .GE. MISDIA) THEN
C
C        CYLINDRICAL BOATTAIL
         BETAM = 0.0
C
      ELSE IF (ISHAPE .EQ. 2) THEN
C
C        CONICAL BOATTAIL
         DR = (MISDIA - BASDIA) / 2.0
         BETAM = ATAN(DR / BOTLEN) * 180.0 / PI
C
      ELSE
C
C        OGIVE BOATTAIL
         DR = (MISDIA - BASDIA) / 2.0
         BIGR = (DR**2 + BOTLEN**2) / (2.0 * DR)
         DEN  = SQRT(BIGR**2 - BOTLEN**2)
         BETAM = ATAN(BOTLEN / DEN) * 180.0 / PI
C
      END IF
C
C     ASSIGN INVARIANT INDEPENDENT VARIABLES TO THE INDEPENDENT
C     VARIABLE LOOKUP ARRAY
      XLOOK(1) = ISHAPE
      XLOOK(3) = ALOD
      XLOOK(4) = BETAM
C
C     INSERT THE NOMINAL JET PRESSURE RATIO IN THE LOOKUP ARRAY
      XLOOK(5) = PNOM
C                     
C     LOOP TO DETERMINE THE NOMINAL BASE PROPERTIES
      DO 10 I = 1, 4
         XLOOK(2) = I
         CALL TABLOK(NDIM,NAVALP,NXARR,NXVALP,XPR,CKPR,XLOOK,PRPNOM(I),
     1               IEXTRP,IERR)
   10 CONTINUE
C                        
C     DETERMINE THE CORRECTION FACTORS FOR BASE PRESSURE RATIO
      XLOOK(5) = PRAT
      DO 20 I = 1, 4
      XLOOK(2) = I
         CALL TABLOK(NDIM,NAVALP,NXARR,NXVALP,XPR,CKPR,XLOOK,PRMUL(I),
     1               IEXTRP,IERR)
   20 CONTINUE
C
C     DETERMINE THE CORRECTION FACTORS FOR FREESTREAM MACH NUMBER
      XLOOK(5) = FMACH
      DO 30 I = 1, 4
      XLOOK(2) = I
         CALL TABLOK(NDIM,NAVALF,NXARR,NXVALF,XFM,CKFM,XLOOK,FMMUL(I),
     1               IEXTRP,IERR)
   30 CONTINUE
C
C     DETERMINE THE CORRECTION FACTORS FOR JET MACH NUMBER
      XLOOK(5) = JMACH - FMACH
      DO 40 I = 1, 4
      XLOOK(2) = I
         CALL TABLOK(NDIM,NAVALJ,NXARR,NXVALJ,XJM,CKJM,XLOOK,JMMUL(I),
     1               IEXTRP,IERR)
   40 CONTINUE
C
C     DETERMINE THE CORRECTION FACTORS FOR JET NOZZLE ANGLE
      XLOOK(5) = BETAN
      DO 50 I = 1, 4
      XLOOK(2) = I
         CALL TABLOK(NDIM,NAVALB,NXARR,NXVALB,XBN,CKBN,XLOOK,BNMUL(I),
     1               IEXTRP,IERR)
   50 CONTINUE
C
C     DETERMINE THE CORRECTION FACTORS JET TEMPERATURE RATIO
      XLOOK(5) = TRAT
      DO 60 I = 1, 4
      XLOOK(2) = I
         CALL TABLOK(NDIM,NAVALT,NXARR,NXVALT,XTR,CKTR,XLOOK,TRMUL(I),
     1               IEXTRP,IERR)
   60 CONTINUE
C
C     DETERMINE THE CORRECTION FACTORS FOR JET DIAMETER RATIO
      XLOOK(5) = JETDIA / BASDIA
      DO 70 I = 1, 4
      XLOOK(2) = I
         CALL TABLOK(NDIM,NAVALD,NXARR,NXVALD,XDJ,CKDJ,XLOOK,DJMUL(I),
     1               IEXTRP,IERR)
   70 CONTINUE
C
C     CALCULATE THE BASE FLOW PROPERTIES AT THE ACTUAL CONDITIONS
      DO 80 I = 1, 4
         PRPACT(I) = PRMUL(I) * FMMUL(I) * JMMUL(I) * BNMUL(I) * 
     1               TRMUL(I) * DJMUL(I) / PRPNOM(I)**5
   80 CONTINUE
C
C     ASSIGN PROPERTIES TO THEIR CORRESPONDING NAMES
      TBASE = PRPACT(1)
      PBASE = PRPACT(2)
      PSEP  = PRPACT(3)
      MSEP  = PRPACT(4)
C
C     DETERMINE THE BASE PRESSURE COEFFICIENT
      CPBASE = 2.0 / (GAMMA * FMACH**2) * (PBASE - 1)
C
C     DETERMINE THE BASE DRAG COEFFICIENT
      CABASE = -CPBASE * (BASDIA**2 - JETDIA**2) / MISDIA**2
C
C     CONVERT BASE TEMPERATURE RATIO TO STATIC TEMPERATURES
      TBASE = TBASE * (1.0 + (GAMMA - 1.0) / 2.0 * FMACH**2)
C
C     COMPUTE THE BOUNDARY LAYER THICKNESS AT THE BOATTAIL BEGINNING
C     ASSUMING TURBULENT BOUNDARY LAYER AT ZERO ANGLE OF ATTACK
      REB = REOL * XB
      DELA0 = 0.37 * XB / REB**0.2
C
C     COMPUTE THE ANGLE OF ATTACK CORRECTIONS TO THE B.L. THICKNESS
      CALL TABLOK(1,3,3,3,ALPEXP,DAOD0W,ALPHA,DRATW,0,IERR)
      CALL TABLOK(1,3,3,3,ALPEXP,DAOD0L,ALPHA,DRATL,0,IERR)
C
C     COMPUTE THE WINDWARD AND LEESIDE BOUNDARY LAYER THICKNESSES
      DELW = DELA0 * DRATW
      DELL = DELA0 * DRATL
C
C     COMPUTE ZUKOWSKI'S CONSTANT FROM SEPARATION PRESSURE AND 
C     MACH NUMBER
      CZUK = (PSEP - 1.0) / MSEP
      IF (CZUK .LT. 0.0) CZUK = 0.0
C
C     COMPUTE SEPARATION LOCATION IN TERMS OF BOUNDARY LAYER THICKNESS
      CALL TABLOK(1,12,12,12,CZDAT,SOD,CZUK,XSODEL,3,IERR)
C
C     COMPUTE THE WINDWARD AND LEESIDE SEPARATION LOCATIONS
      XSW = XSODEL * DELW
      XSL = XSODEL * DELL
C
C     LIMIT SEPARATION LOCATIONS TO BEGINNING OF BOATTAIL
      IF (XSW .GT. BOTLEN) XSW = BOTLEN
      IF (XSL .GT. BOTLEN) XSL = BOTLEN
C
C     CALCULATE THE MACH ANGLE IN DEGREES
      MU = ASIN(1.0 / FMACH) * 180.0 / PI
C
C     COMPUTE THE WINDWARD AND LEEWARD MACH CONE ANGLES
      THETW = MU - ALPHA
      THETL = MU + ALPHA
C
C     BEGIN LOOP TO CALCULATE SEPARATION LOCATION AND MACH CONE ANGLE
C     AT SPECIFIED CIRCUMFERENTIAL LOCATIONS AROUND THE BOATTAIL.
      DXS   = XSL - XSW
      DTHET = THETL - THETW 
      DO 90 IFIN = 1, NFIN
         PHI  = PHIFIN(IFIN) * PI / 180.0
         IF (PHI .GT. PI) PHI = 2.0 * PI - PHI
         FPHI = 0.5 * (1.0 - COS(PHI))
         XSEP(IFIN)  = XB + BOTLEN - XSL + DXS * FPHI
         THSEP(IFIN) = THETL - DTHET * FPHI
   90 CONTINUE
C
C     COMPUTE THE ZERO ANGLE OF ATTACK SEPARATION LOCATION
      XSEPA0 = XSODEL * DELA0
      IF (XSEPA0 .GT. BOTLEN) XSEPA0 = BOTLEN
C
C     CALCULATE BOATTAIL NORMAL FORCE AND MOMENT INCREMENT
      CALL BOTCNM(ISHAPE,SREF,LREF,XCG,XO,XB,FMACH,ALPHA,BOTLEN,
     1            MISDIA,BASDIA,XSEPA0,BOTDCN,BOTDCM)
C
C     CALCULATE BOATTAIL AXIAL FORCE INCREMENT
      CALL BOTCA(ISHAPE,SREF,BOTLEN,MISDIA,BASDIA,FMACH, 
     1           XSEPA0,PBASE,BOTDCA)
C
      RETURN
      END
