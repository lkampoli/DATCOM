      SUBROUTINE MAJERR(GO,IWARN)
C      
C***  SUBROUTINE TO DETERMINE IF ANY MAJOR ERRORS CAN BE DETECTED
C***  BY LOOKING AT THE NAMELISTS READ
C      
      COMMON /PARTF/ PARTS(19)
      LOGICAL PARTS
      COMMON /LOGIC/ LDMPCS,LDAMP,LBUILD,LNACA,LDERDG,
     1               LDERRD,LPART,LNAME,LPLOT,
     2               LFLT,LREFQ,LAXIS,LFIN1,LFIN2,LFIN3,LFIN4,
     3               LDEFL,LTRIM,LDIMIN,LDIMFT,LDIMCM,LDIMM,LELLB,
     4               LINLET,LEXPR,LICRMT,LSPIN,LARBOD
C      
      LOGICAL        LDMPCS,LDAMP,LBUILD,LNACA,LDERDG,
     1               LDERRD,LPART,LNAME,LPLOT,
     2               LFLT,LREFQ,LAXIS,LFIN1,LFIN2,LFIN3,LFIN4,
     3               LDEFL,LTRIM,LDIMIN,LDIMFT,LDIMCM,LDIMM,LELLB,
     4               LINLET,LEXPR,LICRMT,LSPIN,LARBOD
C      
      COMMON /FLC/    NALPHA,ALPHA(20),BETA,PHI,NMACH,MACH(20),
     1                ALT(20),REN(20),VINF(20),TINF(20),PINF(20)
      REAL NALPHA,NMACH,MACH
      COMMON /REFQN/  SREF,LREF,LATREF,ROUGH,XCG,ZCG,SCALE,BLAYER,RHR
      REAL LREF,LATREF
      COMMON /ABODIN/ NX,XO,X(50),R(50),TNOSE,LNOSE,DNOSE,BNOSE,
     1                TRUNC,LCENTR,DCENTR,TAFT,LAFT,DAFT,
     2                POWER,DISCON(20),ELLIP(50),H(50),
     3                ENOSE,ECENTR,EAFT,DEXIT,BASE,BETAN,JMACH(20),
     4                PRAT(20),TRAT(20),PROTUB,NPROT,PTYPE(20),
     5                XPROT(20),NLOC(20),BLDMEM(20),BLDTYP(100),
     6                LPROT(100),WPROT(100),HPROT(100),OPROT(100)
      REAL NX,LNOSE,LCENTR,LAFT,JMACH,NPROT,NLOC,LPROT
C      
      LOGICAL TRUNC,BASE,PROTUB
C
      COMMON /INLETN/ NIN,INTYPE,XINLT,XDIV,HDIV,LDIV,PHIROL(20),
     1                XI(5),HI(5),WI(5),ICOVER,RAMANG,IADD,MFR(20)
C      
      REAL NIN,INTYPE,LDIV,MFR
      LOGICAL ICOVER,IADD
C
      COMMON /CONST/  PI,RAD,UNUSED,KAND
      COMMON /FSET1/  F1IN(399)
      COMMON /FSET2/  F2IN(399)
      COMMON /FSET3/  F3IN(399)
      COMMON /FSET4/  F4IN(399)
C      
      DIMENSION TMACH(20),TREN(20),TTINF(20),TPINF(20),IORDER(20),
     1 TALT(20),TVINF(20),TJMACH(20),TPRAT(20),TTRAT(20),TMFR(20)
C      
      LOGICAL GO,IWARN,LMACH,LVINF,LBODY
C      
C***  SORT ALPHA ARRAY IN ASCENDING ORDER
C***  SORT  REN, TINF AND PINF IN ASCENDING MACH/VINF ORDER
C      
      NM=NMACH+0.5
      NA=NALPHA+0.5
      LMACH=.FALSE.
      LVINF=.FALSE.
C      
      IF(NA .GT. 0)CALL SORT(NA,ALPHA,IORDER)
C      
      DO 1000 I=1,NM
         IF(MACH(I) .NE. UNUSED)LMACH=.TRUE.
         IF(VINF(I) .NE. UNUSED)LVINF=.TRUE.
         TMACH(I)=MACH(I)
         TREN(I)=REN(I)
         TVINF(I)=VINF(I)
         TTINF(I)=TINF(I)
         TPINF(I)=PINF(I)
         TJMACH(I)=JMACH(I)
         TPRAT(I)=PRAT(I)
         TTRAT(I)=TRAT(I)
         TMFR(I)=MFR(I)
         TALT(I)=ALT(I)
 1000 CONTINUE
C      
C***  IF BOTH MACH AND VINF INPUT, DO NOT SORT
C      
      IF(NM .LE. 1)GO TO 1020
      IF(LMACH .AND. LVINF)GO TO 1020
      IF(.NOT.(LMACH .OR. LVINF))GO TO 1020
C      
      IF(LMACH)CALL SORT(NM,MACH,IORDER)
      IF(LVINF)CALL SORT(NM,VINF,IORDER)
C      
      DO 1010 J=1,NM
         I=IORDER(J)
         REN(J)=TREN(I)
         TINF(J)=TTINF(I)
         PINF(J)=TPINF(I)
         JMACH(J)=TJMACH(I)
         PRAT(J)=TPRAT(I)
         TRAT(J)=TTRAT(I)
         MFR(J)=TMFR(I)
         ALT(J)=TALT(I)
 1010 CONTINUE
C      
C***  CHECK FOR FLIGHT CONDITION INPUTS
C      
 1020 IF(.NOT.LFLT .AND. PARTS(18))WRITE(6,1160)
      IF(.NOT.LFLT)GO=.FALSE.
      IF(PHI .EQ. UNUSED)PHI=0.
      IF(BETA .EQ. UNUSED)BETA=0.
C      
C***  VERIFY XO DEFINITION
C      
      IF(XO .EQ. UNUSED)XO=0.
      IF(NX .GT. 1. .AND. XO .EQ. UNUSED)XO=X(1)
C      
C***  VERIFY C. G. DEFINITION
C      
      IF(XCG .EQ. UNUSED)XCG=0.
      IF(ZCG .EQ. UNUSED)ZCG=0.
C      
C***  FATAL ERROR IF NMACH .LE. UNUSED OR NALPHA .LE. 1.
C***  OR MACH .LE. 0.0 AND VINF .LE. 0.0
C      
      IF(LFLT .AND. NMACH .LE. UNUSED .AND. PARTS(18))WRITE(6,1190)
      IF(LFLT .AND. NMACH .LE. UNUSED)GO=.FALSE.
      IF(LFLT .AND. NALPHA .LE. 1. .AND. PARTS(18))WRITE(6,1200)
      IF(LFLT .AND. NALPHA .LE. UNUSED)ALPHA(1)=0.
      IF(LFLT .AND. NALPHA .LE. UNUSED)ALPHA(2)=1.
      IF(LFLT .AND. NALPHA .LE. UNUSED)NALPHA=2.
      IF(LFLT .AND. NALPHA .EQ. 1.)ALPHA(2)=ALPHA(1)+1.
      IF(LFLT .AND. NALPHA .EQ. 1.)NALPHA=2.
      IF(NM .LT. 1)NM=1
      DO 1030 I=1,NM
C      
C***  CHECK FOR INPUT SPEED REFERENCE
C      
      IF(LFLT .AND. MACH(I) .LE. UNUSED .AND. VINF(I) .LE. UNUSED
     1 .AND. PARTS(18))WRITE(6,1210)I
      IF(LFLT .AND. MACH(I) .LE. UNUSED .AND. VINF(I) .LE. UNUSED)
     1 GO=.FALSE.
C      
C***  IF NEED REYNOLDS NUMBER, SET ALT=0.0
C      
      IF(LFLT .AND. ALT(I) .EQ. UNUSED .AND. PINF(I) .EQ. UNUSED
     1   .AND. REN(I) .EQ. UNUSED .AND. PARTS(18))WRITE(6,1250)I
      IF(LFLT .AND. ALT(I) .EQ. UNUSED .AND. PINF(I) .EQ. UNUSED
     1   .AND. REN(I) .EQ. UNUSED) ALT(I)=0.0
C      
 1030 CONTINUE
C      
C***  CHECK FOR REFERENCE QUANTITIES INPUT, PRINT ERROR IF ABSENT
C      
      IF(.NOT.LREFQ .AND. PARTS(18))WRITE(6,1170)
      IF(SREF .GE. 0. .AND. SREF .LE. UNUSED .AND. LREFQ
     1   .AND. PARTS(18))WRITE(6,1220)
      IF(LREF .GE. 0. .AND. LREF .LE. UNUSED .AND. LREFQ
     1   .AND. PARTS(18))WRITE(6,1230)
      IF(SCALE .LE. UNUSED)SCALE=1.
C      
C***  CHECK FOR BODY AND/OR FINS INPUT
C      
      LBODY=LAXIS .OR. LELLB .OR. LARBOD
C      
      IF(.NOT.LBODY.AND..NOT.(LFIN1.OR.LFIN2.OR.LFIN3.OR.LFIN4)
     1 .AND. PARTS(18))WRITE(6,1180)
      IF(.NOT.LBODY.AND..NOT.(LFIN1.OR.LFIN2.OR.LFIN3.OR.LFIN4))
     1 GO=.FALSE.
C      
C***  CHECK FOR PROPER INPUTS FOR FIN PARAMETERS
C      
C .. NUMBER OF FIN PANELS MUST BE FROM 1 TO 4
C      
      NPANEL=F1IN(112)+.5
      NSET=1
      IF(LFIN1.AND.NPANEL.LT.1)F1IN(112)=4.
      IF(LFIN1.AND.NPANEL.LT.1 .AND. PARTS(18))
     1   WRITE(6,1400)NSET,F1IN(112)
      IF(LFIN1.AND.NPANEL.GT.8)F1IN(112)=8.
      IF(LFIN1.AND.NPANEL.GT.8 .AND. PARTS(18))
     1   WRITE(6,1400)NSET,F1IN(112)
      NPANEL=F2IN(112)+.5
      NSET=2
      IF(LFIN2.AND.NPANEL.LT.1)F2IN(112)=4.
      IF(LFIN2.AND.NPANEL.LT.1 .AND. PARTS(18))
     1   WRITE(6,1400)NSET,F2IN(112)
      IF(LFIN2.AND.NPANEL.GT.8)F2IN(112)=8.
      IF(LFIN2.AND.NPANEL.GT.8 .AND. PARTS(18))
     1   WRITE(6,1400)NSET,F2IN(112)
      NPANEL=F3IN(112)+.5
      NSET=3
      IF(LFIN3.AND.NPANEL.LT.1)F3IN(112)=4.
      IF(LFIN3.AND.NPANEL.LT.1 .AND. PARTS(18))
     1   WRITE(6,1400)NSET,F3IN(112)
      IF(LFIN3.AND.NPANEL.GT.8)F3IN(112)=8.
      IF(LFIN3.AND.NPANEL.GT.8 .AND. PARTS(18))
     1   WRITE(6,1400)NSET,F3IN(112)
      NPANEL=F4IN(112)+.5
      NSET=4
      IF(LFIN4.AND.NPANEL.LT.1)F4IN(112)=4.
      IF(LFIN4.AND.NPANEL.LT.1 .AND. PARTS(18))
     1   WRITE(6,1400)NSET,F4IN(112)
      IF(LFIN4.AND.NPANEL.GT.8)F4IN(112)=8.
      IF(LFIN4.AND.NPANEL.GT.8 .AND. PARTS(18))
     1   WRITE(6,1400)NSET,F4IN(112)
      DO 1050 I=1,10
         IF(F1IN(I+11).LT.0. .OR. F1IN(I+11).GT.1.)GO TO 1040
         IF(F1IN(I+21).LT.0. .OR. F1IN(I+21).GT.1.)GO TO 1040
         IF(F1IN(I+31).LT.0. .OR. F1IN(I+31).GT.1.)GO TO 1040
         IF(F1IN(I+41).LT.0. .OR. F1IN(I+41).GT.1.)GO TO 1040
         IF(F1IN(I+61).LT.0. .OR. F1IN(I+61).GT.1.)GO TO 1040
         IF(F1IN(I+71).LT.0. .OR. F1IN(I+71).GT.1.)GO TO 1040
         IF(F2IN(I+11).LT.0. .OR. F2IN(I+11).GT.1.)GO TO 1040
         IF(F2IN(I+21).LT.0. .OR. F2IN(I+21).GT.1.)GO TO 1040
         IF(F2IN(I+31).LT.0. .OR. F2IN(I+31).GT.1.)GO TO 1040
         IF(F2IN(I+41).LT.0. .OR. F2IN(I+41).GT.1.)GO TO 1040
         IF(F2IN(I+61).LT.0. .OR. F2IN(I+61).GT.1.)GO TO 1040
         IF(F2IN(I+71).LT.0. .OR. F2IN(I+71).GT.1.)GO TO 1040
         IF(F3IN(I+11).LT.0. .OR. F3IN(I+11).GT.1.)GO TO 1040
         IF(F3IN(I+21).LT.0. .OR. F3IN(I+21).GT.1.)GO TO 1040
         IF(F3IN(I+31).LT.0. .OR. F3IN(I+31).GT.1.)GO TO 1040
         IF(F3IN(I+41).LT.0. .OR. F3IN(I+41).GT.1.)GO TO 1040
         IF(F3IN(I+61).LT.0. .OR. F3IN(I+61).GT.1.)GO TO 1040
         IF(F3IN(I+71).LT.0. .OR. F3IN(I+71).GT.1.)GO TO 1040
         IF(F4IN(I+11).LT.0. .OR. F4IN(I+11).GT.1.)GO TO 1040
         IF(F4IN(I+21).LT.0. .OR. F4IN(I+21).GT.1.)GO TO 1040
         IF(F4IN(I+31).LT.0. .OR. F4IN(I+31).GT.1.)GO TO 1040
         IF(F4IN(I+41).LT.0. .OR. F4IN(I+41).GT.1.)GO TO 1040
         IF(F4IN(I+61).LT.0. .OR. F4IN(I+61).GT.1.)GO TO 1040
         IF(F4IN(I+71).LT.0. .OR. F4IN(I+71).GT.1.)GO TO 1040
C      
         GO TO 1050
C      
 1040    CONTINUE
C      
         IF(PARTS(18))WRITE(6,1390)
         GO=.FALSE.
         GO TO 1060
C      
 1050 CONTINUE
C      
 1060 CONTINUE
C      
      IF((NX .EQ. 1. .OR. NX .LE. 0.) .AND. PARTS(18))WRITE(6,1240)
      IF(NX .EQ. UNUSED)GO TO 1080
C      
C***  CHECK FOR CONFLICT IN BODY INPUTS
C      
      LL=2
      LU=(NX-1.)+.5
      DO 1070 L=LL,LU
      IF(NX .GT. 1. .AND. (X(L) .EQ. UNUSED .OR. R(L) .EQ. UNUSED)
     1 .AND. PARTS(18))WRITE(6,1380)
      IF(NX .GT. 1. .AND. (X(L) .EQ. UNUSED .OR. R(L) .EQ. UNUSED))
     1 GO=.FALSE.
      IF(.NOT. GO)GO TO 1080
 1070 CONTINUE
C      
C ... CHECK FOR BODY SECTIONS DEFINED BUT DIAMETERS ARE NOT
C      
 1080 IF(LNOSE .GT. UNUSED .AND. DNOSE .LE. UNUSED .AND. PARTS(18))
     1 WRITE(6,1340)
      IF(LNOSE .GT. UNUSED .AND. DNOSE .LE. UNUSED)DNOSE=1.
      IF(BNOSE .LE. UNUSED .AND. LNOSE .EQ. DNOSE/2. .AND. PARTS(18))
     1 WRITE(6,1370)
      IF(BNOSE .LE. UNUSED .AND. LNOSE .EQ. DNOSE/2.)BNOSE=LNOSE
      IF(LCENTR .GT. UNUSED .AND. DCENTR .EQ. UNUSED .AND. PARTS(18))
     1 WRITE(6,1350)
      IF(LCENTR .GT. UNUSED .AND. DCENTR .EQ. UNUSED)DCENTR=DNOSE
C      
C  NUMBER OF POINTS MUST NOT BE GREATER THAN 50
C      
      IF(NX .GT. 50. .AND. PARTS(18))WRITE(6,1310)
      IF(NX .GT. 50.)NX=50.
C      
C***  WARNING MESSAGE IF SUBSONIC/TRANSONIC AND BASE-JET PLUME
C***         CALCULATIONS DESIRED
C
      IF(BASE.AND.(MACH(1).LT.1.2).AND.PARTS(18))WRITE(6,1410)
C
C***  WARNING MESSAGE IF INLET ADDITIVE DRAG CALCULATIONS
C***          DESIRED AND MACH NUMBERS ARE LESS THAN 1.0
      IF(LINLET.AND.IADD.AND.(MACH(1).LT.1.0))WRITE(6,1450)
C      
C*** PART CONTROL CARD TURNS ON ALL PARTIAL OUTPUT
C      
      IF(.NOT. LPART)GO TO 1110
      DO 1100 I=1,16
         PARTS(I)=.TRUE.
 1100 CONTINUE
C      
 1110 CONTINUE
C      
C***  WRITE WARNING MESSAGE IF DIMENSIONAL UNITS ARE CHANGED
C      
      IF(IWARN .AND. PARTS(18))WRITE(6,1260)
C      
      IF(BLAYER .LT. 1.)BLAYER=0.
      IF(BLAYER .GE. 1.)BLAYER=1.
      IF(BLAYER .EQ. 0. .AND. PARTS(18))WRITE(6,1320)
      IF(BLAYER .NE. 0. .AND. PARTS(18))WRITE(6,1330)
C      
      IF(LDIMIN .AND. PARTS(18))WRITE(6,1270)SCALE
      IF(LDIMCM .AND. PARTS(18))WRITE(6,1280)SCALE
      IF(LDIMM  .AND. PARTS(18))WRITE(6,1290)SCALE
      IF(.NOT.(LDIMIN .OR. LDIMCM .OR. LDIMM) .AND. PARTS(18))
     1 WRITE(6,1300)SCALE
      IF(SCALE .NE. UNUSED .AND. SCALE .NE. 1. .AND. PARTS(18))
     1 WRITE(6,1360)
C      
      IF(LFIN1 .AND. F1IN(102).EQ.UNUSED)F1IN(102)=0.
      IF(LFIN2 .AND. F2IN(102).EQ.UNUSED)F2IN(102)=0.
      IF(LFIN3 .AND. F3IN(102).EQ.UNUSED)F3IN(102)=0.
      IF(LFIN4 .AND. F4IN(102).EQ.UNUSED)F4IN(102)=0.

C      
      DO 1130 I=1,4
         DELPH1=360./F1IN(112)
         DELPH2=360./F2IN(112)
         DELPH3=360./F3IN(112)
         DELPH4=360./F4IN(112)
         DO 1120 J=1,8
            IF(LFIN1 .AND. F1IN(J+381) .EQ. UNUSED
     1         .AND. FLOAT(J) .LE. F1IN(112)+.1)
     2         F1IN(J+381)=FLOAT(J-1)*DELPH1
            IF(LFIN2 .AND. F2IN(J+381) .EQ. UNUSED
     1         .AND. FLOAT(J) .LE. F2IN(112)+.1)
     2         F2IN(J+381)=FLOAT(J-1)*DELPH2
            IF(LFIN3 .AND. F3IN(J+381) .EQ. UNUSED
     1         .AND. FLOAT(J) .LE. F3IN(112)+.1)
     2         F3IN(J+381)=FLOAT(J-1)*DELPH3
            IF(LFIN4 .AND. F4IN(J+381) .EQ. UNUSED
     1         .AND. FLOAT(J) .LE. F4IN(112)+.1)
     2         F4IN(J+381)=FLOAT(J-1)*DELPH4
 1120    CONTINUE
 1130 CONTINUE
C      
 1160 FORMAT(4X,'* FATAL ERROR * FLIGHT CONDITION INPUTS FROM NAMELIST',
     1 ' FLTCON MISSING')
 1170 FORMAT(4X,'* WARNING * REFERENCE QUANTITIES FROM NAMELIST',
     1 ' REFQ MISSING')
 1180 FORMAT(4X,'* FATAL ERROR * NEITHER A BODY NOR A SET OF',
     1 ' FINS HAVE BEEN DEFINED')
 1190 FORMAT(4X,'* FATAL ERROR * THE NUMBER OF MACH NUMBERS IS LESS',
     1 ' THAN ONE')
 1200 FORMAT(4X,'* WARNING * THE NUMBER OF ANGLES OF ATTACK IS LESS',
     1 ' THAN TWO')
 1210 FORMAT(4X,'* FATAL ERROR * THE SPEED SPECIFIED IS TOO SMALL',
     1 ' FOR INPUT MACH ',I2)
 1220 FORMAT(4X,'* WARNING * THE REFERENCE AREA IS UNSPECIFIED,',
     1 ' DEFAULT VALUE ASSUMED')
 1230 FORMAT(4X,'* WARNING * THE REFERENCE LENGTH IS UNSPECIFIED,',
     1 ' DEFAULT VALUE ASSUMED')
 1240 FORMAT(4X,'* WARNING * THE NUMBER OF BODY INPUT STATIONS,',
     1 ' (NX), IS LESS THAN TWO')
 1250 FORMAT(4X,'* WARNING * THE REYNOLDS NUMBER IS NOT DEFINED,',
     1 ' FOR INPUT SPEED',I3,/,4X,
     2 12X,'SEA LEVEL ALTITUDE ASSUMED')
 1260 FORMAT(4X,'* WARNING * THIS IS A SAVE CASE AND THE UNITS,',
     1 ' ARE BEING REDEFINED')
 1270 FORMAT(4X,'THE INPUT UNITS ARE IN INCHES, THE SCALE FACTOR IS ',
     1 F8.4)
 1280 FORMAT(4X,'THE INPUT UNITS ARE IN CENTIMETERS, ',
     1 'THE SCALE FACTOR IS ',F8.4)
 1290 FORMAT(4X,'THE INPUT UNITS ARE IN METERS, THE SCALE FACTOR IS ',
     1 F8.4)
 1300 FORMAT(4X,'THE INPUT UNITS ARE IN FEET, THE SCALE FACTOR IS ',
     1 F8.4)
 1310 FORMAT(4X,'* WARNING * THE NUMBER OF INPUT POINTS IN NAMELIST',
     1 ' AXIBOD IS GREATER THAN 50')
 1320 FORMAT(4X,'THE BOUNDARY LAYER IS ASSUMED TO BE TURBULENT')
 1330 FORMAT(4X,'THE BOUNDARY LAYER IS ASSUMED TO DEVELOP NATURALLY')
 1340 FORMAT(4X,'* WARNING * NOSE SECTION DEFINED BUT BASE',
     1 ' DIAMETER ZERO OR NOT INPUT',/,4X,12X,'VALUE OF 1.0 ASSUMED')
 1350 FORMAT(4X,'* WARNING * CENTER SECTION DEFINED BUT BASE',
     1 ' DIAMETER NOT INPUT',/,4X,12X,'CYLINDRICAL SECTION ASSUMED')
 1360 FORMAT(4X,'ALL GEOMETRIC INPUTS',
     1 ' WILL BE MULTIPLIED BY THE SCALE FACTOR')
 1370 FORMAT(4X,'* WARNING * NOSE LENGTH EQUALS BASE RADIUS,',
     1       ' SPHERICAL NOSE BLUNTNESS ASSUMED')
 1380 FORMAT(4X,'* FATAL ERROR * BODY GEOMETRY INPUT CONFLICT')
 1390 FORMAT(4X,'* FATAL ERROR * FIN GEOMETRY INPUTS NOT CONSISTENT')
 1400 FORMAT(4X,'* WARNING * NUMBER OF PANELS IN FIN SET ',I2,
     1 ' CHANGED TO',F3.0)
 1410 FORMAT(4X,'* WARNING * BASE-JET PLUME EFFECTS WILL NOT BE ',/,
     1     12X,'CALCULATED FOR SUBSONIC/TRANSONIC SPEEDS')
 1450 FORMAT(4X,'* WARNING * INLET ADDITIVE DRAG WILL NOT BE ',/,
     1     12X,'CALCULATED FOR MACH NUMBERS LESS THAN 1.0')
      RETURN 
      END
