      SUBROUTINE GEOINL
C
C***********************************************************************
C
C     THIS SUBROUTINE PROCESSES THE INPUT GEOMETRY.  IT DEFINES
C     GEOMETRIC VALUES FOR USE IN OTHER PARTS OF THE PROGRAM.
C
C***********************************************************************
C
C  WRITTEN BY M. MOORE, MCDONNELL DOUGLAS
C  MODIFIED BY K. BURNS, MCDONNELL DOUGLAS
C
      COMMON /INLETN/ NIN,INTYPE,XINLT,XDIV,HDIV,LDIV,PHI(20),
     1                XI(5),HI(5),WI(5),ICOVER,RAMANG,IAD,MFR(20)
      COMMON /ILTGEO/ LENGTH(5),LLNGTH(5),ALPEFF(5),SI(5),
     1                CRXLEN(5),DEQI(5),XMIDI(5),XMIDD(3),
     2                XD(3),HD(3),WD(3),DEQD(3),
     3                RARC(5),DLEN(3),KWA(20),KWB(20),
     4                RA2(20),RB2(20),AREAI(5),AREAD(3)
      COMMON /FLC/    NALPHA,ALPHA(20),BETA,PHIROL,NMACH,MACH(20),
     1                ALT(20),REN(20),FLDUM(60)
      COMMON /REFQN/  SREF,LREF,BREF,ROUGH,XCG,ZCG,SCALE,BLAYER,RHR
      COMMON /CONST/  PI,RAD,UNUSED,KAND
      COMMON /POTENT/ CNP(20),CYP(20),CMP(20),CLNP(20)
      COMMON / MOMEN/ CNMOM(20),CYMOM(20)
      COMMON /LINEAR/ ARA,ARB,CNLIN(20),CMLIN(20),
     1                CYLIN(20),CLNLIN(20)
      COMMON /CFFLOW/ CNCF(20),CYCF(20),
     1                CMCF(20),CLNCF(20)
      COMMON /ILTVSC/ THETAI(5,20),PHIR(20),CRXIWD(5,20),
     1                CRXIHT(5,20),CNNIZP(5,20),
     2                CNNIZN(5,20),CNNIYP(5,20),
     3                CNNIYN(5,20),PANILN(5,5,20),
     4                CNCIRN,FRI,RARX(3),
     5                DLCNAP(3,20),DLCNAN(3,20),
     6                DLCYAP(3,20),DLCYAN(3,20)
      COMMON /DIVVSC/ THETAD(5,20),CRXDWD(3,20),
     1                CRXDHT(3,20),CNNDZP(3,20),
     2                CNNDZN(3,20),CNNDYP(3,20),
     3                CNNDYN(3,20),PANDLN(5,20),FRD
      COMMON /BODVSC/ DLCNBP(5,20),DLCNBN(5,20),
     1                DLCYBP(5,20),DLCYBN(5,20),FRB
C
      REAL NIN,INTYPE,LENGTH,LLNGTH,LREF,LDIV,MACH,KWA,KWB
      REAL NALPHA,NMACH
      LOGICAL ICOVER
      COMMON /GEOBOD/ DUM(32),DMAX,SMAX,LBOD,GDUM(12)
      COMMON /PARTF/ PARTS(19)
      LOGICAL PARTS
      REAL LBOD
      RBOD=DMAX/2.0
C
C***  SCALE THE INPUT GEOMETRY IF REQUIRED
      IF ((SCALE.NE.UNUSED).OR.(SCALE.NE.1.)) THEN
        XINLT=XINLT*SCALE
        XDIV=XDIV*SCALE
        HDIV=HDIV*SCALE
        LDIV=LDIV*SCALE
        DO 5 I=1,5
          XI(I)=XI(I)*SCALE
          HI(I)=HI(I)*SCALE
          WI(I)=WI(I)*SCALE
   5    CONTINUE
      ENDIF
C
C     CALCULATE INLET SPECIFIC GEOMETRY CHARACTERISTICS
C
      IF(INTYPE.NE.3.)THEN
        DO 10 ICRX=1,5
          IF ((HI(ICRX).EQ.0.0).AND.(WI(ICRX).EQ.0.0)) THEN
            IF (ABS(HI(ICRX)-HI(ICRX-1)).GT.
     &        ABS(WI(ICRX)-WI(ICRX-1))) THEN
              SI(ICRX)=PI/2.0
            ELSEIF (ABS(WI(ICRX)-WI(ICRX-1)).GT.
     &        ABS(HI(ICRX)-HI(ICRX-1))) THEN
              SI(ICRX)=0.0
            ELSEIF (ABS(WI(ICRX)-WI(ICRX-1)).EQ.
     &        ABS(HI(ICRX)-HI(ICRX-1))) THEN
              SI(ICRX)=PI/4.0
            ENDIF
          ELSE
            SI(ICRX)=ATAN2(HI(ICRX),WI(ICRX))
          ENDIF
          CRXLEN(ICRX)=SQRT(WI(ICRX)**2.+HI(ICRX)**2.)
   10   CONTINUE
      ENDIF
C
      DO 20 JSEC=1,4
        LENGTH(JSEC)=XI(JSEC+1)-XI(JSEC)
        XMIDI(JSEC)=XI(JSEC)+LENGTH(JSEC)/2.
        ALPEFF(JSEC)=0.0
   20 CONTINUE
C
C *** CALCULATION OF INLET CROSS SECTIONAL AREA CHANGE FROM
C     FIRST INLET SECTION (USED FOR PRESSURE DRAG CALCS)
C
C     FOR 2-D TOP OR SIDE MOUNTED INLETS 
C     (EITHER SURFACE MOUNT OR WITH DIVERTER)
C
      IF (INTYPE.EQ.1. .OR. INTYPE.EQ.2. .AND. HDIV.GE.0.0) THEN
        IF (ICOVER) THEN
          DO 30 ICRX=1,5
            AREAI(ICRX)=HI(ICRX)*WI(ICRX)
            DEQI(ICRX)=SQRT(4*AREAI(ICRX)/PI)
            IF(HDIV .EQ. 0.0)RARC(ICRX) = 0.0
   30     CONTINUE
        ELSE
          AREAI(1) = 0.0
          DEQI(1)  = 0.0
          IF(HDIV .EQ. 0.0)RARC(1) = 0.0
          DO 40 ICRX=2,5
            AREAI(ICRX)=HI(ICRX)*WI(ICRX)-HI(2)*WI(1)
            DEQI(ICRX) = SQRT(4.*HI(ICRX)*WI(ICRX)/PI)
            IF(HDIV .EQ. 0.0)RARC(ICRX) = 0.0
   40     CONTINUE
        ENDIF
      ENDIF
C
C     FOR 2-D TOP MOUNTED SEMI-SUBMERGED INLETS
C
C     THETAB = ANGLE OF ARC WHERE BODY AND INLET INTERSECT
C     AREAS  = AREA OF INLET SUBMERGED INSIDE BODY MOLDLINE
C     RARC   = ANGLE USED TO CALCULATE FORCES ON OVERLAPPING AREAS
C              OF BODY AND INLET (OR DIVERTER IF HDIV .GT. 0)
C
      IF (INTYPE.EQ.1. .AND. HDIV.LT.0.0) THEN
        IF (ICOVER) THEN
          DO 41 ICRX=1,5
            THETAB     = 2.0*ASIN(WI(ICRX)/2./RBOD)
            AREAS      = 0.5*RBOD**2*(THETAB - SIN(THETAB))
            AREAI(ICRX)= HI(ICRX)*WI(ICRX) - AREAS
            DEQI(ICRX) = SQRT(4.*AREAI(ICRX)/PI)
            RARC(ICRX) = 90./RAD - THETAB/2.
   41     CONTINUE
        ELSE
          AREAI(1) = 0.0
          DEQI(1)  = 0.0
          RARC(1)  = 0.0
          THETAB   = 2.0*ASIN(WI(2)/2./RBOD)
          AREAS    = 0.5*RBOD**2*(THETAB - SIN(THETAB))
          AREA2    = WI(1)*HI(2) - AREAS
          DO 42 ICRX=2,5
            THETAB     = 2.0*ASIN(WI(ICRX)/2./RBOD)
            AREAS      = 0.5*RBOD**2*(THETAB - SIN(THETAB))
            AREAI(ICRX)= HI(ICRX)*WI(ICRX) - AREA2 - AREAS
            DEQI(ICRX) = SQRT(4.*(HI(ICRX)*WI(ICRX)-AREAS)/PI)
            RARC(ICRX) = 90./RAD - THETAB/2.
   42     CONTINUE
        ENDIF
      ENDIF
C
C     FOR 2-D SIDE MOUNTED SEMI-SUBMERGED INLETS
C
      IF (INTYPE.EQ.2. .AND. HDIV.LT.0.0) THEN
        IF (ICOVER) THEN
          DO 43 ICRX=1,5
            THETAB     = 2.0*ASIN(HI(ICRX)/2./RBOD)
            AREAS      = 0.5*RBOD**2*(THETAB - SIN(THETAB))
            AREAI(ICRX)= HI(ICRX)*WI(ICRX) - AREAS
            DEQI(ICRX) = SQRT(4.*AREAI(ICRX)/PI)
            RARC(ICRX) = 90./RAD - THETAB/2.
   43     CONTINUE
        ELSE
          AREAI(1) = 0.0
          DEQI(1)  = 0.0
          RARC(1)  = 0.0
          THETAB   = 2.0*ASIN(HI(2)/2./RBOD)
          AREAS    = 0.5*RBOD**2*(THETAB - SIN(THETAB))
          AREA2    = WI(1)*HI(2) - AREAS
          DO 44 ICRX=2,5
            THETAB     = 2.0*ASIN(HI(ICRX)/2./RBOD)
            AREAS      = 0.5*RBOD**2*(THETAB - SIN(THETAB))
            AREAI(ICRX)= HI(ICRX)*WI(ICRX) - AREA2 - AREAS
            DEQI(ICRX) = SQRT(4.*(HI(ICRX)*WI(ICRX)-AREAS)/PI)
            RARC(ICRX) = 90./RAD - THETAB/2.
   44     CONTINUE
        ENDIF
      ENDIF
C
C     FOR AXISYMMETRIC INLETS WITH DIVERTERS OR SURFACE MOUNTED
C
      IF (INTYPE.EQ.3. .AND. HDIV.GE.0.0) THEN
        IF (ICOVER) THEN
          DO 50 ICRX=1,5
            AREAI(ICRX) = PI/4.*WI(ICRX)**2.
            DEQI(ICRX)  = WI(ICRX)
   50     CONTINUE
        ELSE
          AREAI(1) = 0.0
          DEQI(1)  = 0.0
          DO 60 ICRX=2,5
            AREAI(ICRX) = PI/4.*WI(ICRX)**2.-PI/4.*WI(2)**2.
            DEQI(ICRX)  = WI(ICRX)
   60     CONTINUE
        ENDIF
      ENDIF
C
C     FOR AXISYMMETRIC INLETS THAT ARE SEMI-SUBMERGED
C
C     THETAB = ANGLE FROM BODY CENTER TO BODY/INLET INTERSECTION PTS
C     THETI  = ANGLE FROM INLET CENTER TO INTERSECTION PTS
C     XINT   = DISTANCE TO INTERSECTION POINT ALONG A LINE CONNECTING
C              THE BODY CENTER TO INLET CENTER
C     YINT   = VERTICAL DISTANCE TO INTERSECTION POINT NORMAL TO A LINE
C              CONNECTING THE BODY CENTER TO INLET CENTER
C
      IF (INTYPE.EQ.3. .AND. HDIV.LT.0.0) THEN
        IF (ICOVER) THEN
          DO 61 ICRX=1,5
            RINL        = WI(ICRX)/2.
            XINT        = (RBOD**2 + RBOD*RINL + RBOD*HDIV + 
     1                    RINL*HDIV + HDIV**2/2.)/ (RBOD + RINL + HDIV)
            YINT        = SQRT(RBOD**2 - XINT**2)
            THETAB      = 2.*ATAN(YINT/XINT)
            THETI       = 2.*ATAN(YINT/(RBOD + RINL + HDIV - XINT))
            IF(THETI .LT. 0.0)THETI = THETI + 2.0*PI
            AREAS       = 0.5*RBOD**2*(THETAB - SIN(THETAB)) +
     1                    0.5*RINL**2*(THETI  - SIN(THETI))
            AREAI(ICRX) = PI/4.*WI(ICRX)**2. - AREAS
            DEQI(ICRX)  = SQRT(4.*(AREAI(ICRX) - AREAS)/PI)
            RARC(ICRX) = 90./RAD - THETAB/2.
   61     CONTINUE
        ELSE
          AREAI(1) = 0.0
          DEQI(1)  = 0.0
          RARC(1)  = 0.0
          DO 62 ICRX=2,5
            RINL        = WI(ICRX)/2.
            XINT        = (RBOD**2 + RBOD*RINL + RBOD*HDIV + 
     1                    RINL*HDIV + HDIV**2/2.)/ (RBOD + RINL + HDIV)
            YINT        = SQRT(RBOD**2 - XINT**2)
            THETAB      = 2.*ATAN(YINT/XINT)
            THETI       = 2.*ATAN(YINT/(RBOD + RINL + HDIV - XINT))
            IF(THETI .LT. 0.0)THETI = THETI + 2.0*PI
            AREAS       = 0.5*RBOD**2*(THETAB - SIN(THETAB)) +
     1                    0.5*RINL**2*(THETI  - SIN(THETI))
            AREAI(ICRX) = PI/4.*WI(ICRX)**2.-PI/4.*WI(2)**2.
            DEQI(ICRX)  = SQRT(4.*(PI*RINL**2 - AREAS)/PI)
            RARC(ICRX) = 90./RAD - THETAB/2.
   62     CONTINUE
        ENDIF
      ENDIF
C
C     VEHICLE SPAN CALCULATION USED TO DETERMINE WING-BODY CARRY
C     OVER AND ASPECT RATIO EFFECTS
C
      IF (INTYPE.EQ.1.)THEN
        SPAN = RBOD + HDIV + HI(3)
      ELSE
        SPAN = RBOD + HDIV + WI(3)
      ENDIF
      NUMI=NIN+0.5
      DO 70 INLET=1,NUMI
        KWA(INLET) = (1. + ABS(SIN(PHI(INLET)/RAD))*RBOD/SPAN)**2.
        KWB(INLET) = (1. + ABS(COS(PHI(INLET)/RAD))*RBOD/SPAN)**2.
        IF (INTYPE.EQ.1.) THEN
          RA2(INLET) = SPAN*ABS(SIN(PHI(INLET)/RAD)) +
     1                                ABS(COS(PHI(INLET)/RAD))*WI(3)/2.
          RB2(INLET) = SPAN*ABS(COS(PHI(INLET)/RAD)) +
     1                                ABS(SIN(PHI(INLET)/RAD))*WI(3)/2.
        ELSEIF (INTYPE.EQ.2.) THEN
          RA2(INLET) = SPAN*ABS(SIN(PHI(INLET)/RAD)) +
     1                                ABS(COS(PHI(INLET)/RAD))*HI(3)/2.
          RB2(INLET) = SPAN*ABS(COS(PHI(INLET)/RAD)) +
     1                                ABS(SIN(PHI(INLET)/RAD))*HI(3)/2.
        ELSEIF (INTYPE.EQ.3.) THEN
          RA2(INLET) = (SPAN-WI(3)/2.)*ABS(SIN(PHI(INLET)/RAD)) +
     1                                                          WI(3)/2.
          RB2(INLET) = (SPAN-WI(3)/2.)*ABS(COS(PHI(INLET)/RAD)) +
     1                                                          WI(3)/2.
        ENDIF
   70 CONTINUE
C
C
C     CALCULATE DIVERTER SPECIFIC GEOMETRY CHARACTERISTICS SUCH AS
C     HEIGTH, WIDTH, AND CROSS SECTIONAL AREA
C
      DO 79 IDCRX=1,3
        XD(IDCRX) = 0.0
        HD(IDCRX) = 0.0
        WD(IDCRX) = 0.0
        AREAD(IDCRX) = 0.0
        DEQD(IDCRX) = 0.0
        RARC(IDCRX) = 0.0
        RARX(IDCRX) = 0.0
   79 CONTINUE
C
      IF(HDIV .GT. 0.0)THEN
        XD(1) =  XDIV
        XD(2) =  XDIV + LDIV
        XD(3) =  XI(5)
        HD(1) =  HDIV
        WD(1) =  0.0
        AREAD(1) = 0.0
        DEQD(1)= WD(1)
        RARC(1)= 0.0
        RARX(1)= 0.0
        RARX(2)= 90./RAD
        RARX(3)= 90./RAD
        DO 80 J=1,4
          IF((INTYPE.EQ.1.).OR.(INTYPE.EQ.3.))THEN
            IF(ABS(XD(2)-XI(J+1)).LT.0.00001)WD(2)=WI(J+1)
            IF(XD(2).GT.XI(J).AND.XD(2).LT.XI(J+1))WD(2)=WI(J)+
     1             (XD(2)-XI(J))*(WI(J+1)-WI(J))/(XI(J+1)-XI(J))
          ELSEIF(INTYPE.EQ.2.)THEN
            IF(ABS(XD(2)-XI(J+1)).LT.0.00001)WD(2)=HI(J+1)
            IF(XD(2).GT.XI(J).AND.XD(2).LT.XI(J+1))WD(2)=HI(J)+
     1                     (XD(2)-XI(J))*(HI(J+1)-HI(J))/(XI(J+1)-XI(J))
          ENDIF
   80   CONTINUE
        WD(3)=WI(5)
        DEQD(2) = WD(2)
        DEQD(3) = WD(3)
        DO 90 I=2,3
          THETA = ACOS(WD(I)/(2.*RBOD))
          RARC(I) = 90./RAD - THETA
          HD(I) = HD(1) + RBOD*(1.-SIN(THETA))
          IF(INTYPE.EQ.3.)HD(I)=HD(1)+RBOD*(1.-SIN(THETA))+WD(I)/2.
          DLEN(I-1) = XD(I)-XD(I-1)
          XMIDD(I-1)= XD(I-1) + DLEN(I-1)/2.
          AREAD(I) = WD(I)*HD(I)-RBOD**2.*RARC(I)+
     1               0.5*RBOD*WD(I)*SIN(THETA)
          IF (INTYPE.EQ.3.) AREAD(I)=AREAD(I)-0.125*PI*WD(I)**2.
   90   CONTINUE
      ENDIF
C
C     CREATE PARAMETERS NEEDED BY THE CROSS FLOW DRAG ROUTINES
C
      CALL ILTVIN
C
C     DETERMINE ANGLES NEEDED BY THE CROSS FLOW DRAG ROUTINES
C
      CALL ILTANG
C
C     DETERMINE THE FINENESS RATIO OF THE VEHICLE COMPONENTS
C
      CALL ILTFR
C
C     DETERMINE NEWTONIAN CORRECTION FOR 2-D INLETS
C
      IF(INTYPE .NE. 3.)THEN
        DO 100 ICRX=1,5
          CALL ILTNWT(ICRX)
  100   CONTINUE
      ENDIF
C
C     DETERMINE INCREMENTS IN NEWTONIANS TERMS DUE TO DIVERTER
C     OR INLET OVERLAP WITH BODY
C
      IF(HDIV .GT. 0.0)THEN
        NCRX = 3
      ELSE
        NCRX = 5
      ENDIF
      DO 105 ICX = 1,NCRX
        CALL BODNWT(ICX)
  105 CONTINUE
C
C       DETERMINE NEWTONIAN CORRECTION FOR DIVERTERS
C
      IF(HDIV .GT. 0.0)THEN
        DO 110 IDCRX=1,3
          CALL DIVNWT(IDCRX)
  110   CONTINUE
      ELSE
        DO 120 IDCRX=1,3
          DO 122 IALP=1,20
            CRXDWD(IDCRX,IALP) = 0.0
            CRXDHT(IDCRX,IALP) = 0.0
            CNNDZP(IDCRX,IALP) = 0.0
            CNNDYP(IDCRX,IALP) = 0.0
            CNNDZN(IDCRX,IALP) = 0.0
            CNNDYN(IDCRX,IALP) = 0.0
  122     CONTINUE
  120   CONTINUE
        FRD = 0.0
        DO 130 IDCRX=1,5
          DO 132 IALP=1,20
            THETAD(IDCRX,IALP) = 0.0
            PANDLN(IDCRX,IALP) = 0.0
  132     CONTINUE
  130   CONTINUE
      ENDIF
C
C***  PRINT INLET GEOMETRY INPUTS
      IF (PARTS(15)) CALL INLETG
C
      RETURN
      END
