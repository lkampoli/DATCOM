      SUBROUTINE ILTSWT(SWET)
C***********************************************************************
C
C     THIS SUBROUTINE CALCULATES THE WETTED AREA OF THE INLET AND
C     DIVERTER COMBINATION.
C
C***********************************************************************
C
C  WRITTEN BY M. MOORE, MCDONNELL DOUGLAS
C  MODIFIED BY K. BURNS, MCDONNELL DOUGLAS
C
      COMMON /GEOBOD/ DUM(32),DMAX,SMAX,LBOD,DUM2(12)
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
      REAL LBOD
C
      DIMENSION LD(2)
C
      RBOD=DMAX/2.0
C
C     DIVERTER
C
      SWETD  = 0.0
      DOUBLE = 0.0
      IF(HDIV .GT. 0.0)THEN
        LD(1) = SQRT((WD(2)/2.)**2.+(XD(2)-XD(1))**2.)
        LD(2) = XD(3) - XD(2)
        DO 10 I=1,2
          SWETD1 = (HD(I+1)+HD(I))*LD(I)
          SWETD  = SWETD + SWETD1
C
C     CALCULATION TO ACCOUNT FOR INLET-DIVERTER INTERFACE
C
          DOUBLE = DOUBLE + 0.5*(WD(I)+WD(I+1))*(XD(I+1)-XD(I))
C
   10   CONTINUE
      ENDIF
C
C     CALCULATE INLET/BODY AREA OVERLAP WHEN NO DIVERTER EXIST
C
      IF(HDIV .LT. 0.0)THEN
        IF(INTYPE .EQ. 1.)THEN
          DO 15 I=1,4
            DOUBLE = DOUBLE + (WI(I) + WI(I+1))/2.*LENGTH(I)
   15     CONTINUE
        ENDIF
        IF(INTYPE .EQ. 2.)THEN
          DO 16 I=1,4
            DOUBLE = DOUBLE + (HI(I) + HI(I+1))/2.*LENGTH(I)
   16     CONTINUE
        ENDIF
        IF(INTYPE .EQ. 3.)THEN
          DO 17 I=2,4
C
C           CALCULATE THE ARC AT THE ENDS OF THIS INLET SECTION THAT 
C           DEFINE BODY/INLET INTERSECTION POINTS AND REMOVE AREA
C           SUBTENDED BY THESE ARCS FROM WETTED AREA
            RINL  = WI(I)/2.
            XINT  = (RBOD**2 + RBOD*RINL + RBOD*HDIV + 
     1              RINL*HDIV + HDIV**2/2.)/ (RBOD + RINL + HDIV)
            YINT  = SQRT(RBOD**2 - XINT**2)
            THETI = 2.*ATAN(YINT/(RBOD+RINL+HDIV-XINT))
            IF(THETI .LT. 0.0)THETI = THETI + 2.0*PI
            ARCI  = THETI*RINL
C
            RINL   = WI(I+1)/2.
            XINT        = (RBOD**2 + RBOD*RINL + RBOD*HDIV + 
     1                    RINL*HDIV + HDIV**2/2.)/ (RBOD + RINL + HDIV)
            YINT        = SQRT(RBOD**2 - XINT**2)
            THETI1 = 2.*ATAN(YINT/(RBOD+RINL+HDIV-XINT))
            IF(THETI1 .LT. 0.0)THETI1 = THETI1 + 2.0*PI
            ARCI1  = THETI1*RINL
            DOUBLE = DOUBLE + (ARCI + ARCI1)/2.*LENGTH(I)
   17     CONTINUE
        ENDIF
      ENDIF            
C
C     INLET
C
      SWETI = 0.0
      IF (INTYPE.NE.3.) THEN
        DO 20 I=1,4
          SWETI1 = (WI(I)+HI(I) + WI(I+1)+HI(I+1))*LENGTH(I)
          SWETI  = SWETI + SWETI1
   20   CONTINUE
      ELSE
        DO 25 I=1,4
          SWETI1 = PI*(WI(I)+WI(I+1))/2.*LENGTH(I)
          SWETI = SWETI + SWETI1
   25   CONTINUE
      ENDIF
C
C     SUM INLET AND DIVERTER WETTED AREAS
C
      SWET = SWETI + SWETD - DOUBLE
      IF ((.NOT.ICOVER).AND.INTYPE.NE.3.) THEN
        SWET = SWET - 0.5*(WI(1)+WI(2))*LENGTH(1)
      ELSEIF ((.NOT.ICOVER).AND.INTYPE.EQ.3.) THEN
        SWET = SWET - PI*WI(2)/2.*LENGTH(1)
      ENDIF
C
      RETURN
      END
