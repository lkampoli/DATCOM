      SUBROUTINE ILTVIN
C**********************************************************************
C
C     THIS SUBROUTINE IS USED TO CALCULATE HEIGHTS AND WIDTHS OF
C     INLETS IN THE BODY SYSTEM.  THESE PARAMETERS ARE NEEDED BY THE
C     SUBROUTINES ILTNWT AND ILTCFD.
C
C**********************************************************************
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
C
      DIMENSION PANINC(5)
C
      CNCIRN = 4.0 / 3.0
      NUMI=NIN+0.5
      DO 10 INLET=1,NUMI
        PHIR(INLET)=PHI(INLET)/RAD
        IF (INTYPE.NE.3.0) THEN
          IF (INTYPE.EQ.1.0) THEN
            DO 20 ICRX=1,5
              ANGP=PHI(INLET)/RAD+PI/2.+SI(ICRX)
              ANGM=PHI(INLET)/RAD+PI/2.-SI(ICRX)
              IF((PHI(INLET).GE.0.0.AND.PHI(INLET).LE.90.).OR.
     &            (PHI(INLET).GE.180.AND.PHI(INLET).LE.270))THEN
                CRXIWD(ICRX,INLET)=ABS(CRXLEN(ICRX)*SIN(ANGM))
                CRXIHT(ICRX,INLET)=ABS(CRXLEN(ICRX)*COS(ANGP))
              ELSE
                CRXIWD(ICRX,INLET)=ABS(CRXLEN(ICRX)*SIN(ANGP))
                CRXIHT(ICRX,INLET)=ABS(CRXLEN(ICRX)*COS(ANGM))
              ENDIF
   20       CONTINUE
          ELSE
            DO 25 ICRX=1,5
              ANGP=PHI(INLET)/RAD+SI(ICRX)
              ANGM=PHI(INLET)/RAD-SI(ICRX)
              IF((PHI(INLET).GE.0.0.AND.PHI(INLET).LE.90.).OR.
     &            (PHI(INLET).GE.180.AND.PHI(INLET).LE.270))THEN
                CRXIWD(ICRX,INLET)=ABS(CRXLEN(ICRX)*SIN(ANGP))
                CRXIHT(ICRX,INLET)=ABS(CRXLEN(ICRX)*COS(ANGM))
              ELSE
                CRXIWD(ICRX,INLET)=ABS(CRXLEN(ICRX)*SIN(ANGM))
                CRXIHT(ICRX,INLET)=ABS(CRXLEN(ICRX)*COS(ANGP))
              ENDIF
   25       CONTINUE
          ENDIF
C
          DO 40 ICRX = 1, 5
            PANINC(ICRX) = 0.0
            IF(HDIV .GT. 0.0)THEN
              DO 50 IDCRX = 1, 2
               IF(XI(ICRX).GE.XD(IDCRX).AND.XI(ICRX).LE.XD(IDCRX+1))THEN
                 PANINC(ICRX)=WD(IDCRX)+((XI(ICRX)-XD(IDCRX))/
     &                  (XD(IDCRX+1)-XD(IDCRX)))*(WD(IDCRX+1)-WD(IDCRX))
               ENDIF
   50         CONTINUE
            ENDIF
   40     CONTINUE
          DO 60 ICRX = 1, 5
            IF (INTYPE.EQ.1.) THEN
              PANILN(1,ICRX,INLET)=HI(ICRX)
              PANILN(2,ICRX,INLET)=WI(ICRX)
              PANILN(3,ICRX,INLET)=HI(ICRX)
              PANILN(4,ICRX,INLET)=WI(ICRX)-PANINC(ICRX)
            ELSE
              PANILN(1,ICRX,INLET)=WI(ICRX)
              PANILN(2,ICRX,INLET)=HI(ICRX)
              PANILN(3,ICRX,INLET)=WI(ICRX)
              PANILN(4,ICRX,INLET)=HI(ICRX)-PANINC(ICRX)
            ENDIF
   60     CONTINUE
        ENDIF
C
C     CALCULATE THE DIVERTER HEIGTHS AND WIDTHS
        IF(HDIV .GT. 0.0)THEN
          DO 30 IDCRX = 1, 3
            CRXDHT(IDCRX,INLET)=ABS(HD(IDCRX)*COS(PHI(INLET)/RAD))
            CRXDWD(IDCRX,INLET)=ABS(HD(IDCRX)*SIN(PHI(INLET)/RAD))
   30     CONTINUE
        ENDIF
C
   10 CONTINUE
C
      IF(HDIV .GT. 0.0)THEN
        DO 70 IDCRX = 1, 3
          PANDLN(1,IDCRX)=HD(IDCRX)
          PANDLN(2,IDCRX)=0.0
          PANDLN(3,IDCRX)=HD(IDCRX)
          PANDLN(4,IDCRX)=0.0
   70   CONTINUE
      ENDIF
C
      RETURN
      END
