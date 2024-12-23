      SUBROUTINE BODNWT(IDCRX)
C***********************************************************************
C
C     THIS SUBROUTINE ACCOUNTS FOR BODY CROSS FLOW CONTRIBUTIONS
C     THAT DO NOT ACTUALLY EXIST BECAUSE THE INLET DIVERTER.  THESE
C     FORCE CONTRIBUTIONS ARE CALCULATED AND THEN SUBTRACTED FROM THE
C     INLET CROSS FLOW FORCE CALCULATIONS.
C
C***********************************************************************
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
      REAL MINARC,M
C
      DELFOR(P,M)=1./3.*(SIN(P)*(COS(P)**2+2.)-SIN(M)*(COS(M)**2+2.))
C
C     DETERMINE THE ARC LENGTH COVERED BY THE INLET OR DIVERTER
      NUMI=NIN+0.5
      DO 100 INLET=1,NUMI
C
C     INITIALIZE VARIABLES
        DLCNBP(IDCRX,INLET) = 0.0
        DLCNBN(IDCRX,INLET) = 0.0
        DLCYBP(IDCRX,INLET) = 0.0
        DLCYBN(IDCRX,INLET) = 0.0
C
        PHIPR = PHI(INLET)/RAD+RARC(IDCRX)
        PHIMR = PHI(INLET)/RAD-RARC(IDCRX)
        COSPR = COS(PHIPR)
        COSMR = COS(PHIMR)
        SINPR = SIN(PHIPR)
        SINMR = SIN(PHIMR)
        IF (ABS(COSPR).LT.0.00001) COSPR = 0.0
        IF (ABS(COSMR).LT.0.00001) COSMR = 0.0
        IF (ABS(SINPR).LT.0.00001) SINPR = 0.0
        IF (ABS(SINMR).LT.0.00001) SINMR = 0.0
C
C     FOR POSITIVE ANGLES OF ATTACK
C       CHECK IF ANY PART OF INLET IS IN SECOND AND THIRD QUADRANT
        IF(COSPR.LE.0..OR.COSMR.LE.0.)THEN
C         FIRST AND SECOND QUADRANT
          IF(COSMR.GT.0.)THEN
            PLSARC =PHIPR-PI
            MINARC =-PI/2.0
C         THIRD AND FOURTH QUADRANT
          ELSEIF(COSPR.GT.0.0)THEN
            PLSARC =PI/2.0 
            MINARC =PHIMR-PI
C         FULLY IN SECOND AND THIRD QUADRANT
          ELSE
            PLSARC =PHIPR-PI 
            MINARC =PHIMR-PI
          ENDIF
          DLCNBP(IDCRX,INLET) = DELFOR(PLSARC,MINARC)
        ENDIF
C
C     FOR NEGATIVE ANGLES OF ATTACK
C       CHECK IF ANY PART OF INLET IS IN FOURTH OR FIRST QUADRANT
        IF(COSPR.GE.0..OR.COSMR.GE.0.)THEN
C         FIRST AND SECOND QUADRANT
          IF(COSPR.LT.0.)THEN
            PLSARC =PI/2.0
            MINARC =PHIMR
C         THIRD AND FOURTH QUADRANT
          ELSEIF(COSMR.LT.0.0)THEN
            PLSARC =PHIPR-2.0*PI
            MINARC =-PI/2.0
C         FOURTH AND FIRST QUADRANT
          ELSEIF(PHI(INLET).GE.0.0.AND.PHI(INLET).LE.90.) THEN
            PLSARC =PHIPR
            MINARC =PHIMR
          ELSE
            PLSARC =PHIPR-2.0*PI
            MINARC =PHIMR-2.0*PI
          ENDIF
          DLCNBN(IDCRX,INLET) = DELFOR(PLSARC,MINARC)
        ENDIF
C
C     FOR POSITIVE SIDE-SLIP ANGLES 
C       CHECK IF ANY PART OF INLET IS IN FIRST OR SECOND QUADRANT
        IF(SINPR.GE.0..OR.SINMR.GE.0.)THEN
C         FOURTH AND FIRST QUADRANT
          IF(SINMR.LT.0.)THEN
            PLSARC =PHIPR-PI/2.0
            MINARC =-PI/2.0
C         SECOND AND THIRD QUADRANT
          ELSEIF(SINPR.LT.0.0)THEN
            PLSARC =PI/2.0
            MINARC =PHIMR-PI/2.0
C         FULLY IN FIRST OR SECOND QUADRANT
          ELSE
            PLSARC =PHIPR-PI/2.0
            MINARC =PHIMR-PI/2.0
          ENDIF
          DLCYBP(IDCRX,INLET) = DELFOR(PLSARC,MINARC)
        ENDIF
C
C     FOR NEGATIVE SIDE-SLIP ANGLES 
C       CHECK IF ANY PART OF INLET IS IN THIRD OR FOURTH QUADRANT
        IF(SINPR.LE.0..OR.SINMR.LE.0.)THEN
C         FOURTH AND FIRST QUADRANT
          IF(SINPR.GT.0.)THEN
            PLSARC =PI/2.0
            MINARC =PHIMR-3.0*PI/2.0
C         SECOND AND THIRD QUADRANT
          ELSEIF(SINMR.GT.0.0)THEN
            PLSARC =PHIPR-3.0*PI/2.0
            MINARC =-PI/2.0
C         FULLY IN THIRD AND FOURTH QUADRANT
          ELSE
            PLSARC =PHIPR-3.0*PI/2.0
            MINARC =PHIMR-3.0*PI/2.0
          ENDIF
          DLCYBN(IDCRX,INLET) = DELFOR(PLSARC,MINARC)
        ENDIF
  100 CONTINUE
C
      RETURN
      END
