      SUBROUTINE LINXCP(IMACH,XCPA,XCPB)
C***********************************************************************
C
C     THIS SUBROUTINE CALCULATES CENTER OF PRESSURE LOCATION IN THE
C     LONGITUDINAL DIRECTION.  RESULTS PRESENTED IN CHARTS 10 & 11
C     OF NACA REPORT 1307, FOR XCP AT BOTH SUBSONIC AND SUPERSONIC,
C     ARE USED.
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
      BETARA = SQRT(ABS(MACH(IMACH)**2.-1.))*ARA
      BETARB = SQRT(ABS(MACH(IMACH)**2.-1.))*ARB
C
      IF (MACH(IMACH).GT.1.0) THEN
        IF ((BETARA.GE.0.0).AND.(BETARA.LT.1.5)) XCPA =
     1     -0.11429*BETARA**2.+0.45143*BETARA+0.0014286
        IF ((BETARB.GE.0.0).AND.(BETARB.LT.1.5)) XCPB =
     1     -0.11429*BETARB**2.+0.45143*BETARB+0.0014286
        IF ((BETARA.GE.1.5).AND.(BETARA.LT.3.5)) XCPA =
     1     -0.01122*BETARA**2.+0.07837*BETARA+0.33188
        IF ((BETARB.GE.1.5).AND.(BETARB.LT.3.5)) XCPA =
     1     -0.01122*BETARB**2.+0.07837*BETARB+0.33188
        IF (BETARA.GE.3.5) XCPA = 0.00511*BETARA + 0.45211
        IF (BETARB.GE.3.5) XCPB = 0.00511*BETARB + 0.45211
      ELSEIF (MACH(IMACH).LE.1.0) THEN
        IF (BETARA.LT.2.0) XCPA = 0.05921*BETARA**3.-
     1                              0.27959*BETARA**2.+
     1                               0.44650*BETARA+0.0033737
        IF (BETARA.GE.2.0) XCPA = 0.250
        IF (BETARB.LT.2.0) XCPB = 0.05921*BETARB**3.-
     1                              0.27959*BETARB**2.+
     1                               0.44650*BETARB+0.0033737
        IF (BETARB.GE.2.0) XCPB = 0.250
      ENDIF
C
      XCPA = XCPA*XI(5)
      XCPB = XCPB*XI(5)
C
      RETURN
      END
