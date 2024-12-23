      SUBROUTINE CNPTWO(ALPI,CNI,CNIXCP)
C***********************************************************************
C
C     THIS SUBROUTINE CALCULATES INLET NORMAL FORCE COEFFICIENT AND
C     CENTER OF PRESSURE FOR A TWO DIMENSIONAL INLET IN THE INLET
C     COORDINATE SYSTEM.  THIS SUBROUTINE CALCULATES AERODYNAMICS FOR
C     ONE SET OF FLIGHT CONDITIONS.
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
      DIMENSION DCNA(4),DCN(4),DCM(4),CNA(5)
      DIMENSION ALPI(20),CNI(20),CMI(20),CNIXCP(20)
C
      NUMI=NIN+0.5
      DO 10 J=1,NUMI
        DO 20 I=1,5
C
C***      IF INLET IS OPEN CALCULATE CN-ALPHA ON RAMP
          IF ((.NOT.ICOVER).AND.(I.EQ.2)) THEN
            A=XI(I)*TAN(RAMANG/RAD)
            B=WI(I)
          ELSE
            A=HI(I)
            B=WI(I)
          ENDIF
C
C***      CALCULATE CN-ALPHA AT CROSS SECTION
          IF (A*B.EQ.0.0) THEN
            CNA(I)=0.0
          ELSE
            CNA(I)=(PI/1.8*B/A+2./PI)*A*B/SREF
          ENDIF
   20   CONTINUE
C
C***    FIND FORCES AND MOMENTS FOR EACH SECTION
        CNI(J)=0.0
        CMI(J)=0.0
        DO 30 I=1,4
C
C***      CN-ALPHA OF SECTION
          DCNA(I)=CNA(I+1)-CNA(I)
C
C***      DON'T INCLUDE BOATTAIL SECTION IN CN-ALPHA CALCULATIONS
          IF (DCNA(I).LT.0.0) DCNA(I)=0.0
C
C***      CN OF SECTION
          DCN(I)=DCNA(I)*(ALPI(J)/RAD)
C
C***      INCREMENT INLET CN
          CNI(J)=CNI(J)+DCN(I)
C
C***      CALCULATE SECTION CENTER OF PRESSURE
          XCP=XI(I)+1./3.*LENGTH(I)*(2.*WI(I+1)+WI(I))/(WI(I+1)+WI(I))
C
C***      SECTION CM
          DCM(I)=-DCN(I)*XCP/LREF
C
C***      INCREMENT INLET CM
          CMI(J)=CMI(J)+DCM(I)
   30   CONTINUE
C
C***    CALCULATE INLET CENTER OF PRESSURE
        IF (ABS(CNI(J)).LT.0.00001) THEN
          CNIXCP(J)=0.0
        ELSE
          CNIXCP(J)=ABS(CMI(J)*LREF/CNI(J))
        ENDIF
   10 CONTINUE
C
      RETURN
      END
