      SUBROUTINE CYPDIV(BETAD,CYD,CYDXCP)
C***********************************************************************
C
C     THIS SUBROUTINE CALCULATES INLET NORMAL FORCE COEFFICIENT AND
C     CENTER OF PRESSURE FOR A TWO DIMENSIONAL DIVERTER IN THE INLET
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
      DIMENSION DCYB(2),DCY(2),DCLN(2),CYB(3)
      DIMENSION BETAD(20),CYD(20),CLND(20),CYDXCP(20)
C
      NUMI=NIN+0.5
      DO 10 J=1,NUMI
C
C***      CALCULATE CY-BETA AT CROSS SECTION
        DO 20 I=1,3
          CYB(I)=0.0
          IF(WD(I).NE.0.) THEN
            CYB(I)=(PI/1.8*AREAD(I)/WD(I)**2+2./PI)*AREAD(I)/SREF
          ENDIF
   20   CONTINUE
C
C***    FIND FORCES AND MOMENTS FOR EACH SECTION
        CYD(J)=0.0
        CLND(J)=0.0
        DO 30 I=1,2
C
C***      CY-BETA OF SECTION
          DCYB(I)=CYB(I+1)-CYB(I)
C
C***      DON'T INCLUDE BOATTAIL SECTION IN CY-BETA CALCULATIONS
          IF (DCYB(I).LT.0.0) DCYB(I)=0.0
C
C***      CY OF SECTION
          DCY(I)=-DCYB(I)*(BETAD(J)/RAD)
C
C***      INCREMENT DIVERTER CY
          CYD(J)=CYD(J)+DCY(I)
C
C***      CALCULATE SECTION CENTER OF PRESSURE
          XCP=XD(I)+1./3.*DLEN(I)*(2.*HD(I+1)+HD(I))/(HD(I+1)+HD(I))
C
C***      SECTION CLN
          DCLN(I)=-DCY(I)*XCP/BREF
C
C***      INCREMENT DIVERTER CLN
          CLND(J)=CLND(J)+DCLN(I)
   30   CONTINUE
C
C***    CALCULATE DIVERTER CENTER OF PRESSURE
        IF (ABS(CYD(J)).LT.0.00001) THEN
          CYDXCP(J)=0.0
        ELSE
          CYDXCP(J)=ABS(CLND(J)*BREF/CYD(J))
        ENDIF
   10 CONTINUE
C
      RETURN
      END
