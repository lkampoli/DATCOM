      SUBROUTINE CYPAXI(BET,CYI,CLNI)
C
C***********************************************************************
C
C     THIS SUBROUTINE CALCULATES INLET SIDE FORCE COEFFICIENT AND
C     CENTER OF PRESSURE IN THE INLET COORDINATE SYSTEM FOR AN AXISYM-
C     METRIC INLET.  THIS SUBROUTINE CALCULATES AERODYNAMICS AT ONE
C     SET OF FLIGHT CONDITIONS.
C
C***********************************************************************
C
C  WRITTEN BY M. MOORE, MCDONNELL DOUGLAS
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
      DIMENSION DCYI(4),DCLNI(4)
C
      IF (.NOT.ICOVER) THEN
        WII=2.*LENGTH(1)*TAN(RAMANG/RAD)
        DCYI(1)=-(PI*BET)/(2.*RAD*SREF)*(WII)**2
        DCLNI(1)=((PI*BET)/(6.*RAD*BREF*SREF))*(2.*WII**2)*LENGTH(1)
      ELSE
        DCYI(1)=-((PI*BET)/(2.*RAD*SREF))*(DEQI(2)**2-DEQI(1)**2)
        DCLNI(1)=((PI*BET)/(6.*RAD*BREF*SREF))*(2.*DEQI(2)**2
     &            -DEQI(2)*DEQI(1)-DEQI(1)**2)*LENGTH(1)
      ENDIF
      DO 10 J=2,4
        DCYI(J)=-(PI*BET/(2.*RAD*SREF))*(DEQI(J+1)**2-DEQI(J)**2)
        TCLNI=((PI*BET)/(6.*RAD*BREF*SREF))*(2.*DEQI(J+1)**2
     &        -DEQI(J+1)*DEQI(J)-DEQI(J)**2)*LENGTH(J)
        IF(DCYI(J).GT.-0.00001)THEN
          DCYI(J)=0.0
          DCLNI(J)=0.0
        ELSE
          TCYICP=TCLNI*BREF/DCYI(J)
          DCYICP=-XI(J)+TCYICP
          DCLNI(J)=DCYI(J)*DCYICP/BREF
        ENDIF
   10   CONTINUE
      CYI=0.0
      CLNI=0.0
      DO 20 J=1,4
        CYI=CYI+DCYI(J)
        CLNI=CLNI+DCLNI(J)
   20 CONTINUE
C
      RETURN
      END
