       SUBROUTINE ILTRVX(NSTEP,X,R)
C***********************************************************************
C
C     THIS SUBROUTINE RESOLVES THE CROSS SECTIONAL AREAS OF THE INLET
C     DIVERTER COMBINATION INTO R VS X VALUES FOR USE BY THE SUBROU-
C     TINE CDPRESS.
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
      DIMENSION XTEMP1(50),XTEMP2(50),AREA(50),RTEMP1(50),
     1                            RTEMP2(50),X(200),R(200)
C
      IF(HDIV .GT. 0.0)THEN
        K = 1
        JJ = 1
    5   CONTINUE
        IF (XI(JJ).LT.XD(1)) THEN
          XTEMP1(K) = XI(JJ)
          AREA(K) = AREAI(JJ)
          JJ = JJ + 1
          K = K + 1
          GO TO 5
        ENDIF
        DO 20 J = JJ-1, 4
          DO 10 I = 1, 2
            IF (XD(I).GT.XI(J).AND.XD(I).LT.XI(J+1)) THEN
              XTEMP1(K) = XD(I)
              AREA(K) = AREAD(I) + (AREAI(J) + ((XD(I)-XI(J))/
     1  (XI(J+1)-XI(J)))**2.*(AREAI(J+1)-AREAI(J)))
              K = K +1
            ELSEIF (XI(J).GT.XD(I).AND.XI(J).LT.XD(I+1)) THEN
              XTEMP1(K) = XI(J)
              AREA(K) = AREAI(J) + (AREAD(I) + ((XI(J)-XD(I))/
     1  (XD(I+1)-XD(I)))**2.*(AREAD(I+1)-AREAD(I)))
              K = K +1
            ELSEIF (XI(J).EQ.XD(I)) THEN
              XTEMP1(K) = XI(J)
              XTEMP1(K+1) = XD(I)
              AREA(K) = AREAI(J) + AREAD(I)
              AREA(K+1) = AREAI(J) + AREAD(I)
              K = K + 2
            ENDIF
   10     CONTINUE
   20   CONTINUE
        XTEMP1(7) = XI(5)
        AREA(7) = AREAI(5) + AREAD(3)
      ENDIF
C
C     WHEN THERE IS NO DIVERTER, ADD A SECTION MID-WAY BETWEEN 
C     INPUT SECTIONS 2 AND 3 AS WELL AS 4 AND 5 TO MATCH THE NUMBER
C     OF SECTIONS IN XTEMP1 WHEN THERE IS A DIVERTER
C
      IF(HDIV .LE. 0.0)THEN
        J = 1
        DO 25 I=1,7
          IF(I.NE.3 .AND. I.NE.6)THEN
            XTEMP1(I) = XI(J)
            AREA(I)   = AREAI(J)
            J = J+1
          ENDIF
   25   CONTINUE
        RTMPL = SQRT(ABS(AREAI(2))/PI)
        IF(AREAI(2) .LT. 0.0)RTMPL = -RTMPL
        RTMPH = SQRT(ABS(AREAI(3))/PI)
        IF(AREAI(3) .LT. 0.0)RTMPH = -RTMPH
        RAVE  = 0.5*(RTMPL + RTMPH)
        AREA(3) = PI*RAVE**2
        XTEMP1(3) = XI(2) + 0.5*(XI(3) - XI(2))
C
        RTMPL = SQRT(ABS(AREAI(4))/PI)
        IF(AREAI(4) .LT. 0.0)RTMPL = -RTMPL
        RTMPH = SQRT(ABS(AREAI(5))/PI)
        IF(AREAI(5) .LT. 0.0)RTMPH = -RTMPH
        RAVE  = 0.5*(RTMPL + RTMPH)
        AREA(6) = PI*RAVE*ABS(RAVE)
        XTEMP1(6) = XI(4) + 0.5*(XI(5) - XI(4))
      ENDIF
C
C     CALCULATE RADIUS FROM THE GIVEN AREA, REMOVE COINCIDENT POINTS,
C     AND INSERT NEW POINTS BETWEEN THE CROSS SECTIONS.
C
      DO 30 I=1,7
        RTEMP1(I)=SQRT(ABS(AREA(I)/PI))
        IF (AREA(I).LT.0.0) RTEMP1(I)=-RTEMP1(I)
   30 CONTINUE
      MCOUNT = 7
      DO 50 I=1,5
        IF (RTEMP1(I).EQ.RTEMP1(I+1)) THEN
          II = I
          K = I+1
          DO 40 J=II,5
            XTEMP1(K)=XTEMP1(K+1)
            RTEMP1(K)=RTEMP1(K+1)
            K = K+1
   40     CONTINUE
          XTEMP1(K)=0.0
          RTEMP1(K)=0.0
          MCOUNT = MCOUNT - 1
        ENDIF
   50 CONTINUE
      K = 1
      DO 60 I=1,MCOUNT-1
        XTEMP2(K)=XTEMP1(I)
        XTEMP2(K+1)=XTEMP1(I)+0.25*(XTEMP1(I+1)-XTEMP1(I))
        XTEMP2(K+2)=XTEMP1(I)+0.50*(XTEMP1(I+1)-XTEMP1(I))
        XTEMP2(K+3)=XTEMP1(I)+0.75*(XTEMP1(I+1)-XTEMP1(I))
        RTEMP2(K)=RTEMP1(I)
        RTEMP2(K+1)=RTEMP1(I)+0.25*(RTEMP1(I+1)-RTEMP1(I))
        RTEMP2(K+2)=RTEMP1(I)+0.50*(RTEMP1(I+1)-RTEMP1(I))
        RTEMP2(K+3)=RTEMP1(I)+0.75*(RTEMP1(I+1)-RTEMP1(I))
        K = K+4
   60 CONTINUE
      XTEMP2(K) = XTEMP1(MCOUNT)
      RTEMP2(K) = RTEMP1(MCOUNT)
      DO 70 I=1,K
        IF (RTEMP2(I).GT.0.0) GO TO 80
   70 CONTINUE
   80 CONTINUE
      M = 1
      DO 90 N=I-1,K
       IF (RTEMP2(N).LT.0.0) GO TO 100
        X(M)=XTEMP2(N)
        R(M)=RTEMP2(N)
        M = M+1
   90 CONTINUE
  100 CONTINUE
      NSTEP = M-1
C
      RETURN
      END
