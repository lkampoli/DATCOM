      SUBROUTINE BETADR(IM,TOTC)
C      
C***  EXECUTIVE ROUTINE FOR CALCULATING THE STATIC AERODYNAMICS
C***  FOR ALL COMPONENTS OF THE CONFIGURATION.  THE FIRST EXECUTION
C***  PORTION IS AT THE INPUT CONDITIONS.  THE SECOND PORTION IS FOR
C***  A BETA PERTURBATION OF ONE DEGREE TO GET THE BETA DERIVATIVES.
C      
      COMMON /CASEID/ CC(177),NOLAT,IR,IPAGE
      LOGICAL NOLAT
      COMMON /CONST/ PI,RAD,UNUSED,KAND
      COMMON /FLC/    NALPHA,FLDUM(143)
      REAL NALPHA
      COMMON /SBODY/  CBODY(220)
      COMMON /SFIN1/  CFIN1(220)
      COMMON /SFIN2/  CFIN2(220)
      COMMON /SFIN3/  CFIN3(220)
      COMMON /SFIN4/  CFIN4(220)
      COMMON /SB1/    CB1(220)
      COMMON /SB12/   CB12(220)
      COMMON /SB123/  CB123(220)
      COMMON /SB1234/ CB1234(220)
      COMMON /PARTF/  PARTS(19)
      DIMENSION PARTSS(16)
      LOGICAL PARTS,PARTSS,LPARTS
C      
      COMMON /LOGIC/ LDMPCS,LDAMP,LBUILD,LNACA,LDERDG,
     1               LDERRD,LPART,LNAME,LPLOT,
     2               LFLT,LREFQ,LAXIS,LFIN1,LFIN2,LFIN3,LFIN4,
     3               LDEFL,LTRIM,LDIMIN,LDIMFT,LDIMCM,LDIMM,LELLB,
     4               LINLET,LEXPR,LICRMT,LSPIN,LARBOD
      LOGICAL        LDMPCS,LDAMP,LBUILD,LNACA,LDERDG,
     1               LDERRD,LPART,LNAME,LPLOT,
     2               LFLT,LREFQ,LAXIS,LFIN1,LFIN2,LFIN3,LFIN4,
     3               LDEFL,LTRIM,LDIMIN,LDIMFT,LDIMCM,LDIMM,LELLB,
     4               LINLET,LEXPR,LICRMT,LSPIN,LARBOD
C
      DIMENSION TOTC(80),TTOTC(80),TB1(220),TB12(220),TB123(220),
     1 TB1234(220)
      DIMENSION TBODY(220),IFLAGA(3,4)
C      
C     SET FLAGS FOR BLANKING LAT-DIR DERIV. TO ZERO
C      
      DO 1010 J=1,4
         DO 1000 I=1,3
            IFLAGA(I,J)=0
 1000    CONTINUE
 1010 CONTINUE
      NALP=NALPHA+.5
C      
C***  COMPUTE STATIC AERODYNAMICS AT INPUT CONDITIONS
C      
      CALL SYNTHS(IM,TOTC,IFLAGA,0.)
      CALL BLKLDD(IFLAGA)
C      
C***  IF NOLAT IS TRUE THEN DO NOT COMPUTE BETA DERIVATIVES
C***  BLANK OUT DERIVATIVE FIELDS
C      
      IF(.NOT. NOLAT)GO TO 1030
C      
      DO 1020 I=161,220
         CB1(I)=-UNUSED
         CB12(I)=-UNUSED
         CB123(I)=-UNUSED
         CB1234(I)=-UNUSED
 1020 CONTINUE
      GO TO 1130
C      
C***  SAVE PRESENT CALCULATIONS
C***  TURN OFF PART FLAG FOR DERIVATIVE CALCULATIONS
C      
 1030 LPARTS=LPART
      LPART=.FALSE.
      DO 1040 I=1,16
         PARTSS(I)=PARTS(I)
         PARTS(I)=.FALSE.
 1040 CONTINUE
      DO 1050 I=1,220
         IF(I .LE. 80)TTOTC(I)=TOTC(I)
         TBODY(I)=CBODY(I)
         TB1(I)=CB1(I)
         TB12(I)=CB12(I)
         TB123(I)=CB123(I)
         TB1234(I)=CB1234(I)
         CB1(I)=UNUSED
         CB12(I)=UNUSED
         CB123(I)=UNUSED
         CB1234(I)=UNUSED
 1050 CONTINUE
C      
C***  PERTURB BETA BY ONE DEGREE AND COMPUTE NEW FLIGHT CONDITIONS
C      
      DO 1060 I=21,40
         TTOTC(I)=TTOTC(I)+1.
 1060 CONTINUE
C      
      DO 1070 I=1,NALP
         TTOTC(I+40)=UNUSED
         TTOTC(I+60)=UNUSED
         CALL ALPBET(TTOTC(I),TTOTC(I+60),TTOTC(I+20),TTOTC(I+40))
 1070 CONTINUE
C      
C*** FOR BETA PERTURBED, COMPUTE BODY ALONE DATA
C      
      DO 1080 I=1,NALP
         CBODY(I+60)=CBODY(I+60)+CBODY(I+160)
         CBODY(I+80)=CBODY(I+80)+CBODY(I+180)
         CBODY(I+100)=CBODY(I+100)+CBODY(I+200)
 1080 CONTINUE
C      
C***  SYNTHESIZE (RESULTS STORED IN I.O.M.)
C      
      CALL SYNTHS(IM,TTOTC,IFLAGA,0.)
C      
C***  DIFFERENCE PRESENT SYNTHESIS RESULTS WITH PRIOR TO GET
C***  BETA DERIVATIVES
C      
      DO 1090 I=1,220
         CB1(I)=CB1(I)-TB1(I)
         CB12(I)=CB12(I)-TB12(I)
         CB123(I)=CB123(I)-TB123(I)
         CB1234(I)=CB1234(I)-TB1234(I)
 1090 CONTINUE
C      
C ... DESIRE ONLY CY, CLN AND CLL DERIVATIVES
C      
      DO 1100 I=161,220
         CB1(I)=CB1(I-100)
         CB12(I)=CB12(I-100)
         CB123(I)=CB123(I-100)
         CB1234(I)=CB1234(I-100)
 1100 CONTINUE
      CALL BLKLDD(IFLAGA)
C      
C***  RESTORE FIRST SYNTHESIS RESULTS
C      
      DO 1110 I=1,220
         CBODY(I)=TBODY(I)
         IF(I .GT. 160)GO TO 1110
         CB1(I)=TB1(I)
         CB12(I)=TB12(I)
         CB123(I)=TB123(I)
         CB1234(I)=TB1234(I)
 1110 CONTINUE
C      
      LPART=LPARTS
      DO 1120 I=1,16
         PARTS(I)=PARTSS(I)
 1120 CONTINUE
C      
 1130 CONTINUE
C
      RETURN
      END
