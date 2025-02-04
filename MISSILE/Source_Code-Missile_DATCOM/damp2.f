      SUBROUTINE DAMP2(IM,TOTC)
C      
C  REV 1/96 UPDATE - NEW SUBROUTINE
C
C***  EXECUTIVE ROUTINE FOR CALCULATING THE DYNAMIC AERODYNAMICS
C***  FOR ALL COMPONENTS OF THE CONFIGURATION.  THE DERIVATIVES
C***  ARE CALCULATED IN THE SEQUENCE PITCH, YAW, ROLL.
C***  THE EQUIVALENT ANGLE OF ATTACK IS MODIFIED FOR EACH
C***  AXIS AND THE RESULTS ARE DIFFERENCED FROM THE STATIC RESULTS.
C
C  WRITTEN BY W. BLAKE, WL/FIGC
C
C***  A REDUCED FREQUENCY OF 0.02 IS ASSUMED (RATE*LENGTH/2.*VELOCITY)
C***  UNITS ARE RADIANS.  LREF IS THE REF. LENGTH FOR THE PITCH AXIS,
C***  LATREF IS THE REF. LENGTH FOR THE ROLL AND YAW AXES.
C 
      COMMON /CASEID/ CC(177),NOLAT,IR,IPAGE
      COMMON /CONST/ PI,RAD,UNUSED,KAND
      COMMON /FLC/    NALPHA,FLDUM(143)
C
      COMMON /SBODY/  CBODY(220)
      COMMON /SFIN1/  CFIN1(220)
      COMMON /SFIN2/  CFIN2(220)
      COMMON /SFIN3/  CFIN3(220)
      COMMON /SFIN4/  CFIN4(220)
      COMMON /SB1/    CB1(220)
      COMMON /SB12/   CB2(220)
      COMMON /SB123/  CB3(220)
      COMMON /SB1234/ CB4(220)
C
      COMMON /DBODY/ BCNQ(20),BCMQ(20),BCAQ(20),BCYQ(20),
     1  BCLNQ(20),BCLLQ(20),BCNR(20),BCMR(20),BCAR(20),
     2  BCYR(20),BCLNR(20),BCLLR(20),BCNP(20),BCMP(20),
     3  BCAP(20),BCYP(20),BCLNP(20),BCLLP(20),
     4  BCNAD(20),BCMAD(20),CLLP,CYP,CNP1,CNP3,CNP5,CNPY5
C
      COMMON /DB1/ D1CNQ(20),D1CMQ(20),D1CAQ(20),D1CYQ(20),
     1  D1CLNQ(20),D1CLLQ(20),D1CNR(20),D1CMR(20),D1CAR(20),
     2  D1CYR(20),D1CLNR(20),D1CLLR(20),D1CNP(20),D1CMP(20),
     3  D1CAP(20),D1CYP(20),D1CLNP(20),D1CLLP(20),
     4  D1CNAD(20),D1CMAD(20)
C
      COMMON /DB12/ D2CNQ(20),D2CMQ(20),D2CAQ(20),D2CYQ(20),
     1  D2CLNQ(20),D2CLLQ(20),D2CNR(20),D2CMR(20),D2CAR(20),
     2  D2CYR(20),D2CLNR(20),D2CLLR(20),D2CNP(20),D2CMP(20),
     3  D2CAP(20),D2CYP(20),D2CLNP(20),D2CLLP(20),
     4  D2CNAD(20),D2CMAD(20)
C
      COMMON /DB123/ D3CNQ(20),D3CMQ(20),D3CAQ(20),D3CYQ(20),
     1  D3CLNQ(20),D3CLLQ(20),D3CNR(20),D3CMR(20),D3CAR(20),
     2  D3CYR(20),D3CLNR(20),D3CLLR(20),D3CNP(20),D3CMP(20),
     3  D3CAP(20),D3CYP(20),D3CLNP(20),D3CLLP(20),
     4  D3CNAD(20),D3CMAD(20)
C
      COMMON /DB1234/ D4CNQ(20),D4CMQ(20),D4CAQ(20),D4CYQ(20),
     1  D4CLNQ(20),D4CLLQ(20),D4CNR(20),D4CMR(20),D4CAR(20),
     2  D4CYR(20),D4CLNR(20),D4CLLR(20),D4CNP(20),D4CMP(20),
     3  D4CAP(20),D4CYP(20),D4CLNP(20),D4CLLP(20),
     4  D4CNAD(20),D4CMAD(20)
C
      COMMON /PARTF/  PARTS(19)
      DIMENSION PARTSS(16)
      REAL NALPHA
      LOGICAL PARTS,PARTSS,LPARTS,NOLAT
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
      DIMENSION TOTC(80),TTOTC(80),TB1(220),TB2(220),TB3(220),
     1 TB4(220)
      DIMENSION TBODY(220),IFLAGA(3,4)
C
      NALP=NALPHA+.5
C      
C   CALCULATE BODY ALONE DYNAMIC DERIVATIVES
C
      CALL BDAMP(IM)
C
C   CALCULATE FIN ALPHA-DOT DERIVATIVES
C
      CALL FDAMP(IM)
C      
C     SET FLAGS FOR BLANKING LAT-DIR DERIV. TO ZERO
C      
      DO 1100 J=1,4
         DO 1150 I=1,3
            IFLAGA(I,J)=0
 1150    CONTINUE
 1100 CONTINUE
      RATE=0.02
C      
C***  SAVE PRESENT CALCULATIONS (STATIC RESULTS)
C***  TURN OFF PART FLAG FOR DERIVATIVE CALCULATIONS
C      
      LPARTS=LPART
      LPART=.FALSE.
      DO 1200 I=1,16
         PARTSS(I)=PARTS(I)
         PARTS(I)=.FALSE.
 1200 CONTINUE
C
      DO 1300 I=1,220
         TB1(I)=CB1(I)
         TB2(I)=CB2(I)
         TB3(I)=CB3(I)
         TB4(I)=CB4(I)
         CB1(I)=UNUSED
         CB2(I)=UNUSED
         CB3(I)=UNUSED
         CB4(I)=UNUSED
 1300 CONTINUE
C
C***  PITCH RESULTS
C
      CALL SYNTHS(IM,TOTC,IFLAGA,1.)
C
C***  CALCULATE PITCH DERIVATIVES
C
      QFACT=1./RATE
      DO 1400 I=1,NALP
         D1CNQ(I)=BCNQ(I)+QFACT*(CB1(I)-TB1(I))
         D1CMQ(I)=BCMQ(I)+QFACT*(CB1(I+20)-TB1(I+20))
         D1CAQ(I)=BCAQ(I)+QFACT*(CB1(I+40)-TB1(I+40))
         D1CYQ(I)=BCYQ(I)+QFACT*(CB1(I+60)-TB1(I+60))
         D1CLNQ(I)=BCLNQ(I)+QFACT*(CB1(I+80)-TB1(I+80))
         D1CLLQ(I)=BCLLQ(I)+QFACT*(CB1(I+100)-TB1(I+100))
C
         D2CNQ(I)=BCNQ(I)+QFACT*(CB2(I)-TB2(I))
         D2CMQ(I)=BCMQ(I)+QFACT*(CB2(I+20)-TB2(I+20))
         D2CAQ(I)=BCAQ(I)+QFACT*(CB2(I+40)-TB2(I+40))
         D2CYQ(I)=BCYQ(I)+QFACT*(CB2(I+60)-TB2(I+60))
         D2CLNQ(I)=BCLNQ(I)+QFACT*(CB2(I+80)-TB2(I+80))
         D2CLLQ(I)=BCLLQ(I)+QFACT*(CB2(I+100)-TB2(I+100))
C
         D3CNQ(I)=BCNQ(I)+QFACT*(CB3(I)-TB3(I))
         D3CMQ(I)=BCMQ(I)+QFACT*(CB3(I+20)-TB3(I+20))
         D3CAQ(I)=BCAQ(I)+QFACT*(CB3(I+40)-TB3(I+40))
         D3CYQ(I)=BCYQ(I)+QFACT*(CB3(I+60)-TB3(I+60))
         D3CLNQ(I)=BCLNQ(I)+QFACT*(CB3(I+80)-TB3(I+80))
         D3CLLQ(I)=BCLLQ(I)+QFACT*(CB3(I+100)-TB3(I+100))
C
         D4CNQ(I)=BCNQ(I)+QFACT*(CB4(I)-TB4(I))
         D4CMQ(I)=BCMQ(I)+QFACT*(CB4(I+20)-TB4(I+20))
         D4CAQ(I)=BCAQ(I)+QFACT*(CB4(I+40)-TB4(I+40))
         D4CYQ(I)=BCYQ(I)+QFACT*(CB4(I+60)-TB4(I+60))
         D4CLNQ(I)=BCLNQ(I)+QFACT*(CB4(I+80)-TB4(I+80))
         D4CLLQ(I)=BCLLQ(I)+QFACT*(CB4(I+100)-TB4(I+100))
 1400 CONTINUE
C
      DO 1500 I=1,220
         CB1(I)=UNUSED
         CB2(I)=UNUSED
         CB3(I)=UNUSED
         CB4(I)=UNUSED
 1500 CONTINUE
C
C  SKIP YAW AND ROLL CALCULATIONS IF NOLAT PRESENT
C
      IF(NOLAT) GO TO 1950
C
C***  YAW RESULTS
C
      CALL SYNTHS(IM,TOTC,IFLAGA,2.)
C
C***  CALCULATE YAW DERIVATIVES
C
      RFACT=1./RATE
      DO 1600 I=1,NALP
         D1CNR(I)=BCNR(I)+RFACT*(CB1(I)-TB1(I))
         D1CMR(I)=BCMR(I)+RFACT*(CB1(I+20)-TB1(I+20))
         D1CAR(I)=BCAR(I)+RFACT*(CB1(I+40)-TB1(I+40))
         D1CYR(I)=BCYR(I)+RFACT*(CB1(I+60)-TB1(I+60))
         D1CLNR(I)=BCLNR(I)+RFACT*(CB1(I+80)-TB1(I+80))
         D1CLLR(I)=BCLLR(I)+RFACT*(CB1(I+100)-TB1(I+100))
C
         D2CNR(I)=BCNR(I)+RFACT*(CB2(I)-TB2(I))
         D2CMR(I)=BCMR(I)+RFACT*(CB2(I+20)-TB2(I+20))
         D2CAR(I)=BCAR(I)+RFACT*(CB2(I+40)-TB2(I+40))
         D2CYR(I)=BCYR(I)+RFACT*(CB2(I+60)-TB2(I+60))
         D2CLNR(I)=BCLNR(I)+RFACT*(CB2(I+80)-TB2(I+80))
         D2CLLR(I)=BCLLR(I)+RFACT*(CB2(I+100)-TB2(I+100))
C
         D3CNR(I)=BCNR(I)+RFACT*(CB3(I)-TB3(I))
         D3CMR(I)=BCMR(I)+RFACT*(CB3(I+20)-TB3(I+20))
         D3CAR(I)=BCAR(I)+RFACT*(CB3(I+40)-TB3(I+40))
         D3CYR(I)=BCYR(I)+RFACT*(CB3(I+60)-TB3(I+60))
         D3CLNR(I)=BCLNR(I)+RFACT*(CB3(I+80)-TB3(I+80))
         D3CLLR(I)=BCLLR(I)+RFACT*(CB3(I+100)-TB3(I+100))
C
         D4CNR(I)=BCNR(I)+RFACT*(CB4(I)-TB4(I))
         D4CMR(I)=BCMR(I)+RFACT*(CB4(I+20)-TB4(I+20))
         D4CAR(I)=BCAR(I)+RFACT*(CB4(I+40)-TB4(I+40))
         D4CYR(I)=BCYR(I)+RFACT*(CB4(I+60)-TB4(I+60))
         D4CLNR(I)=BCLNR(I)+RFACT*(CB4(I+80)-TB4(I+80))
         D4CLLR(I)=BCLLR(I)+RFACT*(CB4(I+100)-TB4(I+100))
 1600 CONTINUE
C
      DO 1700 I=1,220
         CB1(I)=UNUSED
         CB2(I)=UNUSED
         CB3(I)=UNUSED
         CB4(I)=UNUSED
 1700 CONTINUE
C
C***  ROLL RESULTS
C
      CALL SYNTHS(IM,TOTC,IFLAGA,3.)
C
C***  CALCULATE ROLL DERIVATIVES
C
      PFACT=1./RATE
      DO 1800 I=1,NALP
         D1CNP(I)=BCNP(I)+PFACT*(CB1(I)-TB1(I))
         D1CMP(I)=BCMP(I)+PFACT*(CB1(I+20)-TB1(I+20))
         D1CAP(I)=BCAP(I)+PFACT*(CB1(I+40)-TB1(I+40))
         D1CYP(I)=BCYP(I)+PFACT*(CB1(I+60)-TB1(I+60))
         D1CLNP(I)=BCLNP(I)+PFACT*(CB1(I+80)-TB1(I+80))
         D1CLLP(I)=BCLLP(I)+PFACT*(CB1(I+100)-TB1(I+100))
C
         D2CNP(I)=BCNP(I)+PFACT*(CB2(I)-TB2(I))
         D2CMP(I)=BCMP(I)+PFACT*(CB2(I+20)-TB2(I+20))
         D2CAP(I)=BCAP(I)+PFACT*(CB2(I+40)-TB2(I+40))
         D2CYP(I)=BCYP(I)+PFACT*(CB2(I+60)-TB2(I+60))
         D2CLNP(I)=BCLNP(I)+PFACT*(CB2(I+80)-TB2(I+80))
         D2CLLP(I)=BCLLP(I)+PFACT*(CB2(I+100)-TB2(I+100))
C
         D3CNP(I)=BCNP(I)+PFACT*(CB3(I)-TB3(I))
         D3CMP(I)=BCMP(I)+PFACT*(CB3(I+20)-TB3(I+20))
         D3CAP(I)=BCAP(I)+PFACT*(CB3(I+40)-TB3(I+40))
         D3CYP(I)=BCYP(I)+PFACT*(CB3(I+60)-TB3(I+60))
         D3CLNP(I)=BCLNP(I)+PFACT*(CB3(I+80)-TB3(I+80))
         D3CLLP(I)=BCLLP(I)+PFACT*(CB3(I+100)-TB3(I+100))
C
         D4CNP(I)=BCNP(I)+PFACT*(CB4(I)-TB4(I))
         D4CMP(I)=BCMP(I)+PFACT*(CB4(I+20)-TB4(I+20))
         D4CAP(I)=BCAP(I)+PFACT*(CB4(I+40)-TB4(I+40))
         D4CYP(I)=BCYP(I)+PFACT*(CB4(I+60)-TB4(I+60))
         D4CLNP(I)=BCLNP(I)+PFACT*(CB4(I+80)-TB4(I+80))
         D4CLLP(I)=BCLLP(I)+PFACT*(CB4(I+100)-TB4(I+100))
 1800 CONTINUE
C
 1950 CONTINUE
C      
C***  RESTORE FIRST SYNTHESIS RESULTS (STATIC CALCULATION)
C      
      DO 1900 I=1,220
         CB1(I)=TB1(I)
         CB2(I)=TB2(I)
         CB3(I)=TB3(I)
         CB4(I)=TB4(I)
 1900 CONTINUE
C      
      LPART=LPARTS
      DO 2000 I=1,16
         PARTS(I)=PARTSS(I)
 2000 CONTINUE
C      
      RETURN
      END
