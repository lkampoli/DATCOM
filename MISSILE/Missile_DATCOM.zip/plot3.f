      SUBROUTINE PLOT3(IC,IM,CN,CM,CA,CY,CLN,CLL)
C
C***  SUBROUTINE TO GENERATE A PLOT FILE ON TAPE UNIT 3
C***  FOR POST PROCESSING BY A PLOTTING PROGRAM
C
C  MODIFIED BY B. HEATH, WL/FIGC
C
C***  INPUTS
C
C     IM -- CURRENT VALUE OF MACH NUMBER (INTEGER VALUE)
C     CN -- ARRAY OF NORMAL FORCE DATA
C     CM -- ARRAY OF PITCHING MOMENT DATA
C     CA -- ARRAY OF AXIAL FORCE DATA
C     CY -- ARRAY OF SIDE FORCE DATA
C    CLN -- ARRAY OF YAW MOMENT DATA
C    CLL -- ARRAY OF ROLL MOMENT DATA
C
      COMMON /CASEID/ IDCASE(74),KOUNT,NAMSV(100),CASE,NOEXTR,NOLAT,
     1                IR,IPAGE
      COMMON /REFQN/  SREF,LREF,LATREF,ROUGH,XCG,ZCG,SCALE,BLAYER,RHR
      COMMON /FLC/    NALPHA,ALPHA(20),BETA,PHI,NMACH,MACH(20),
     1                ALT(20),REN(20),VINF(20),TINF(20),PINF(20)
      COMMON /CONST/  PI,RAD,UNUSED,KAND
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
      DIMENSION CN(20),CM(20),CA(20),CY(20),CLN(20),CLL(20)
C
      REAL NALPHA,NMACH,MACH,LREF,LATREF
      CHARACTER*4 PIN,PFT,PCM,PM,IPTYP
C
      DATA PIN / 'I   '/
      DATA PFT / 'F   '/
      DATA PCM / 'C   '/
      DATA PM  / 'M   '/
C
      IR=IR+1
      NALP=NALPHA+0.5
C
      IPTYP=PFT
      IF(LDIMIN)IPTYP=PIN
      IF(LDIMCM)IPTYP=PCM
      IF(LDIMM )IPTYP=PM
C
      CALL CVRTFT
C
C  REV 6/93 AND PRIOR VERSION
C  CODE COMMENTED OUT HERE
C
C     WRITE(3,1010)IR,NALP,IPTYP,IC,CASE
C     WRITE(3,1020)MACH(IM),REN(IM),PHI
C     WRITE(3,1030)SREF,LREF,XCG,LATREF,ZCG
C
C  TECPLOT COMMAND LINE
C
      WRITE(3,1100) MACH(IM)
C
      DO 1000 I=1,NALP
         WRITE(3,1040)ALPHA(I),CN(I),CM(I),CA(I),
     1                CY(I),CLN(I),CLL(I),0.
 1000 CONTINUE
C
C     WRITE(3,1050)
C
      CALL CVRTUS
C
 1010 FORMAT('RUN',I4,I3,2X,A1,A4,6X,'CASE ',I3)
 1020 FORMAT(F10.4,F10.0,F10.4)
 1030 FORMAT(5F10.4)
 1040 FORMAT(8F10.4)
 1050 FORMAT('R')
 1100 FORMAT('ZONE T="NO TRIM MACH=',F6.2,'"')
C
      RETURN
      END
