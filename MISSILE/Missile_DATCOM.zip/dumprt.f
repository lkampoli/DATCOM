      SUBROUTINE DUMPRT
C
C***  ROUTINE TO DUMP INTERNAL ARRAYS
C
      COMMON /DUMPF/
     1   LGEOB,LF1GM,LF2GM,LF3GM,LF4GM,LATMP,LBDWK,LFLCT,LINLD,
     2   LINPT,LFLTC,LREFQN,LBDIN,LF1IN,LF2IN,LF3IN,LF4IN,LINLIN,

     3   LIOM,LSBOD,LSF1,LSF2,LSF3,LSF4,LSB1,LSB12,LSB123,LS1234,LDBOD,
     4   LDF1,LDF2,LDF3,LDF4,LDB1,LDB12,LDB123,LD1234
      LOGICAL
     1   LGEOB,LF1GM,LF2GM,LF3GM,LF4GM,LATMP,LBDWK,LFLCT,LINLD,
     2   LINPT,LFLTC,LREFQN,LBDIN,LF1IN,LF2IN,LF3IN,LF4IN,LINLIN,
     3   LIOM,LSBOD,LSF1,LSF2,LSF3,LSF4,LSB1,LSB12,LSB123,LS1234,LDBOD,
     4   LDF1,LDF2,LDF3,LDF4,LDB1,LDB12,LDB123,LD1234
C
      LOGICAL LDMP(37),LBODY
      EQUIVALENCE (LDMP(1),LGEOB)
      COMMON /LOGIC/ LDMPCS,LDAMP,LBUILD,LNACA,LDERDG,LDERRD,LPART,
     1             LNAME,LPLOT,LFLT,LREFQ,LAXIS,LFIN1,LFIN2,LFIN3,LFIN4,
     2             LDEFL,LTRIM,LDIMIN,LDIMFT,LDIMCM,LDIMM,LELLB,LINLET,
     3             LEXPR,LICRMT,LSPIN,LARBOD
      LOGICAL      LDMPCS,LDAMP,LBUILD,LNACA,LDERDG,LDERRD,LPART,
     1             LNAME,LPLOT,LFLT,LREFQ,LAXIS,LFIN1,LFIN2,LFIN3,LFIN4,
     2             LDEFL,LTRIM,LDIMIN,LDIMFT,LDIMCM,LDIMM,LELLB,LINLET,
     3             LEXPR,LICRMT,LSPIN,LARBOD
C
      COMMON /CONST/  PI,RAD,UNUSED,KAND
      COMMON /FLC/    FLCT(144)
      COMMON /TOTALC/ ATOTAL(80)
      COMMON /REFQN/  REFF(9)
      COMMON /GEOBOD/ GEOB(47)
      COMMON /ABODIN/ BDIN(881)
      COMMON /BDWORK/ BDW(241)
      COMMON /INLETN/ INLE(64)
      COMMON /SBODY/  CBOD(220)
      COMMON /SFIN1/  CF1(220)
      COMMON /SFIN2/  CF2(220)
      COMMON /SFIN3/  CF3(220)
      COMMON /SFIN4/  CF4(220)
      COMMON /SB1/    CB1(220)
      COMMON /SB12/   CB12(220)
      COMMON /SB123/  CB123(220)
      COMMON /SB1234/ CB1234(220)
      COMMON /DBODY/  DBOD(406)
      COMMON /DB1/    D1(400)
      COMMON /DB12/   D12(400)
      COMMON /DB123/  D123(400)
      COMMON /DB1234/ D1234(400)
C
      COMMON /FSET1/  FDATA1(399)
      COMMON /FSET2/  FDATA2(399)
      COMMON /FSET3/  FDATA3(399)
      COMMON /FSET4/  FDATA4(399)
C
      COMMON /GEOFS1/ GFIN1(188)
      COMMON /GEOFS2/ GFIN2(188)
      COMMON /GEOFS3/ GFIN3(188)
      COMMON /GEOFS4/ GFIN4(188)
C
      COMMON /F1WORK/ F1WK(290)
      COMMON /F2WORK/ F2WK(290)
      COMMON /F3WORK/ F3WK(290)
      COMMON /F4WORK/ F4WK(290)
      COMMON /INLTD/  INLD(120)
C
      REAL INLE,INLD
      CHARACTER*4 IFLT,IREFF,ITOT,IBDIN,IBDW,IGEOB,IFIN1,IFIN2,
     1  IFIN3,IFIN4,IGF1,IGF2,IGF3,IGF4,IF1WK,IF2WK,IF3WK,IF4WK,
     2  ICBOD,ICF1,ICF2,ICF3,ICF4,ICB1,ICB12,ICB123,ICB1234,
     3  IINLD,IINLIN,IDBOD,IDB1,IDB12,IDB123,IDB1234,IC1234,ID1234
C
      DATA IFLT   /'FLT '/
      DATA IREFF  /'REFQ'/
      DATA ITOT   /'FLCT'/
      DATA IBDIN  /'BDIN'/
      DATA IBDW   /'BDWK'/
      DATA IGEOB  /'GEOB'/
      DATA IFIN1  /'F1IN'/
      DATA IFIN2  /'F2IN'/
      DATA IFIN3  /'F3IN'/
      DATA IFIN4  /'F4IN'/
      DATA IGF1   /'F1GM'/
      DATA IGF2   /'F2GM'/
      DATA IGF3   /'F3GM'/
      DATA IGF4   /'F4GM'/
      DATA IF1WK  /'F1WK'/
      DATA IF2WK  /'F2WK'/
      DATA IF3WK  /'F3WK'/
      DATA IF4WK  /'F4WK'/
      DATA ICBOD  /'SBOD'/
      DATA ICF1   /'SF1 '/
      DATA ICF2   /'SF2 '/
      DATA ICF3   /'SF3 '/
      DATA ICF4   /'SF4 '/
      DATA ICB1   /'SB1 '/
      DATA ICB12  /'SB12'/
      DATA ICB123 /'SB13'/
      DATA IC1234 /'SB14'/
      DATA IINLD  /'INLD'/
      DATA IINLIN /'INLI'/
      DATA IDBOD  /'DBOD'/
C     DATA IDF1   /'DF1 '/
C     DATA IDF2   /'DF2 '/
C     DATA IDF3   /'DF3 '/
C     DATA IDF4   /'DF4 '/
      DATA IDB1   /'DB1 '/
      DATA IDB12  /'DB12'/
      DATA IDB123 /'DB13'/
      DATA ID1234 /'DB14'/
C
C***  DETERMINE IF ANY ARRAY DUMPS ARE NECESSARY
C
      IF(LDMPCS)GO TO 1010
C
      DO 1000 I=1,37
         IF(LDMP(I))GO TO 1010
 1000 CONTINUE
C
      GO TO 1020
C
 1010 CONTINUE
C
      WRITE(6,1030)
C
C  FLIGHT CONDITIONS
C
      IF((LDMPCS.AND.LFLT) .OR. LFLTC .OR. LATMP .OR. (LINPT.AND.LFLT))
     1 CALL DMPARY(0,FLCT,125,IFLT,3)
      IF((LDMPCS.AND.LFLT) .OR. LFLCT .OR. (LINPT.AND.LFLT))
     1 CALL DMPARY(0,ATOTAL,80,ITOT,4)
C
C  REFERENCE QUANTITIES
C
      IF((LDMPCS.AND.LREFQ) .OR. LREFQN .OR. (LINPT.AND.LREFQ))
     1 CALL DMPARY(0,REFF,9,IREFF,4)
C
C  AXISYMMETRIC BODY INPUTS
C
      LBODY=LAXIS .OR. LELLB
C
      IF((LDMPCS.AND.LAXIS) .OR. (LBDIN.AND.LAXIS)
     1    .OR. (LINPT.AND.LAXIS)) CALL DMPARY(0,BDIN,881,IBDIN,4)
      IF((LDMPCS.AND.LELLB) .OR. (LBDIN.AND.LELLB)
     1    .OR. (LINPT.AND.LELLB)) CALL DMPARY(0,BDIN,881,IBDIN,4)
C
C  AXISYMMETRIC BODY GEOMETRY
C
      IF((LDMPCS.AND.LAXIS) .OR. LGEOB)
     1 CALL DMPARY(0,GEOB,35,IGEOB,4)
      IF((LDMPCS.AND.LELLB) .OR. LGEOB)
     1 CALL DMPARY(0,GEOB,47,IGEOB,4)
C
C  AXISYMMETRIC BODY WORKING DATA ARRAYS
C
      IF((LDMPCS.AND.LBODY) .OR. LGEOB .OR. LBDWK)
     1 CALL DMPARY(0,BDW,241,IBDW,4)
C
C  FIN SET 1 INPUTS
C
      IF((LDMPCS.AND.LFIN1) .OR. LF1IN .OR. (LINPT.AND.LFIN1))
     1 CALL DMPARY(0,FDATA1,399,IFIN1,4)
C
C  FIN SET 2 INPUTS
C
      IF((LDMPCS.AND.LFIN2) .OR. LF2IN .OR. (LINPT.AND.LFIN2))
     1 CALL DMPARY(0,FDATA2,399,IFIN2,4)
C
C  FIN SET 3 INPUTS
C
      IF((LDMPCS.AND.LFIN3) .OR. LF3IN .OR. (LINPT.AND.LFIN3))
     1 CALL DMPARY(0,FDATA3,399,IFIN3,4)
C
C  FIN SET 4 INPUTS
C
      IF((LDMPCS.AND.LFIN4) .OR. LF4IN .OR. (LINPT.AND.LFIN4))
     1 CALL DMPARY(0,FDATA4,399,IFIN4,4)
C
C  FIN SET 1 GEOMETRY
C
      IF((LDMPCS.AND.LFIN1) .OR. LF1GM)
     1 CALL DMPARY(0,GFIN1,188,IGF1,4)
      IF((LDMPCS.AND.LFIN1) .OR. LF1GM)
     1 CALL DMPARY(0,F1WK,290,IF1WK,4)
C
C  FIN SET 2 GEOMETRY
C
      IF((LDMPCS.AND.LFIN2) .OR. LF2GM)
     1 CALL DMPARY(0,GFIN2,188,IGF2,4)
      IF((LDMPCS.AND.LFIN2) .OR. LF2GM)
     1 CALL DMPARY(0,F2WK,290,IF2WK,4)
C
C  FIN SET 3 GEOMETRY
C
      IF((LDMPCS.AND.LFIN3) .OR. LF3GM)
     1 CALL DMPARY(0,GFIN3,188,IGF3,4)
      IF((LDMPCS.AND.LFIN3) .OR. LF3GM)
     1 CALL DMPARY(0,F3WK,290,IF3WK,4)
C
C  FIN SET 4 GEOMETRY
C
      IF((LDMPCS.AND.LFIN4) .OR. LF4GM)
     1 CALL DMPARY(0,GFIN4,188,IGF4,4)
      IF((LDMPCS.AND.LFIN4) .OR. LF4GM)
     1 CALL DMPARY(0,F4WK,290,IF4WK,4)
C
C  AXISYMMETRIC BODY I.O.M.
C
      IF((LDMPCS.AND.LBODY) .OR. LSBOD .OR. (LIOM.AND.LBODY))
     1 CALL DMPARY(0,CBOD,220,ICBOD,4)
C
C  FIN SET 1 I.O.M.
C
      IF((LDMPCS.AND.LFIN1) .OR. LSF1 .OR. (LIOM.AND.LFIN1))
     1 CALL DMPARY(0,CF1,220,ICF1,4)
C
C  FIN SET 2 I.O.M.
C
      IF((LDMPCS.AND.LFIN2) .OR. LSF2 .OR. (LIOM.AND.LFIN2))
     1 CALL DMPARY(0,CF2,220,ICF2,4)
C
C  FIN SET 3 I.O.M.
C
      IF((LDMPCS.AND.LFIN3) .OR. LSF3 .OR. (LIOM.AND.LFIN3))
     1 CALL DMPARY(0,CF3,220,ICF3,4)
C
C  FIN SET 4 I.O.M.
C
      IF((LDMPCS.AND.LFIN4) .OR. LSF4 .OR. (LIOM.AND.LFIN4))
     1 CALL DMPARY(0,CF4,220,ICF4,4)
C
C  BODY-FIN SET 1 I.O.M.
C
      IF((LDMPCS.AND.LBODY.AND.LFIN1) .OR. LSB1 .OR.
     1 (LIOM.AND.LBODY.AND.LFIN1))
     2 CALL DMPARY(0,CB1,220,ICB1,4)
C
C  BODY-FIN SET 1-FIN SET 2 I.O.M.
C
      IF((LDMPCS.AND.LBODY.AND.LFIN1.AND.LFIN2) .OR. LSB12 .OR.
     1 (LIOM.AND.LBODY.AND.LFIN1.AND.LFIN2))
     2 CALL DMPARY(0,CB12,220,ICB12,4)
C
C  BODY + FIN SET 1,2 AND 3  I.O.M.
C
      IF((LDMPCS.AND.LBODY.AND.LFIN1.AND.LFIN2.AND.LFIN3) .OR. LSB123
     1  .OR. (LIOM.AND.LBODY.AND.LFIN1.AND.LFIN2.AND.LFIN3))
     2 CALL DMPARY(0,CB123,220,ICB123,4)
C
C  BODY + FIN SET 1,2,3 AND 4  I.O.M.
C
      IF((LDMPCS.AND.LBODY.AND.LFIN1.AND.LFIN2.AND.LFIN3.AND.LFIN4)
     1  .OR. LS1234 .OR. (LIOM.AND.LBODY.AND.LFIN1.AND.LFIN2.AND.
     2   LFIN3.AND.LFIN4))
     3 CALL DMPARY(0,CB1234,220,IC1234,4)
C
C  BODY DYNAMIC DERIVATIVES
C
      IF((LDMPCS.AND.LBODY.AND.LDAMP) .OR. (LDBOD.AND.LDAMP.AND.LBODY))
     1   CALL DMPARY(0,DBOD,406,IDBOD,4)
C
C  BODY + FIN SET 1 DYNAMIC DERIVATIVES
C
      IF((LDMPCS.AND.LBODY.AND.LFIN1.AND.LDAMP) .OR.
     1  (LDB1.AND.LBODY.AND.LFIN1.AND.LDAMP))
     2  CALL DMPARY(0,D1,400,IDB1,4)
C
C  BODY + FIN SET 1 AND 2 DYNAMIC DERIVATIVES
C
      IF((LDMPCS.AND.LBODY.AND.LFIN1.AND.LFIN2.AND.LDAMP) .OR.
     1  (LDB12.AND.LBODY.AND.LFIN1.AND.LFIN2.AND.LDAMP))
     2  CALL DMPARY(0,D12,400,IDB12,4)
C
C  BODY + FIN SET 1,2 AND 3 DYNAMIC DERIVATIVES
C
      IF((LDMPCS.AND.LBODY.AND.LFIN1.AND.LFIN2.AND.LFIN3.AND.LDAMP) .OR.
     1  (LDB123.AND.LBODY.AND.LFIN1.AND.LFIN2.AND.LFIN3.AND.LDAMP))
     2  CALL DMPARY(0,D123,400,IDB123,4)
C
C  BODY + FIN SET 1,2,3 AND 4 DYNAMIC DERIVATIVES
C
      IF((LDMPCS.AND.LBODY.AND.LFIN1.AND.LFIN2.AND.LFIN3.AND.LFIN4.AND.
     1  LDAMP) .OR.(LD1234.AND.LBODY.AND.LFIN1.AND.LFIN2.AND.LFIN3.AND.
     2  LFIN4.AND.LDAMP))
     2  CALL DMPARY(0,D1234,400,ID1234,4)
C
C  INLET INPUTS
C
      IF((LDMPCS.AND.LINLET) .OR. (LINLIN .AND. LINLET))
     1   CALL DMPARY(0,INLE,64,IINLIN,4)
C
C  INLET INCREMENTALS
C
      IF((LDMPCS.AND.LINLET) .OR. (LINLD.AND.LINLET))
     1    CALL DMPARY(0,INLD,100,IINLD,4)
C
 1020 CONTINUE
C
 1030 FORMAT(4X,'DUMP OF INTERNAL DATA ARRAYS IN FOOT-POUND-RANKINE',
     1  ' UNITS')
C
      RETURN
      END
