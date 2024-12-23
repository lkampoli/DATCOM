      SUBROUTINE CONTRL(KOL,CASE,SAVCSE,IWARN)
C
C***  SUBROUTINE TO TEST FOR LEGAL CONTROL CARDS
C***  AND SET APPROPIATE CONTROL FLAGS
C
      COMMON /CASEID/ IDCASE(74),KOUNT,NAMSV(100),CCASE,NOEXTR,NOLAT,
     1                IR,IPAGE
      COMMON /THERY/  LSOSE,PRESUR,LHYBRD,LHYPER
      LOGICAL LSOSE,PRESUR,LHYBRD,LHYPER,NOEXTR,NOLAT
      COMMON /PARTF/ PARTS(19)
      LOGICAL PARTS
      COMMON /LOGIC/ LDMPCS,LDAMP,LBUILD,LNACA,LDERDG,
     1               LDERRD,LPART,LNAME,LPLOT,
     2               LFLT,LREFQ,LAXIS,LFIN1,LFIN2,LFIN3,LFIN4,
     3               LDEFL,LTRIM,LDIMIN,LDIMFT,LDIMCM,LDIMM,LELLB,
     4               LINLET,LEXPR,LICRMT,LSPIN,LARBOD
      LOGICAL        LDMPCS,LDAMP,LBUILD,LNACA,LDERDG,
     1               LDERRD,LPART,LNAME,LPLOT,
     2               LFLT,LREFQ,LAXIS,LFIN1,LFIN2,LFIN3,LFIN4,
     3               LDEFL,LTRIM,LDIMIN,LDIMFT,LDIMCM,LDIMM,LELLB,
     4               LINLET,LEXPR,LICRMT,LSPIN,LARBOD,PROTUB
C
      LOGICAL SAVCSE,IWARN
C
      COMMON /DESIG/ NNACA(80,4)
      COMMON /DUMPF/
     1   LGEOB,LF1GM,LF2GM,LF3GM,LF4GM,LATMP,LBDWK,LFLCT,LINLD,
     2   LINPT,LFLTC,LREFQN,LBDIN,LF1IN,LF2IN,LF3IN,LF4IN,LINLEN,
     3   LIOM,LSBOD,LSF1,LSF2,LSF3,LSF4,LSB1,LSB12,LSB123,LS1234,LBOD,
     4   LDF1,LDF2,LDF3,LDF4,LDB1,LDB12,LDB123,LD1234
      LOGICAL
     1   LGEOB,LF1GM,LF2GM,LF3GM,LF4GM,LATMP,LBDWK,LFLCT,LINLD,
     2   LINPT,LFLTC,LREFQN,LBDIN,LF1IN,LF2IN,LF3IN,LF4IN,LINLEN,
     3   LIOM,LSBOD,LSF1,LSF2,LSF3,LSF4,LSB1,LSB12,LSB123,LS1234,LBOD,
     4   LDF1,LDF2,LDF3,LDF4,LDB1,LDB12,LDB123,LD1234
      COMMON /DFLAGS/ DFLT,DREF,DAXI,DFIN1,DFIN2,DFIN3,DFIN4,DDEFL,
     1                DTRIM,DELLB,DINLET,DARBOD
      LOGICAL DFLT,DREF,DAXI,DFIN1,DFIN2,DFIN3,DFIN4,DDEFL,DTRIM
      LOGICAL LDMP(37),LTEST
      EQUIVALENCE (LDMP(1),LGEOB)
      INTEGER CASE
      DIMENSION KOL(80), PART(4), IDIM(3), IDIMT(8), NMLIST(8)
      DIMENSION ISOSE(4),IPRESR(9),IEXTRA(12)
      DIMENSION IWRIE(6),IFORMT(6),INOLAT(6)
      DIMENSION BUILD(5), NACA(4), DERD(9), DERR(9), KASE(6)
      DIMENSION IDMP(5), IDMC(4), TRIM(4), IHYPER(5),ICRMT(6)
      DIMENSION DAMP(4), NACAT(4), PLOT(4),ISPIN(4)
C
      DIMENSION KEYDP(136),  NDP(37)
      DIMENSION KEYARY(36), NDPARY(9)
      DIMENSION KEYINP(35), NDPINP(9)
      DIMENSION KEYIOM(65), NDPIOM(19)
      EQUIVALENCE (KEYDP(  1),KEYARY(1)), (NDP( 1),NDPARY(1))
      EQUIVALENCE (KEYDP( 37),KEYINP(1)), (NDP(10),NDPINP(1))
      EQUIVALENCE (KEYDP( 72),KEYIOM(1)), (NDP(19),NDPIOM(1))
C
C     INTEGER BUILD, DERD, DERR, TRIM, DAMP, PART, PLOT
      CHARACTER*4 IRGEOM,IRAERO,IRTYPE,BUILD,ISOSE,IEXTRA,IPRESR,
     1  IWRIE,IFORMT,INOLAT,NACA,NACAT,DERD,DERR,PART,IDIM,IDIMT,
     2  NMLIST,KASE,IDMP,IDMC,IBLNK,KOMMA,TRIM,DAMP,PLOT,IHYPER,
     3  ICRMT,ISPIN,KEYARY,KEYINP,KEYIOM,KOL,NNACA,IDCASE,KEYDP
C
      LOGICAL NMTEST,DELNMS
      DIMENSION IRGEOM(10),IRAERO(10),IRTYPE(47),IRLEN(11)
      DATA IRLEN / 1,5,9,13,17,21,26,32,36,40,45 /
      DATA IRGEOM /
     1 'P   ','R   ','I   ','N   ','T   ','    ',
     2 'G   ','E   ','O   ','M   '/
      DATA IRAERO /
     1 'P   ','R   ','I   ','N   ','T   ','    ',
     2 'A   ','E   ','R   ','O   '/
      DATA IRTYPE /
     1 'B   ','O   ','D   ','Y   ',
     2 'F   ','I   ','N   ','1   ',
     3 'F   ','I   ','N   ','2   ',
     4 'F   ','I   ','N   ','3   ',
     5 'F   ','I   ','N   ','4   ',
     6 'I   ','N   ','L   ','E   ','T   ',
     7 'S   ','Y   ','N   ','T   ','H   ','S   ',
     8 'T   ','R   ','I   ','M   ',
     9 'B   ','E   ','N   ','D   ',
     A 'H   ','I   ','N   ','G   ','E   ','+   ','-   ','*   '/
C
      DATA BUILD  /'B   ', 'U   ', 'I   ', 'L   ', 'D   ' /
      DATA ISOSE /'S   ','O   ','S   ','E   '/
      DATA IEXTRA/'P   ','R   ','I   ','N   ','T   ','    ',
     1 'E   ','X   ','T   ','R   ','A   ','P   '/
      DATA IPRESR / 'P   ','R   ','E   ','S   ','S   ',
     1 'U   ','R   ','E   ','S   '/
      DATA IWRIE / 'W   ','R   ','I   ','T   ','E   ','    ' /
      DATA IFORMT/ 'F   ','O   ','R   ','M   ','A   ','T   ' /
      DATA INOLAT/ 'N   ','O   ','    ','L   ','A   ','T   ' /
      DATA NACA   /'N   ', 'A   ', 'C   ', 'A   ' /
      DATA NACAT  /'1   ', '2   ', '3   ', '4   '/
      DATA DERD   /'D   ','E   ','R   ','I   ','V   ','    ',
     1             'D   ','E   ','G   '/
      DATA DERR   /'D   ','E   ','R   ','I   ','V   ','    ',
     1             'R   ','A   ','D   '/
      DATA PART   /'P   ','A   ','R   ','T   '/
      DATA IDIM   /'D   ','I   ','M   ' /
      DATA IDIMT  /'F   ','T   ','I   ','N   ','M   ','    ',
     1             'C   ','M   '/
      DATA NMLIST /'N   ','A   ','M   ','E   ','L   ',
     1             'I   ','S   ','T   '/
      DATA KASE   /'C   ','A   ','S   ','E   ','I   ','D   '/
      DATA IDMP   /'D   ','U   ','M   ','P   ','    '/
      DATA IDMC   /'C   ','A   ','S   ','E   '/
      DATA IBLNK  /'    '/
      DATA KOMMA /',   '/
      DATA TRIM   /'T   ','R   ','I   ','M   '/
      DATA DAMP   /'D   ','A   ','M   ','P   '/
      DATA PLOT   /'P   ','L   ','O   ','T   '/
      DATA IHYPER /'H   ','Y   ','P   ','E   ','R   '/
      DATA ICRMT  /'I   ','N   ','C   ','R   ','M   ','T   '/
      DATA ISPIN  /'S   ','P   ','I   ','N   '/
C
      DATA NLN / 37 /
C
      DATA KEYARY  /
     1       'G   ','E   ','O   ','B   ','F   ','1   ','G   ','M   ',
     2       'F   ','2   ','G   ','M   ','F   ','3   ','G   ','M   ',
     3       'F   ','4   ','G   ','M   ','A   ','T   ','M   ','P   ',
     4       'B   ','D   ','W   ','K   ','F   ','L   ','C   ','T   ',
     5       'I   ','N   ','L   ','D   '/
      DATA NDPARY / 9*4 /
C
      DATA KEYINP  /
     1       'I   ','N   ','P   ','T   ','F   ','L   ','T   ','R   ',
     2       'E   ','F   ','Q   ','B   ','D   ','I   ','N   ','F   ',
     3       '1   ','I   ','N   ','F   ','2   ','I   ','N   ','F   ',
     4       '3   ','I   ','N   ','F   ','4   ','I   ','N   ','I   ',
     5       'N   ','L   ','I   '/
      DATA NDPINP / 4,3,7*4 /
C
      DATA KEYIOM  /
     1       'I   ','O   ','M   ','S   ','B   ','O   ','D   ','S   ',
     2       'F   ','1   ','S   ','F   ','2   ','S   ','F   ','3   ',
     3       'S   ','F   ','4   ','S   ','B   ','1   ','S   ','B   ',
     4       '1   ','2   ','S   ','B   ','1   ','3   ','S   ','B   ',
     5       '1   ','4   ','D   ','B   ','O   ','D   ','D   ','F   ',
     6       '1   ','D   ','F   ','2   ','D   ','F   ','3   ','D   ',
     7       'F   ','4   ','D   ','B   ','1   ','D   ','B   ','1   ',
     8       '2   ','D   ','B   ','1   ','3   ','D   ','B   ','1   ',
     9       '4   '/
      DATA NDPIOM / 3,4,5*3,4*4,5*3,3*4 /
C
C***  TEST FOR DELETE CONTROL CARD
C
      ND=0
      IF(DELNMS(KOL,1,ND))GO TO 1600
C
C***  TEST FOR - DUMP
C
      DO 1000 I=1,5
         IF(KOL(I) .NE. IDMP(I)) GO TO 1090
 1000 CONTINUE
C
C***  TEST FOR - DUMP CASE
C
      J=1
      DO 1010 L=6,9
      IF(KOL(L) .NE. IDMC(J)) GO TO 1030
      J=J+1
 1010 CONTINUE
C
C***  TEST FOR COLUMNS 10-80 BLANK
C
      DO 1020 I=10,80
      IF(KOL(I) .NE. IBLNK) GO TO 1030
 1020 CONTINUE
      LDMPCS=.TRUE.
      LPART=.TRUE.
      GO TO 1600
C
C***  TEST FOR ARRAY NAMES
C
 1030 I=5
 1040 I=I+1
      IF(I.GT.80) GO TO 1600
      IF(KOL(I) .EQ. IBLNK .OR. KOL(I) .EQ. KOMMA)GO TO 1040
      NS=I
      NCHAR=1
 1050 I=I+1
      IF(I.GT.80)GO TO 1600
      IF(KOL(I) .EQ. IBLNK .OR. KOL(I) .EQ. KOMMA) GO TO 1060
      NCHAR=NCHAR+1
      GO TO 1050
 1060 IF(NCHAR .LT. 7) GO TO 1070
      NA = NA+1
      GO TO 1040
 1070 LOC = 1
      DO 1080 J=1,NLN
         IF(J .GT. 1) LOC = LOC+NDP(J-1)
         IF(NDP(J) .NE. NCHAR) GO TO 1080
         LTEST=NMTEST(KOL(NS),KEYDP(LOC),NDP(J))
         IF(LTEST)LDMP(J)=.TRUE.
         IF(LTEST)GO TO 1040
 1080 CONTINUE
      NA = NA+1
      GO TO 1040
C
C***  TEST FOR - CASEID
C
 1090 DO 1100 I=1,6
         IF(KOL(I) .NE. KASE(I)) GO TO 1120
 1100 CONTINUE
      DO 1110 I=7,80
         IDCASE(I-6)=KOL(I)
 1110 CONTINUE
      GO TO 1600
C
C***  TEST FOR - TRIM
C
 1120 DO 1130 I=1,4
         IF(KOL(I) .NE. TRIM(I)) GO TO 1140
 1130 CONTINUE
      LTRIM=.TRUE.
      GO TO 1600
C
C***  TEST FOR - DAMP
C
 1140 DO 1150 I=1,4
         IF(KOL(I) .NE. DAMP(I)) GO TO 1160
 1150 CONTINUE
      LDAMP=.TRUE.
      GO TO 1600
C
C***  TEST FOR - SPIN
C
 1160 DO 1170 I=1,4
         IF(KOL(I) .NE. ISPIN(I))GO TO 1180
 1170 CONTINUE
      LSPIN=.TRUE.
      GO TO 1600
C
C***  TEST FOR - BUILD
C
 1180 DO 1190 I=1,5
         IF(KOL(I) .NE. BUILD(I)) GO TO 1200
 1190 CONTINUE
      LBUILD=.TRUE.
      GO TO 1600
C
C***  TEST FOR - INCRMT
C
 1200 DO 1210 I=1,6
         IF(KOL(I) .NE. ICRMT(I))GO TO 1220
 1210 CONTINUE
      LICRMT=.TRUE.
      GO TO 1600
C
C***  TEST FOR - NACA
C
 1220 DO 1230 I=1,4
         IF(KOL(I) .NE. NACA(I)) GO TO 1270
 1230 CONTINUE
      LNACA=.TRUE.
      DO 1240 I=1,4
         NSET=I
         IF(KOL(6) .EQ. NACAT(I)) GO TO 1250
 1240 CONTINUE
      GO TO 1600
 1250 DO 1260 I=1,80
         NNACA(I,NSET)=KOL(I)
 1260 CONTINUE
      GO TO 1600
C
C***  TEST FOR - DERIV DEG
C
 1270 DO 1280 I=1,9
         IF(KOL(I) .NE. DERD(I)) GO TO 1290
 1280 CONTINUE
      LDERDG=.TRUE.
      GO TO 1600
C
C***  TEST FOR - DERIV RAD
C
 1290 DO 1300 I=1,9
         IF(KOL(I) .NE. DERR(I)) GO TO 1310
 1300 CONTINUE
      LDERRD=.TRUE.
      GO TO 1600
C
C***  TEST FOR - PART
C
 1310 DO 1320 I=1,4
         IF(KOL(I) .NE. PART(I)) GO TO 1330
 1320 CONTINUE
      LPART=.TRUE.
      GO TO 1600
C
C***  TEST FOR DIMENSION CARD
C
 1330 DO 1340 I=1,3
         IF(KOL(I) .NE. IDIM(I)) GO TO 1350
 1340 CONTINUE
C
C***  KEEP PRESENT UNITS SYSTEM TO COMPARE LATER
C
      LUNIT=0
      IF(LDIMFT)LUNIT=1
      IF(LDIMIN)LUNIT=2
      IF(LDIMM )LUNIT=3
      IF(LDIMCM)LUNIT=4
      IF(LUNIT .EQ. 0)LUNIT=1
C
C***  NEGATE ANY PREVIOUSLY SET UNITS SYSTEM
C
      LDIMFT=.FALSE.
      LDIMIN=.FALSE.
      LDIMM=.FALSE.
      LDIMCM=.FALSE.
C
C***  SET CORRECT UNITS SYSTEM
C
      IF(KOL(5) .EQ. IDIMT(1) .AND. KOL(6) .EQ. IDIMT(2))LDIMFT=.TRUE.
      IF(KOL(5) .EQ. IDIMT(3) .AND. KOL(6) .EQ. IDIMT(4))LDIMIN=.TRUE.
      IF(KOL(5) .EQ. IDIMT(5) .AND. KOL(6) .EQ. IDIMT(6))LDIMM=.TRUE.
      IF(KOL(5) .EQ. IDIMT(7) .AND. KOL(6) .EQ. IDIMT(8))LDIMCM=.TRUE.
C
      IF(LDIMFT)MUNIT=1
      IF(LDIMIN)MUNIT=2
      IF(LDIMM )MUNIT=3
      IF(LDIMCM)MUNIT=4
      IF(LUNIT .NE. MUNIT .AND. SAVCSE)IWARN=.TRUE.
C
      IF(LDIMFT .OR. LDIMM .OR. LDIMIN .OR. LDIMCM)GO TO 1600
C
C***  IF UNRECOGNIZABLE SET DEFAULT
C
      LDIMFT=.TRUE.
      GO TO 1600
C
C***  TEST FOR NAMELIST CARD
C
 1350 DO 1360 I=1,8
         IF(KOL(I) .NE. NMLIST(I)) GO TO 1370
 1360 CONTINUE
      LNAME=.TRUE.
      GO TO 1600
C
C***  TEST FOR PLOT CARD
C
 1370 DO 1380 I=1,4
         IF(KOL(I) .NE. PLOT(I)) GO TO 1390
 1380 CONTINUE
      LPLOT=.TRUE.
      GO TO 1600
C
C***  TEST FOR SOSE CONTROL CARD
C
 1390 DO 1400 I=1,4
         IF(KOL(I) .NE. ISOSE(I)) GO TO 1410
 1400 CONTINUE
      LSOSE=.TRUE.
      LHYBRD=.FALSE.
      LHYPER=.FALSE.
      GO TO 1600
C
C***  TEST FOR PRESSURES CONTROL CARD
C
 1410 DO 1420 I=1,9
         IF(KOL(I) .NE. IPRESR(I)) GO TO 1430
 1420 CONTINUE
      PRESUR=.TRUE.
      GO TO 1600
C
C***  PRINT EXTRAP CONTROL CARD
C
 1430 DO 1440 I=1,12
         IF(KOL(I) .NE. IEXTRA(I))GO TO 1450
 1440 CONTINUE
      NOEXTR=.FALSE.
      GO TO 1600
C
C***  WRITE CONTROL CARD
C
 1450 DO 1460 I=1,6
         IF(KOL(I) .NE. IWRIE(I))GO TO 1470
 1460 CONTINUE
      WRITE(7,1610)(KOL(I),I=7,80)
      GO TO 1600
C
C
 1470 DO 1480 I=1,6
         IF(KOL(I) .NE. IFORMT(I))GO TO 1490
 1480 CONTINUE
      WRITE(7,1610)(KOL(I),I=7,80)
      GO TO 1600
C
C***  NOLAT CONTROL CARD
C
 1490 DO 1500 I=1,6
         IF(KOL(I) .NE. INOLAT(I))GO TO 1510
 1500 CONTINUE
      NOLAT=.TRUE.
      GO TO 1600
C
C*** HYPER CONTROL CARD
C
 1510 DO 1520 I=1,5
         IF(KOL(I) .NE. IHYPER(I))GO TO 1530
 1520 CONTINUE
      LHYPER=.TRUE.
      LSOSE=.FALSE.
      LHYBRD=.FALSE.
      GO TO 1600
C
C*** PRINT GEOM CARD
C
 1530 DO 1540 I=1,10
         IF(KOL(I) .NE. IRGEOM(I))GO TO 1550
 1540 CONTINUE
      IMAX=6
      GO TO 1570
C
C*** PRINT AERO CARD
C
 1550 DO 1560 I=1,10
         IF(KOL(I) .NE. IRAERO(I))GO TO 1600
 1560 CONTINUE
      IMAX=10
C
C ... NOW FIND TYPE MATCH
C
 1570 DO 1590 I=1,IMAX
         IR1=12
         IR2=IR1+IRLEN(I+1)-IRLEN(I)-1
         DO 1580 II=IR1,IR2
            IND=IRLEN(I)+II-IR1
            IF(KOL(II) .NE. IRTYPE(IND))GO TO 1590
 1580    CONTINUE
C
C ... SET APPROPIATE FLAG
C
         JJ=I
         IF(I .GT. 6)JJ=I-1
         IF(I .EQ. 6 .AND. IMAX .EQ. 6)JJ=15
         IF(I .EQ. 6 .AND. IMAX .EQ. 10)JJ=16
         IF(I .NE. 6 .AND. IMAX .EQ. 10)JJ=JJ+5
         PARTS(JJ)=.TRUE.
         GO TO 1600
C
 1590 CONTINUE
C
C ... PRINT AERO - OR PRINT GEOM - SUPPRESSES INPUT LISTING
C
      IF(KOL(12).EQ.IRTYPE(46))PARTS(18)=.FALSE.
C
C ... PRINT AERO * OR PRINT GEOM * SUPPRESSES ALL NORMAL OUTPUT
C
      IF(KOL(12).EQ.IRTYPE(47))PARTS(19)=.FALSE.
C
 1600 CONTINUE
C
      RETURN
C***  FORMAT CONTROL CARD
 1610 FORMAT(74A1)
      END
