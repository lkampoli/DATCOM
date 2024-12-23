      SUBROUTINE CCARD(ISEQ,END,KOL)
C
C***  SUBROUTINE TO TEST FOR LEGAL CONTROL CARDS
C
      COMMON /CONST/  PI,RAD,UNUSED,KAND
      COMMON /PARTF/  PARTS(19)
      LOGICAL PARTS
      DIMENSION KOL(80), PART(4), IDIM(3), IDIMT(8), NMLIST(8)
      DIMENSION BUILD(5), NACA(4), DERD(9), DERR(9), KASE(6)
      DIMENSION ISOSE(4),IPRESR(9),IEXTRA(12)
      DIMENSION IWRIE(6),IFORMT(6),INOLAT(6),ICRMT(6),ISPIN(4)
      DIMENSION SAVE(4), NEXT(9), IDMP(5), IDMC(4), TRIM(4), IHYPER(5)
      DIMENSION DAMP(4), NACAT(4), PLOT(4)
      DIMENSION KEYDP(144),  NDP(39)
      DIMENSION KEYARY(40), NDPARY(10)
      DIMENSION KEYINP(39), NDPINP(10)
      DIMENSION KEYIOM(65), NDPIOM(19)
      EQUIVALENCE (KEYDP(  1),KEYARY(1)), (NDP( 1),NDPARY(1))
      EQUIVALENCE (KEYDP( 41),KEYINP(1)), (NDP(11),NDPINP(1))
      EQUIVALENCE (KEYDP( 80),KEYIOM(1)), (NDP(21),NDPIOM(1))
      CHARACTER*4 BUILD, DERD, DERR, SAVE, TRIM, DAMP, PART, PLOT
      CHARACTER*4 IRGEOM,IRAERO,IRTYPE,ISOSE,IEXTRA,IWRIE,IFORMT
      CHARACTER*4 INOLAT,NACA,NACAT,IDIM,IDIMT,NMLIST,KASE,KOL
      CHARACTER*4 IDMP,IDMC,IBLNK,IHYPER,ICRMT,ISPIN,KOMMA,KEYDP
      CHARACTER*4 KEYINP,KEYARY,KEYIOM,IBLANK,NEXT,IPRESR,KAND
      LOGICAL NMTEST,END,DELNMS
C
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
      DATA ISOSE /'S   ','O   ','S   ','E   '/
      DATA IEXTRA/'P   ','R   ','I   ','N   ','T   ','    ',
     1 'E   ','X   ','T   ','R   ','A   ','P   '/
      DATA IPRESR / 'P   ','R   ','E   ','S   ','S   ',
     1 'U   ','R   ','E   ','S   '/
      DATA IWRIE / 'W   ','R   ','I   ','T   ','E   ','    ' /
      DATA IFORMT/ 'F   ','O   ','R   ','M   ','A   ','T   ' /
      DATA INOLAT/ 'N   ','O   ','    ','L   ','A   ','T   ' /
      DATA BUILD  /'B   ', 'U   ', 'I   ', 'L   ', 'D   ' /
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
      DATA SAVE   /'S   ','A   ','V   ','E   ' /
      DATA NEXT   /'N   ','E   ','X   ','T   ','    ','C   ','A   ',
     1             'S   ','E   '/
      DATA IDMP   /'D   ','U   ','M   ','P   ','    '/
      DATA IDMC   /'C   ','A   ','S   ','E   '/
      DATA IBLNK  /'    '/,KOMMA /',   '/
      DATA TRIM   /'T   ','R   ','I   ','M   '/
      DATA DAMP   /'D   ','A   ','M   ','P   '/
      DATA PLOT   /'P   ','L   ','O   ','T   '/
      DATA IHYPER /'H   ','Y   ','P   ','E   ','R   '/
      DATA ICRMT  /'I   ','N   ','C   ','R   ','M   ','T   '/
      DATA ISPIN  /'S   ','P   ','I   ','N   '/
C
      DATA NLN / 39 /
C
      DATA KEYARY  /
     1       'G   ','E   ','O   ','B   ','F   ','1   ','G   ','M   ',
     2       'F   ','2   ','G   ','M   ','F   ','3   ','G   ','M   ',
     3       'F   ','4   ','G   ','M   ','A   ','T   ','M   ','P   ',
     4       'B   ','D   ','W   ','K   ','F   ','L   ','C   ','T   ',
     5       'O   ','I   ','N   ','L   ','I   ','N   ','L   ','D   '/
      DATA NDPARY / 10*4 /
C
      DATA KEYINP  /
     1       'I   ','N   ','P   ','T   ','F   ','L   ','T   ','R   ',
     2       'E   ','F   ','Q   ','B   ','D   ','I   ','N   ','F   ',
     3       '1   ','I   ','N   ','F   ','2   ','I   ','N   ','F   ',
     4       '3   ','I   ','N   ','F   ','4   ','I   ','N   ','I   ',
     5       'N   ','L   ','I   ','D   ','I   ','V   ','I   '/
      DATA NDPINP / 4,3,8*4 /
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
      DATA IBLANK / '    ' /
C
      NA = 0
      NB = 0
      ND = 0
C
C***  TEST FOR DELETE CONTROL CARD
C
         IF(DELNMS(KOL,0,ND))GO TO 1620
C
C***  TEST FOR - NEXT CASE
C
      DO 1000 I=1,9
         IF(KOL(I) .NE. NEXT(I)) GO TO 1010
 1000 CONTINUE
         IF(.NOT.END .AND. PARTS(17))WRITE(6,1690)KAND
         IF(.NOT.END)WRITE(8,1700)KAND,(IBLANK,I=1,75)
         IF(.NOT.END)END=.TRUE.
      GO TO 1620
C
C***  TEST FOR - DUMP
C
 1010 DO 1020 I=1,5
         IF(KOL(I) .NE. IDMP(I)) GO TO 1110
 1020 CONTINUE
C
C***  TEST FOR - DUMP CASE
C
      J=1
      DO 1030 L=6,9
         IF(KOL(L) .NE. IDMC(J)) GO TO 1050
      J=J+1
 1030 CONTINUE
C
C***  TEST FOR COLUMNS 10-80 BLANK
C
      DO 1040 I=10,80
         IF(KOL(I) .NE. IBLNK) GO TO 1050
 1040 CONTINUE
      GO TO 1620
C
C***  TEST FOR ARRAY NAMES
C
 1050 I=5
 1060 I=I+1
         IF(I.GT.80) GO TO 1620
         IF(KOL(I) .EQ. IBLNK .OR. KOL(I) .EQ. KOMMA)GO TO 1060
      NS=I
      NCHAR=1
 1070 I=I+1
         IF(I.GT.80)GO TO 1620
         IF(KOL(I) .EQ. IBLNK .OR. KOL(I) .EQ. KOMMA) GO TO 1080
      NCHAR=NCHAR+1
      GO TO 1070
 1080 IF(NCHAR .LT. 7) GO TO 1090
      NA = NA+1
      GO TO 1060
 1090 LOC = 1
      DO 1100 J=1,NLN
         IF(J .GT. 1) LOC = LOC+NDP(J-1)
         IF(NDP(J) .NE. NCHAR) GO TO 1100
         IF(NMTEST(KOL(NS),KEYDP(LOC),NDP(J))) GO TO 1060
 1100 CONTINUE
      NA = NA+1
      GO TO 1060
C
C***  TEST FOR - SAVE
C
 1110 DO 1120 I=1,4
         IF(KOL(I) .NE. SAVE(I)) GO TO 1130
 1120 CONTINUE
      GO TO 1620
C
C***  TEST FOR - CASEID
C
 1130 DO 1140 I=1,6
         IF(KOL(I) .NE. KASE(I)) GO TO 1150
 1140 CONTINUE
      GO TO 1620
C
C***  TEST FOR - TRIM
C
 1150 DO 1160 I=1,4
         IF(KOL(I) .NE. TRIM(I)) GO TO 1170
 1160 CONTINUE
      GO TO 1620
C
C***  TEST FOR - DAMP
C
 1170 DO 1180 I=1,4
         IF(KOL(I) .NE. DAMP(I)) GO TO 1190
 1180 CONTINUE
      GO TO 1620
C
C***  TEST FOR - SPIN
C
 1190 DO 1200 I=1,4
         IF(KOL(I) .NE. ISPIN(I))GO TO 1210
 1200 CONTINUE
      GO TO 1620
C
C***  TEST FOR - BUILD
C
 1210 DO 1220 I=1,5
         IF(KOL(I) .NE. BUILD(I)) GO TO 1230
 1220 CONTINUE
      GO TO 1620
C
C***  TEST FOR - NACA
C
 1230 DO 1240 I=1,4
         IF(KOL(I) .NE. NACA(I)) GO TO 1260
 1240 CONTINUE
      DO 1250 I=1,4
         IF(KOL(6) .EQ. NACAT(I)) GO TO 1620
 1250 CONTINUE
      NB = 1
      GO TO 1620
C
C***  TEST FOR - DERIV DEG
C
 1260 DO 1270 I=1,9
         IF(KOL(I) .NE. DERD(I)) GO TO 1280
 1270 CONTINUE
      GO TO 1620
C
C***  TEST FOR - DERIV RAD
C
 1280 DO 1290 I=1,9
         IF(KOL(I) .NE. DERR(I)) GO TO 1300
 1290 CONTINUE
      GO TO 1620
C
C***  TEST FOR - PART
C
 1300 DO 1310 I=1,4
         IF(KOL(I) .NE. PART(I)) GO TO 1320
 1310 CONTINUE
      GO TO 1620
C
C***  TEST FOR DIMENSION CARD
C
 1320 DO 1330 I=1,3
         IF(KOL(I) .NE. IDIM(I)) GO TO 1340
 1330 CONTINUE
         IF((KOL(5) .EQ. IDIMT(1) .AND. KOL(6) .EQ. IDIMT(2)) .OR.
     1   (KOL(5) .EQ. IDIMT(3) .AND. KOL(6) .EQ. IDIMT(4)) .OR.
     2   (KOL(5) .EQ. IDIMT(5) .AND. KOL(6) .EQ. IDIMT(6)) .OR.
     3   (KOL(5) .EQ. IDIMT(7) .AND. KOL(6) .EQ. IDIMT(8)))
     4   GO TO 1620
C
C***  TEST FOR NAMELIST CARD
C
 1340 DO 1350 I=1,8
         IF(KOL(I) .NE. NMLIST(I)) GO TO 1360
 1350 CONTINUE
      GO TO 1620
C
C***  TEST FOR PLOT CARD
C
 1360 DO 1370 I=1,4
         IF(KOL(I) .NE. PLOT(I)) GO TO 1380
 1370 CONTINUE
      GO TO 1620
C
C***  TEST FOR SOSE CONTROL CARD
C
 1380 DO 1390 I=1,4
         IF(KOL(I) .NE. ISOSE(I)) GO TO 1400
 1390 CONTINUE
      GO TO 1620
C
C***  CHECK FOR PRESSURES CONTROL CARD
C
 1400 DO 1410 I=1,9
         IF(KOL(I) .NE. IPRESR(I)) GO TO 1420
 1410 CONTINUE
      GO TO 1620
C
C***  PRINT EXTRAPOLATION MESSAGES FLAG
C
 1420 DO 1430 I=1,12
         IF(KOL(I) .NE. IEXTRA(I))GO TO 1440
 1430 CONTINUE
      GO TO 1620
C
C***  WRITE CONTROL CARD
C
 1440 DO 1450 I=1,6
         IF(KOL(I) .NE. IWRIE(I))GO TO 1460
 1450 CONTINUE
      GO TO 1620
C
C
 1460 DO 1470 I=1,6
         IF(KOL(I) .NE. IFORMT(I))GO TO 1480
 1470 CONTINUE
      GO TO 1620
C
C***  NOLAT CONTROL CARD
C
 1480 DO 1490 I=1,6
         IF(KOL(I) .NE. INOLAT(I))GO TO 1500
 1490 CONTINUE
      GO TO 1620
C
C***  CHECK FOR INCRMT CONTROL CARD
C
 1500 DO 1510 I=1,6
         IF(KOL(I) .NE. ICRMT(I))GO TO 1520
 1510 CONTINUE
      GO TO 1620
C
C***  CHECK FOR HYPER CONTROL CARD
C
 1520 DO 1530 I=1,5
         IF(KOL(I) .NE. IHYPER(I))GO TO 1540
 1530 CONTINUE
      GO TO 1620
C
C*** CHECK FOR PRINT GEOM CARD
C
 1540 DO 1550 I=1,10
         IF(KOL(I) .NE. IRGEOM(I))GO TO 1560
 1550 CONTINUE
      IMAX=6
      GO TO 1580
C
C*** CHECK FOR PRINT AERO CARD
C
 1560 DO 1570 I=1,10
         IF(KOL(I) .NE. IRAERO(I))GO TO 1610
 1570 CONTINUE
      IMAX=10
C
 1580 CONTINUE
      DO 1600 I=1,IMAX
         IR1=12
         IR2=IR1+IRLEN(I+1)-IRLEN(I)-1
         DO 1590 II=IR1,IR2
            IND=IRLEN(I)+II-IR1
            IF(KOL(II) .NE. IRTYPE(IND))GO TO 1600
 1590    CONTINUE
         GO TO 1620
 1600 CONTINUE
C
C***  CARD NOT FOUND, CHECK FOR SPECIAL PRINT **** CARDS
C
C ... PRINT AERO + OR PRINT GEOM + SUPPRESSES CONERR OUTPUT
C ... UPON DETECTION STOPS ALL CONERR OUTPUT
C ... NORMALLY SET EXTERNAL TO CONERR
C
      IF(KOL(12).EQ.IRTYPE(45))PARTS(17)=.FALSE.
      IF(KOL(12).EQ.IRTYPE(45))GO TO 1620
C
C ... PRINT AERO - OR PRINT GEOM - SUPPRESSES INPUT LISTING
C
      IF(KOL(12).EQ.IRTYPE(46))PARTS(18)=.FALSE.
      IF(KOL(12).EQ.IRTYPE(46))GO TO 1620
C
C ... PRINT AERO * OR PRINT GEOM * SUPPRESSES ALL NORMAL OUTPUT
C
      IF(KOL(12).EQ.IRTYPE(47))PARTS(19)=.FALSE.
      IF(KOL(12).EQ.IRTYPE(47))GO TO 1620
C
 1610 IF(PARTS(17))WRITE(6,1640) ISEQ,(KOL(I),I=1,80)
      GO TO 1630
C
C***  CARD FOUND
C
 1620 NC = NA+NB
         IF(NA .GT. 0 .AND. PARTS(17))
     1   WRITE(6,1650) ISEQ,(KOL(I),I=1,80), NA
         IF(ND .GT. 0 .AND. PARTS(17))
     1   WRITE(6,1710) ISEQ,(KOL(I),I=1,80), ND
         IF(NB .GT. 0 .AND. PARTS(17))WRITE(6,1660) ISEQ,(KOL(I),I=1,80)
         IF(NC .EQ. 0 .AND. PARTS(17))WRITE(6,1670) ISEQ,(KOL(I),I=1,80)
      WRITE(8,1680) (KOL(I),I=1,80)
C
 1630 CONTINUE
C
      RETURN
C***  FORMAT CONTROL CARD
 1640 FORMAT(1X,I3,1X,80A1,/,
     1 8X,'** ERROR ** UNKNOWN CONTROL CARD - IGNORED')
 1650 FORMAT(1X,I3,1X,80A1,/,8X,'** ERROR **',I4,
     1                  ' INCORRECT ARRAY NAMES')
 1660 FORMAT(1X,I3,1X,80A1,/,8X,'** ERROR ** INCORRECT LIFTING',
     1       ' SURFACE DESIGNATION ON NACA CARD')
 1670 FORMAT(1X,I3,1X,80A1)
 1680 FORMAT(80A1)
 1690 FORMAT(6X,A1,'END',18X,'** MISSING NAMELIST TERMINATION ADDED')
 1700 FORMAT(1X,A1,'END',75A1)
 1710 FORMAT(1X,I3,1X,80A1,/,8X,'** ERROR **',I4,
     1                  ' INCORRECT NAMELIST NAMES')
      END
