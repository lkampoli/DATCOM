      SUBROUTINE SAVEF
C      
C***  SAVE USER DEFINED DATA TO TAPE3
C      
C  MODIFIED BY W. BLAKE, WL/FIGC
C
      COMMON /CASEID/ IDCASE(180)
      COMMON /THERY/  LTHERY(4)
      COMMON /CONST/  CONS(4)
      COMMON /TRACE/  TTRACE(21)
      COMMON /LOGIC/  LFLAGS(28)
      COMMON /DUMPF/  LDUMPF(37)
      COMMON /DFLAGS/ ADELF(12)
      COMMON /DESIG/  NNACA(320)
      COMMON /FLC/    FLTC(144)
      COMMON /TOTALC/ ATOTAL(80)
      COMMON /REFQN/  REFS(9)
      COMMON /ABODIN/ BDIN(881)
      COMMON /FSET1/  FDATA1(399)
      COMMON /FSET2/  FDATA2(399)
      COMMON /FSET3/  FDATA3(399)
      COMMON /FSET4/  FDATA4(399)
      COMMON /INCID/  DELTAS(40)
      COMMON /GEOBOD/ BDGEO(47)
      COMMON /BDWORK/ BDW(241)
      COMMON /F1WORK/ F1WK(290)
      COMMON /F2WORK/ F2WK(290)
      COMMON /F3WORK/ F3WK(290)
      COMMON /F4WORK/ F4WK(290)
      COMMON /GEOFS1/ GFIN1(188)
      COMMON /GEOFS2/ GFIN2(188)
      COMMON /GEOFS3/ GFIN3(188)
      COMMON /GEOFS4/ GFIN4(188)
      COMMON /SBODY/  CBODY(220)
      COMMON /SFIN1/  CFIN1(220)
      COMMON /SFIN2/  CFIN2(220)
      COMMON /SFIN3/  CFIN3(220)
      COMMON /SFIN4/  CFIN4(220)
      COMMON /SB1/    CBF1(220)
      COMMON /SB12/   CBF2(220)
      COMMON /SB123/  CBF1F2(220)
      COMMON /DBODY/  EBODY(406)
      COMMON /DDFIN1/  EFIN1(80)
      COMMON /DDFIN2/  EFIN2(80)
      COMMON /DB1/    EBF1(400)
      COMMON /DB12/   EBF2(400)
      COMMON /DB123/  EBF1F2(400)
      COMMON /DB1234/ EB1234(400)
      COMMON /TRIMIN/ DTRIM(19)
      COMMON /UTRIMD/ DATAX(1200)
      COMMON /TRIMD/  TTRIM(120)
      COMMON /INPCON/ ACONS(78)
      COMMON /PAERO/  PAER(4024)
      COMMON /SB1234/ CB1234(220)
      COMMON /DDFIN3/  EFIN3(80)
      COMMON /DDFIN4/  EFIN4(80)
      COMMON /INLETN/ INLE(64)
      COMMON /INLTD/  DINLET(120)
C      
      REAL INLE
      LOGICAL LTHERY,LFLAGS,LDUMPF,ADELF
C
      CHARACTER*4 IFORM,IBLOCK,LEFT,IBLANK,ISET
      CHARACTER*4 ISET1,ISET2,ISET3,ISET4,ISET5
C      
      DIMENSION IBLOCK(74),IFORM(74),ISET1(66),ISET2(66),ISET3(66),
     1          ISET4(66),ISET5(54),ISET(318),LASTV(53)
C      
      EQUIVALENCE (ISET1(1),ISET(1)),(ISET2(1),ISET(67)),
     1            (ISET3(1),ISET(133)),(ISET4(1),ISET(199)),
     2            (ISET5(1),ISET(265))
C      
      DATA LEFT   /'(   '/
      DATA IBLANK /'    '/
      DATA IFORM  /'(   ','8   ','F   ','1   ','0   ','.   ',
     1             '4   ',')   ',66*'    '/
C      
      DATA LASTV / 180,4,4,21,28,37,12,320,144,80,9,881,4*399,
     1 40,47,241,4*290,4*188,8*220,406,80,80,4*400,
     2 19,1200,120,78,881,4024,220,80,80,64,100 /
C      
      DATA ISET1 /
     1  'C   ','A   ','S   ','E   ','I   ','D   ',
     2  'T   ','H   ','E   ','R   ','Y   ','    ',
     3  'C   ','O   ','N   ','S   ','T   ','    ',
     4  'T   ','R   ','A   ','C   ','E   ','    ',
     5  'L   ','O   ','G   ','I   ','C   ','    ',
     6  'D   ','U   ','M   ','P   ','F   ','    ',
     7  'D   ','F   ','L   ','A   ','G   ','S   ',
     8  'D   ','E   ','S   ','I   ','G   ','    ',
     9  'F   ','L   ','C   ','    ','    ','    ',
     A  'T   ','O   ','T   ','A   ','L   ','C   ',
     B  'R   ','E   ','F   ','Q   ','N   ','    '/
C
      DATA ISET2 /
     1  'A   ','B   ','O   ','D   ','I   ','N   ',
     2  'F   ','S   ','E   ','T   ','1   ','    ',
     3  'F   ','S   ','E   ','T   ','2   ','    ',
     4  'F   ','S   ','E   ','T   ','3   ','    ',
     5  'F   ','S   ','E   ','T   ','4   ','    ',
     6  'I   ','N   ','C   ','I   ','D   ','    ',
     7  'G   ','E   ','O   ','B   ','O   ','D   ',
     8  'B   ','D   ','W   ','O   ','R   ','K   ',
     9  'F   ','1   ','W   ','O   ','R   ','K   ',
     A  'F   ','2   ','W   ','O   ','R   ','K   ',
     B  'F   ','3   ','W   ','O   ','R   ','K   '/
      DATA ISET3 /
     1  'F   ','4   ','W   ','O   ','R   ','K   ',
     2  'G   ','E   ','O   ','F   ','S   ','1   ',
     3  'G   ','E   ','O   ','F   ','S   ','2   ',
     4  'G   ','E   ','O   ','F   ','S   ','3   ',
     5  'G   ','E   ','O   ','F   ','S   ','4   ',
     6  'S   ','B   ','O   ','D   ','Y   ','    ',
     7  'S   ','F   ','I   ','N   ','1   ','    ',
     8  'S   ','F   ','I   ','N   ','2   ','    ',
     9  'S   ','F   ','I   ','N   ','3   ','    ',
     A  'S   ','F   ','I   ','N   ','4   ','    ',
     B  'S   ','B   ','1   ','    ','    ','    '/
      DATA ISET4 /
     1  'S   ','B   ','1   ','2   ','    ','    ', 
     2  'S   ','B   ','1   ','2   ','3   ','    ', 
     3  'D   ','B   ','O   ','D   ','Y   ','    ',
     4  'D   ','F   ','I   ','N   ','1   ','    ',
     5  'D   ','F   ','I   ','N   ','2   ','    ',
     6  'D   ','B   ','1   ','    ','    ','    ',
     7  'D   ','B   ','1   ','2   ','    ','    ',
     8  'D   ','B   ','1   ','2   ','3   ','    ',
     9  'D   ','B   ','1   ','2   ','3   ','4   ',
     A  'T   ','R   ','I   ','M   ','I   ','N   ',
     B  'U   ','T   ','R   ','I   ','M   ','D   '/
       DATA ISET5 /
     1  'T   ','R   ','I   ','M   ','D   ','    ',
     2  'I   ','N   ','P   ','C   ','O   ','N   ',
     3  'E   ','B   ','O   ','D   ','I   ','N   ',
     4  'P   ','A   ','E   ','R   ','O   ','    ',
     5  'S   ','B   ','1   ','2   ','3   ','4   ',
     6  'D   ','F   ','I   ','N   ','3   ','    ',
     7  'D   ','F   ','I   ','N   ','4   ','    ',
     8  'I   ','N   ','L   ','E   ','T   ','N   ',
     9  'I   ','N   ','L   ','T   ','D   ','    '/
C      
      REWIND 7
C      
C***  READ STATEMENT FOR FORTRAN-IV
C      
C1000 READ(7,1010)(IBLOCK(I),I=1,74)
C     IF(EOF(7))1680,1020
C      
C***  READ STATEMENT FOR FORTRAN-V OR IBM
C      
 1000 READ(7,1010,END=1680)(IBLOCK(I),I=1,74)
 1010 FORMAT(74A1)
C      
C***  SHIFT OUT BLANKS
C      
 1020 DO 1030 I=1,74
         KK=I
         IF(IBLOCK(I) .NE. IBLANK)GO TO 1040
 1030 CONTINUE
C      
C***  BLANK CARD
C      
      GO TO 1000
C      
 1040 DO 1050 I=KK,74
         ICOL=I-KK+1
         IBLOCK(ICOL)=IBLOCK(I)
 1050 CONTINUE
      ICOL=ICOL+1
      DO 1060 I=ICOL,74
         IF(ICOL .GT. 74)GO TO 1060
         IBLOCK(I)=IBLANK
 1060 CONTINUE
C      
      IF(IBLOCK(1) .EQ. LEFT)GO TO 1660
C      
C***  DECODE BLOCK NAME AND RANGE
C      
      CALL DDECOD(IBLOCK,LENGTH,IFIRST,ILAST)
C      
      WRITE(4,1070)(IBLOCK(I),I=1,6),IFIRST,ILAST
 1070 FORMAT(4X,'CONTENTS OF ',6A1,' FROM',I5,' TO',I5)
C      
      IF(IFIRST.LE.0 .OR. ILAST.LT.IFIRST)GO TO 1000
C      
       DO 1090 I=1,318,6
         DO 1080 K=1,LENGTH
            IF(ISET(I+K-1) .NE. IBLOCK(K))GO TO 1090
 1080    CONTINUE
C      
C***  BLOCK NAME FOUND
C      
      GO TO 1100
C      
 1090 CONTINUE
C      
C***  BLOCK NAME NOT FOUND
C      
      GO TO 1000
C      
C***  JUMP TO APPROPIATE WRITE STATEMENT
C      
 1100 JUMP=(I+5)/6
C      
      IF(ILAST .GT. LASTV(JUMP))ILAST=LASTV(JUMP)
C      
      IF(JUMP.LT.1 .OR. JUMP.GT.53)GO TO 1000
C      
      GO TO (1110,1120,1130,1140,1150,1160,1170,1180,1190,1200,
     1       1210,1220,1230,1240,1250,1260,1270,1280,1290,1300,
     2       1310,1320,1330,1340,1350,1360,1370,1380,1390,1400,
     3       1410,1420,1430,1440,1450,1460,1470,1480,1490,1500,
     4       1510,1520,1530,1540,1550,1560,1570,1580,1590,1600,
     5       1610,1630,1650), JUMP
C      
 1110 WRITE(4,IFORM)(IDCASE(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1120 WRITE(4,IFORM)(LTHERY(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1130 WRITE(4,IFORM)(CONS(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1140 WRITE(4,IFORM)(TTRACE(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1150 WRITE(4,IFORM)(LFLAGS(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1160 WRITE(4,1690)(LDUMPF(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1170 WRITE(4,IFORM)(ADELF(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1180 WRITE(4,IFORM)(NNACA(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1190 WRITE(4,IFORM)(FLTC(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1200 WRITE(4,IFORM)(ATOTAL(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1210 WRITE(4,IFORM)(REFS(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1220 WRITE(4,IFORM)(BDIN(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1230 WRITE(4,IFORM)(FDATA1(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1240 WRITE(4,IFORM)(FDATA2(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1250 WRITE(4,IFORM)(FDATA3(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1260 WRITE(4,IFORM)(FDATA4(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1270 WRITE(4,IFORM)(DELTAS(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1280 WRITE(4,IFORM)(BDGEO(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1290 WRITE(4,IFORM)(BDW(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1300 WRITE(4,IFORM)(F1WK(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1310 WRITE(4,IFORM)(F2WK(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1320 WRITE(4,IFORM)(F3WK(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1330 WRITE(4,IFORM)(F4WK(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1340 WRITE(4,IFORM)(GFIN1(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1350 WRITE(4,IFORM)(GFIN2(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1360 WRITE(4,IFORM)(GFIN3(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1370 WRITE(4,IFORM)(GFIN4(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1380 WRITE(4,IFORM)(CBODY(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1390 WRITE(4,IFORM)(CFIN1(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1400 WRITE(4,IFORM)(CFIN2(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1410 WRITE(4,IFORM)(CFIN3(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1420 WRITE(4,IFORM)(CFIN4(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1430 WRITE(4,IFORM)(CBF1(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1440 WRITE(4,IFORM)(CBF2(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1450 WRITE(4,IFORM)(CBF1F2(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1460 WRITE(4,IFORM)(EBODY(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1470 WRITE(4,IFORM)(EFIN1(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1480 WRITE(4,IFORM)(EFIN2(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1490 WRITE(4,IFORM)(EBF1(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1500 WRITE(4,IFORM)(EBF2(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1510 WRITE(4,IFORM)(EBF1F2(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1520 WRITE(4,IFORM)(EB1234(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1530 WRITE(4,IFORM)(DTRIM(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1540 WRITE(4,IFORM)(DATAX(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1550 WRITE(4,IFORM)(TTRIM(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1560 WRITE(4,IFORM)(ACONS(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1570 WRITE(4,IFORM)(BDIN(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1580 WRITE(4,IFORM)(PAER(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1590 WRITE(4,IFORM)(CB1234(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1600 WRITE(4,IFORM)(EFIN3(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1610 WRITE(4,IFORM)(EFIN4(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1630 WRITE(4,IFORM)(INLE(KK),KK=IFIRST,ILAST)
      GO TO 1000
 1650 WRITE(4,IFORM)(DINLET(KK),KK=IFIRST,ILAST)
      GO TO 1000
C      
 1660 DO 1670 I=1,74
         IFORM(I)=IBLOCK(I)
 1670 CONTINUE
C      
      GO TO 1000
C      
 1680 CONTINUE
C      
      RETURN
 1690 FORMAT(' ',L1,1X,L1,1X,L1,1X,L1,1X,L1,1X,L1,1X,L1,1X,L1)
      END
