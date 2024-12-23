      SUBROUTINE NMLIST(ISEQ,GO,END,KOL,L,NAME,K,IER,SUBED)
C
C***  SUBROUTINE TO TEST SPELLING OF NAMELIST NAMES
C
      COMMON /CONST/  PI,RAD,UNUSED,KAND
      COMMON /INPCON/ LOC(11),LEN(11),NLNAME(56)
      COMMON /VARNAM/ LEN1(11),LDM1(11),FLTCON(45),
     1                LEN2( 9),LDM2( 9),REFQ(39),
     2                LEN3(34),LDM3(34),AXIBOD(152),
     3                LEN4(23),LDM4(23),FINSET(113),
     4                LEN5( 6),LDM5( 6),DEFLCT(34),
     5                LEN6(12),LDM6(12),TRIM(59),
     6                LEN7(35),LDM7(35),ELLBOD(152),
     7                LEN8(14),LDM8(14),INLET(47),
     8                LEN9(15),LDM9(15),EXPR(53),
     9                LEN10(11),LDM10(11),ARBBOD(57)
C
      CHARACTER*4 FLTCON,REFQ,AXIBOD,FINSET,DEFLCT,TRIM,ELLBOD,INLET,
     1        EXPR,ARBBOD
C
      DIMENSION KOL(80),IUNSD(6),ILAYER(7,2),LAYLEN(2),IVALUE(2,6),
     1          IVALUF(2,4),NAMES(7,6),NLET(6),LENV(6),NAMFIN(4,4),
     2          NLTFIN(4),LENFIN(4),NINLT(6,3),NNLEN(3),NVINL(2,3),
     3          NVLEN(3),NEXP(6,9),NELEN(9),NES(2,9),NNES(9),
     4          NAMES2(7,7),NLET2(7),LENV2(7),IVALU2(2,7)
      LOGICAL END,GO,SUBED
C
      CHARACTER*4 ILAYER,IUNSD,NAMES,IVALUE,NAMES2,IVALU2,NAMFIN,
     1  KAND,KOL,IVALUF,NINLT,NVINL,NEXP,NES,JUNSD
C
C***  NAME SUBSTITUTION CONSTANTS
C
      DATA ILAYER/'T   ','U   ','R   ','B   ','    ','    ','    ',
     1            'N   ','A   ','T   ','U   ','R   ','A   ','L   '/
      DATA LAYLEN / 4,7 /
C
      DATA IUNSD /'U   ','N   ','U   ','S   ','E   ','D   '/
      DATA NAMES /'C   ','O   ','N   ','I   ','C   ','A   ','L   ',
     1            'O   ','G   ','I   ','V   ','E   ','    ','    ',
     2            'P   ','O   ','W   ','E   ','R   ','    ','    ',
     3            'H   ','A   ','A   ','C   ','K   ','    ','    ',
     4            'K   ','A   ','R   ','M   ','A   ','N   ','    ',
     5            'C   ','O   ','N   ','E   ','    ','    ','    '/
      DATA NLET / 7,5,5,5,6,4 /
      DATA IVALUE/'0   ','.   ','1   ','.   ','2   ',
     1            '.   ','3   ','.   ','4   ','.   ',
     2            '0   ','.   '/
      DATA LENV / 2,2,2,2,2,2 /
C
      DATA NAMES2/'V   ','C   ','Y   ','L   ','    ','    ','    ',
     1            'H   ','C   ','Y   ','L   ','    ','    ','    ',
     2            'L   ','U   ','G   ','    ','    ','    ','    ',
     3            'S   ','H   ','O   ','E   ','    ','    ','    ',
     4            'B   ','L   ','O   ','C   ','K   ','    ','    ',
     5            'F   ','A   ','I   ','R   ','I   ','N   ','G   ',
     6            'C   ','O   ','M   ','P   ','    ','    ','    '/
      DATA NLET2 / 4,4,3,4,5,7,4 /
      DATA IVALU2 /'1   ','.   ','2   ','.   ','3   ','.   ','4   ',
     1             '.   ','5   ','.   ','6   ','.   ','7   ','.   '/
      DATA LENV2 / 2,2,2,2,2,2,2 /
C
      DATA NAMFIN /'H   ','E   ','X   ','    ',
     1             'N   ','A   ','C   ','A   ',
     2             'A   ','R   ','C   ','    ',
     3             'U   ','S   ','E   ','R   '/
      DATA NLTFIN / 3,4,3,4 /
      DATA IVALUF/'0   ','.   ','1   ','.   ','2   ',
     1            '.   ','3   ','.   '/
      DATA LENFIN / 2,2,2,2 /
      DATA NINLT / '2   ','D   ','T   ','O   ','P   ','    ',
     1             '2   ','D   ','S   ','I   ','D   ','E   ',
     2             'A   ','X   ','I   ','    ','    ','    '/
      DATA NNLEN/ 5,6,3 /
      DATA NVINL/ '1   ','.   ',
     1            '2   ','.   ',
     2            '3   ','.   '/
      DATA NVLEN / 2,2,2 /
      DATA NEXP/'B   ','O   ','D   ','Y   ','    ','    ',
     1          'F   ','1   ','    ','    ','    ','    ',
     2          'F   ','2   ','    ','    ','    ','    ',
     3          'F   ','3   ','    ','    ','    ','    ',
     4          'F   ','4   ','    ','    ','    ','    ',
     5          'B   ','F   ','1   ','2   ','3   ','4   ',
     6          'B   ','F   ','1   ','2   ','3   ','    ',
     7          'B   ','F   ','1   ','2   ','    ','    ',
     8          'B   ','F   ','1   ','    ','    ','    '/
      DATA NELEN/ 4,2,2,2,2,6,5,4,3 /
      DATA NES/ '1   ','.   ','2   ','.   ','3   ','.   ',
     1          '4   ','.   ','5   ','.   ','9   ','.   ',
     2          '8   ','.   ','7   ','.   ','6   ','.   '/
      DATA NNES/ 9*2 /
C
C***  PASS CORRECT NAMELIST VARIABLES TO TESTOR FOR NAME
C***  AND SYNTAX VERIFICATION
C
C***  REPLACE UNUSED WITH CONSTANT 1.E-30
C
      CALL SUBNAM(ISEQ,KOL,IUNSD,6,1,6,JUNSD,6,6,SUBED)
C
      IF(IER .EQ. 1) NAME = 1
      IF(NAME .LE. 0 .OR. NAME .GT. 11)GO TO 1000
      GO TO (1000,1000,1010,1020,1030,1040,1050,1060,1070,1080,1085)
     1       ,NAME
C
C***  TEST FLTCON NAMELIST
C
 1000 CALL TESTOR(ISEQ,GO,END,KOL,L,NAME,K,
     1 IER,LEN1,LDM1,11,FLTCON,45,SUBED)
      GO TO 1090
C
C***  TEST REFQ NAMELIST
C
 1010 CONTINUE
      CALL SUBNAM(ISEQ,KOL,ILAYER,7,2,LAYLEN,IVALUE,2,LENV,SUBED)
      CALL TESTOR(ISEQ,GO,END,KOL,L,NAME,K,
     1 IER,LEN2,LDM2, 9,REFQ,39,SUBED)
      GO TO 1090
C
C***  TEST AXIBOD NAMELIST
C
 1020 CALL CHKEND(KOL,KAND,END)
      CALL SUBNAM(ISEQ,KOL,NAMES,7,6,NLET,IVALUE,2,LENV,SUBED)
      CALL SUBNM2(ISEQ,KOL,NAMES2,7,7,NLET2,IVALU2,2,LENV2,SUBED)
      CALL TESTOR(ISEQ,GO,END,KOL,L,NAME,K,
     1 IER,LEN3,LDM3,34,AXIBOD,152,SUBED)
      GO TO 1090
C
C***  TEST FINSET NAMELIST
C
 1030 CALL CHKEND(KOL,KAND,END)
      CALL SUBNAM(ISEQ,KOL,NAMFIN,4,4,NLTFIN,IVALUF,2,LENFIN,SUBED)
      CALL TESTOR(ISEQ,GO,END,KOL,L,NAME,K,
     1 IER,LEN4,LDM4,23,FINSET,113,SUBED)
      GO TO 1090
C
C***  TEST DEFLCT NAMELIST
C
 1040 CALL CHKEND(KOL,KAND,END)
      CALL TESTOR(ISEQ,GO,END,KOL,L,NAME,K,
     1 IER,LEN5,LDM5, 6,DEFLCT,34,SUBED)
      GO TO 1090
C
C***  TEST TRIM NAMELIST
C
 1050 CALL CHKEND(KOL,KAND,END)
      CALL TESTOR(ISEQ,GO,END,KOL,L,NAME,K,
     1 IER,LEN6,LDM6,12,TRIM,59,SUBED)
      GO TO 1090
C
C***  TEST ELLBOD NAMELIST
C
 1060 CALL CHKEND(KOL,KAND,END)
      CALL SUBNAM(ISEQ,KOL,NAMES,7,6,NLET,IVALUE,2,LENV,SUBED)
      CALL SUBNM2(ISEQ,KOL,NAMES2,7,7,NLET2,IVALU2,2,LENV2,SUBED)
      CALL TESTOR(ISEQ,GO,END,KOL,L,NAME,K,
     1 IER,LEN7,LDM7,35,ELLBOD,152,SUBED)
      GO TO 1090
C
C***  TEST INLET NAMELIST
C
 1070 CALL CHKEND(KOL,KAND,END)
      CALL SUBNAM(ISEQ,KOL,NINLT,6,3,NNLEN,NVINL,2,NVLEN,SUBED)
      CALL TESTOR(ISEQ,GO,END,KOL,L,NAME,K,IER,LEN8,LDM8,14,
     1            INLET,47,SUBED)
      GO TO 1090
C
C***  TEST EXPR NAMELIST
C
 1080 CALL CHKEND(KOL,KAND,END)
      CALL SUBNAM(ISEQ,KOL,NEXP,6,9,NELEN,NES,2,NNES,SUBED)
      CALL TESTOR(ISEQ,GO,END,KOL,L,NAME,K,
     1 IER,LEN9,LDM9,15,EXPR,53,SUBED)
      GO TO 1090
C
C***  TEST PROTUB NAMELIST
C
 1085 CALL CHKEND(KOL,KAND,END)
      CALL SUBNM2(ISEQ,KOL,NAMES2,7,7,NLET2,IVALU2,2,LENV2,SUBED)
      CALL TESTOR(ISEQ,GO,END,KOL,L,NAME,K,
     1 IER,LEN10,LDM10,11,ARBBOD,57,SUBED)
C
 1090 CONTINUE
      RETURN
      END
