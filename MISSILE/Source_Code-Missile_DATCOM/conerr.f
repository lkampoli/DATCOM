      SUBROUTINE CONERR(GO,NOEXEC)
C
C***  CONERR --- INPUT ERROR CHECKING
C
C***  THE USER DATA IS READ FROM TAPE5, CHECKED AND WRITTEN TO TAPE1
C
      COMMON /CONST/  PI,RAD,UNUSED,KAND
      COMMON /PARTF/  PARTS(19)
      LOGICAL PARTS
      DIMENSION KOL(80), NUM(10), NEXT(9), NOGO(4)
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
      INTEGER FLTCON,REFQ,AXIBOD,FINSET,DEFLCT,TRIM,ELLBOD,INLET,
     1        EXPR,ARBBOD
      LOGICAL NMTEST,END,NEXRED,GO,NOEXEC,SUBED
      CHARACTER*4 IAST,NOGO,NEXT,IBLANK,NUM,KOL,KAND,NLNAME
C
      DATA IAST /'*   '/
      DATA END / .TRUE. /
      DATA NOGO /'N   ','O   ','G   ','O   '/
      DATA NEXT   /'N   ','E   ','X   ','T   ','    ','C   ','A   ',
     1             'S   ','E   '/
      DATA NNAME / 11 /
C
      DATA IBLANK / '    ' /
      DATA NUM /'0   ','1   ','2   ','3   ','4   ','5   ','6   ',
     1          '7   ','8   ','9   '/
C
      IF(PARTS(17))CALL HEADR9
C
C***  READ ONE CARD AND CHECK TYPE -
C***           K = 1 FIRST CARD OF NAMELIST
C***           K = 2 CONTINUATION CARD IN NAMELIST
C***           K = 3 CONTROL CARD
C
      GO=.TRUE.
      END=.TRUE.
      SUBED=.FALSE.
      ISEQ=0
      REWIND 8
      IF(PARTS(17))WRITE(6,1160)
 1000 CONTINUE
      ISEQ=ISEQ+1
C
C***  FOLLOWING READ CARDS FOR FORTRAN IV
C
C     READ(5,1020) (KOL(I),I=1,80)
C     IF(EOF(5))1150,1010
 1010 CONTINUE
C
C***  FOLLOWING READ CARD FOR IBM MACHINES OR FORTRAN V
C
      READ(5,1020,END=1150) (KOL(I),I=1,80)
 1020 FORMAT(80A1)
C
C***  IGNORE BLANK CARDS
C
      DO 1030 I=1,80
         IF(KOL(I) .NE. IBLANK)GO TO 1040
 1030 CONTINUE
C
      IF(PARTS(17))WRITE(6,1220)ISEQ
C
      GO TO 1000
C
C***  IGNORE COMMENT CARDS ON INPUT, * IN COLUMN 1
C
 1040 CONTINUE
      IF(KOL(1) .EQ. IAST .AND. PARTS(17))
     1 WRITE(6,1230)ISEQ,(KOL(I),I=1,80)
      IF(KOL(1) .EQ. IAST)WRITE(8,1020)(KOL(I),I=1,80)
      IF(KOL(1) .EQ. IAST)GO TO 1000
C
C***  VALID CARD READ
C
      NEXRED=.FALSE.
      K = 2
C
C***  ADD BLANK IF KAND IN COLUMN 1
C
      IF(KOL(1) .EQ. KAND)CALL INSBLK(KOL,1,ISKP)
C
C***  SHIFT CARD COLUMNS FOR NAMELIST TO START IN COLUMN 2
C
      CALL PACK(KOL,END)
C
C***  DETERMINE IF NOGO CONTROL CARD READ
C
      DO 1050 I=1,4
         IF(KOL(I) .NE. NOGO(I))GO TO 1060
 1050 CONTINUE
      NOEXEC=.TRUE.
      IF(PARTS(17))WRITE(6,1230)ISEQ,(KOL(I),I=1,80)
      GO TO 1000
C
 1060 CONTINUE
C
      IF(KOL(2) .EQ. KAND)   K = 1
      IF(KOL(1) .NE. IBLANK) K = 3
      IF(KOL(2) .NE. KAND .AND. END) K = 3
C
C***  CHECK NAMELIST NAME IF K = 1
C
      L = 2
      IF(K .NE. 1) GO TO 1130
        IERR = 0
        DO 1110 NAME = 1,NNAME
           I = LOC(NAME)
           L = LEN(NAME)
           IF(.NOT. NMTEST(KOL(3),NLNAME(I),L)) GO TO 1110
C
C***       CHECK FOR NUMERIC INPUTS AFTER NAMELIST NAME
C
           IF(NAME .NE. 5) GO TO 1100
           L=L+1
           M=L+2
           MNUM = 0
 1070      DO 1080 J=1,10
              JVAL=J
              IF(KOL(M) .EQ. NUM(J)) GO TO 1090
 1080      CONTINUE
           IERR = 1
        GO TO 1110
 1090      M=M+1
           IF((JVAL.EQ.1.OR.JVAL.GT.5).AND.NAME.EQ.5)IERR=1
           IF((JVAL.EQ.1.OR.JVAL.GT.5).AND.NAME.EQ.5)GO TO 1110
 1100    CONTINUE
         IF(KOL(L+3) .EQ. IBLANK) GO TO 1120
C
C  INSERT BLANK AFTER NAMELIST NAME IF POSSIBLE
C
        CALL INSBLK(KOL,L+3,ISKP)
        IF(ISKP .EQ. 0)GO TO 1120
 1110   CONTINUE
        IERR = 1
 1120   L = L+3
 1130 CONTINUE
C
C***  CHECK NAMELIST VARIABLES IF K = 1 OR 2
C***  CHECK CONTROL CARD IF K = 3
C
      IF(K .LE. 2) CALL NMLIST(ISEQ,GO,END,KOL,L,NAME,K,IERR,SUBED)
      IF(K .EQ. 3) CALL CCARD(ISEQ,END,KOL)
      DO 1140 I=1,9
         IF(KOL(I).NE.NEXT(I))GO TO 1000
 1140 CONTINUE
      NEXRED=.TRUE.
      GO TO 1000
 1150 CONTINUE
      IF(.NOT.END .AND. PARTS(17))WRITE(6,1170)KAND
      IF(.NOT.END)WRITE(8,1180)KAND,(IBLANK,J=1,75)
      IF(.NOT.END)END=.TRUE.
      IF(.NOT.NEXRED .AND. PARTS(17))WRITE(6,1200)
      IF(.NOT.NEXRED)WRITE(8,1190)
      REWIND 8
C
C***  TERMINATE ON FATAL ERROR
C
      IF(.NOT. GO .AND. PARTS(17))WRITE(6,1210)
C
 1160 FORMAT(4X,/,4X,'CONERR - INPUT ERROR CHECKING',//
     1       4X,'ERROR CODES - N* DENOTES THE NUMBER OF ',
     2          'OCCURENCES OF EACH ERROR',/
     3       4X,'A - UNKNOWN VARIABLE NAME',/
     4       4X,'B - MISSING EQUAL SIGN FOLLOWING VARIABLE NAME',/
     5       4X,'C - NON-ARRAY VARIABLE HAS AN ARRAY ELEMENT',
     6          'DESIGNATION - (N)',/
     7       4X,'D - NON-ARRAY VARIABLE HAS MULTIPLE VALUES ASSIGNED',/
     8       4X,'E - ASSIGNED VALUES EXCEED ARRAY DIMENSION',/
     9       4X,'F - SYNTAX ERROR',//
     A       4X,'*************************',
     B          ' INPUT DATA CARDS ',
     C          '*************************',/)
 1170 FORMAT(4X,A1,'END',18X,'** MISSING NAMELIST TERMINATION ADDED **')
 1180 FORMAT(1X,A1,'END',75A1)
 1190 FORMAT(1X,'NEXT CASE')
 1200 FORMAT(4X,'NEXT CASE',18X,'** MISSING NEXT CASE CARD ADDED **')
 1210 FORMAT(8X,'FATAL ERROR ENCOUNTERED IN CONERR.  PROGRAM STOPPED')
 1220 FORMAT(4X,I3,' ** BLANK CARD - IGNORED')
 1230 FORMAT(I4,1X,80A1)
C
      RETURN
      END
