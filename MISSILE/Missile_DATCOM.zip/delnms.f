      LOGICAL FUNCTION DELNMS(KOL,IFLAG,NA)
C
C***  FUNCTION TO CHECK FOR VALID NAMELIST NAMES TO BE DELETED
C***  FOR THE CASE USING CONTROL CARD DELETE.  FUNCTION RETURNS
C***  A VALUE OF TRUE IF DELETE CONTROL CARD IS READ.
C
C***  INPUTS
C
C    KOL -- CARD IMAGE (80 COLUMN)
C  IFLAG -- TYPE OF ACTION TO TAKE
C           .EQ. 0 MEANS TO CHECK NAMES ONLY
C           .NE. 0 MEANS TO SET FLAG
C     NA -- COUNT OF NUMBER OF ERRORS ENCOUNTERED
C
      COMMON /DFLAGS/ DFLT,DREF,DAXI,DFIN1,DFIN2,DFIN3,DFIN4,DDEFL,
     1 DTRIM,DELLB,DINLET,DARBOD
C
      DIMENSION KOL(80),IDEL(6),NLNMS(71),NLLENS(12)
C
      LOGICAL DFLT,DREF,DAXI,DFIN1,DFIN2,DFIN3,DFIN4,DDEFL,DTRIM,DELLB,
     1 DINLET,DARBOD
      LOGICAL NMTEST,DELFLG(12)
      CHARACTER*4 IDEL,NLNMS,IBLANK,ICOMMA,KOL
C
      EQUIVALENCE (DELFLG(1),DFLT)
C
      DATA IDEL / 'D   ','E   ','L   ','E   ','T   ','E   '/
      DATA NLNMS /
     1 'F   ','L   ','T   ','C   ','O   ','N   ',
     2 'R   ','E   ','F   ','Q   ',
     3 'A   ','X   ','I   ','B   ','O   ','D   ',
     4 'F   ','I   ','N   ','S   ','E   ','T   ','1   ',
     5 'F   ','I   ','N   ','S   ','E   ','T   ','2   ',
     6 'F   ','I   ','N   ','S   ','E   ','T   ','3   ',
     7 'F   ','I   ','N   ','S   ','E   ','T   ','4   ',
     8 'D   ','E   ','F   ','L   ','C   ','T   ',
     9 'T   ','R   ','I   ','M   ',
     A 'E   ','L   ','L   ','B   ','O   ','D   ',
     B 'I   ','N   ','L   ','E   ','T   ',
     C 'P   ','R   ','O   ','T   ','U   ','B   '/
      DATA NLLENS / 6,4,6,7,7,7,7,6,4,6,5,6/
C
      DATA IBLANK / '    '/
      DATA ICOMMA / ',   '/
C
      DELNMS=.FALSE.
C
C***  CHECK FOR DELETE CONTROL CARD
C
      DO 1000 I=1,6
         IF(KOL(I) .NE. IDEL(I))GO TO 1080
 1000 CONTINUE
C
      II=6
C
C***  CARD COLUMN LOOP (IMPLIED DO LOOP)
C
 1010 II=II+1
         IF(II .GT. 80)GO TO 1070
         IF(KOL(II) .EQ. IBLANK .OR. KOL(II) .EQ. ICOMMA)GO TO 1010
         INDEX=1
C
C***     NAME LOOP
C
 1020    CONTINUE
         DO 1030 INAME=1,12
C
C***        CHECK NAME
C
            IF(NMTEST(KOL(II),NLNMS(INDEX),NLLENS(INAME)))GO TO 1040
            INDEX=INDEX+NLLENS(INAME)
C
 1030    CONTINUE
C
C***     NAME NOT FOUND
C
      IF(IFLAG .EQ. 0)NA=NA+1
      GO TO 1050
C
 1040 CONTINUE
C
         IF(IFLAG .NE. 0)DELFLG(INAME)=.TRUE.
C
         II=II+NLLENS(INAME)-1
C
      GO TO 1050
C
C***     FIND NEXT CARD COLUMN WITH A BLANK OR A COMMA
C
 1050    II=II+1
            IF(II .GT. 80)GO TO 1070
            IF(KOL(II) .EQ. IBLANK .OR. KOL(II) .EQ. ICOMMA)GO TO 1060
         GO TO 1050
C
 1060    II=II-1
C
      GO TO 1010
C
 1070 CONTINUE
C
      DELNMS=.TRUE.
C
 1080 CONTINUE
C
      RETURN
      END
