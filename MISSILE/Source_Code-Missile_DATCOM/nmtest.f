      LOGICAL FUNCTION NMTEST(KOL,KEY,NCHAR)
      DIMENSION KOL(80),KEY(NCHAR)
      CHARACTER*4 KOL,KEY
C
C***  TEST FOR NAME IN KEY = NAME IN KOL, NCHAR = NO. OF CHARACTERS
C
      DO 1000 I=1,NCHAR
         IF(KOL(I).NE.KEY(I)) GO TO 1010
 1000 CONTINUE
C
C***  HERE IF NAME CHECKS
C
      NMTEST=.TRUE.
      GO TO 1020
C
C***  HERE IF NAME DOES NOT CHECK
C
 1010 NMTEST=.FALSE.
 1020 CONTINUE
      RETURN
      END
