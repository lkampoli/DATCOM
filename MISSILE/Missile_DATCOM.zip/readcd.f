      SUBROUTINE READCD(IUNIT, KOL, IEOF)
C
C***  READ AN 80 COLUMN CARD IMAGE FROM UNIT IUNIT AND STORE THE
C***  CHARACTERS IN ARRAY KOL.  VARIABLE IEOF IS TRUE IF AN
C***  END-OF-FILE IS DETECTED.
C
      DIMENSION KOL(80)
      CHARACTER*4 KOL
C
      LOGICAL IEOF
C
      IEOF=.FALSE.
C
C***  READ CARD OR LOOK FOR A DOUBLE END-OF-FILE
C
      DO 1000 J=1,2
C
C***     FOLLOWING CARDS FOR FORTRAN-IV
C
C        READ(IUNIT,1020)(KOL(I),I=1,80)
C        IF(EOF(IUNIT))1000,1010
C
C***     FOLLOWING CARDS FOR FORTRAN-V
C
         READ(IUNIT,1020,END=1000)(KOL(I),I=1,80)
         GO TO 1010
C
 1000 CONTINUE
C
      IEOF=.TRUE.
C
 1010 CONTINUE
C
      RETURN
C
 1020 FORMAT(80A1)
C
      END
