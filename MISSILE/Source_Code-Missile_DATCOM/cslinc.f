      SUBROUTINE CSLINC(ICASE,CSLS,ICON,DUMMY)
C      
C     SUBROUTINE TO DETERMINE CSL INCREMENTAL TABLE AND INCREMENT CSL
C      
C***  INPUT
C      
C     ICASE = CASE NUMBER OF A RUN
C     CSLS  = ARRAY OF CSL VALUES FROM SYNTHS
C     DUMMY = ARRAY THAT CONTAINS EXPERIMENTAL COEFFICIENTS AND WILL
C             CONTAIN UPDATED COEFFICIENTS CSL = DUMMY 101 - 120
C     ICON  = CONFIGURATION TO BE ICREMENTED
C             ICON = 1     BODY ALONE
C             ICON = 2     BODY + 1 FIN SET
C             ICON = 3     BODY + 2 FIN SETS
C             ICON = 4     BODY + 3 FIN SETS
C             ICON = 5     BODY + 4 FIN SETS
C      
C***  OUTPUT
C      
C     CSL=INCREMENTED CSL VS. ALPHA ARRAY EQUIVALENT TO DUMMY 101 - 120
C         NOTE: IN THE FIRST CASE DUMMY ENTERS WITH EXPERIMENTAL VALUES
C               IT THEN RETURNS WITH INCREMENTED CSL
C      
      COMMON /CONST/ PI,RAD,UNUSED,KAND
      COMMON /FLC   / NA,ALPHA(20),FLDUM(123)
      COMMON /INC   / DUM(108),C(20),ZIP,ALP(20),NAP
      DIMENSION CSL(20),CSLTH(20),CSLEXP(20),DUMMY(120),CSLS(20,5)
      REAL NA,NAP
C
C **  INITIALIZE WORKING ARRAYS
C      
      DO 1000 I=1,20
         CSL(I)=UNUSED
         CSLTH(I)=UNUSED
         CSLEXP(I)=UNUSED
 1000 CONTINUE
C      
      NAP1 = NAP +.5
      NALPHA= NA + .5
C      
C       DETERMINE WHICH CONFIGURATION IS TO BE INCREMENTED AND FILL
C         THE WORKING ARRAYS
C      
      DO 1010 I=1,NALPHA
         IF(ICON .EQ. 1)CSLTH(I) = CSLS(I,5)
         IF(ICON .GT. 1)CSLTH(I) = CSLS(I,ICON-1)
 1010 CONTINUE
      IF (ICASE .NE. 1 .OR. ZIP .EQ. 1)GO TO 1030
C      
C       FILL CSLEXP ARRAY WITH EXPERIMENTAL DATA BEING HELD IN
C            ARRAY DUMMY
C      
      DO 1020 I=1,NALPHA
         CSLEXP(I) = DUMMY(I+100)
 1020 CONTINUE
C      
C       CALL CALIB TO GET A TABLE OF INCREMENTAL MULTIPLIERS
C      
      CALL CALIB(1,NALPHA,ALPHA,CSLEXP,NALPHA,ALPHA,CSLTH,C)
 1030 CONTINUE
C      
C***  DETERMINE INCREMENTED CSL VALUE USING INCREMENTALS IN ARRAY C
C      
      DO 1040 I=1,NALPHA
         CALL LNTRP(ALP,C,NAP1,ALPHA(I),CI)
         CSL(I) = CI + CSLTH(I)
 1040 CONTINUE
C
C     PLACE INCREMENTED CSL INTO ARRAY DUMMY
C
      DO 1050 I=1,NALPHA
         DUMMY(I+100)=CSL(I)
 1050 CONTINUE
      RETURN
      END
