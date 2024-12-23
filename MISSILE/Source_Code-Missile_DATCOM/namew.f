      SUBROUTINE NAMEW(KAND,IUNIT,NLNAME,LENNLN,VNAME,DVNAME,LENVN,
     1                 NUMVN,VDIME,COMBLK,MAXCOM,LOC)
C
C***  SIMULATE NAMELIST OUTPUT.
C
C   KAND - NAMELIST DELIMITER ($ ON CDC AND VAX, & ON IBM AND OTHER)
C          (BUT CAN BE SPECIFIED TO BE ANY UNIQUE CHARACTER)
C  IUNIT - TAPE UNIT FOR WRITING NAMELIST DATA
C NLNAME - NAMELIST NAME TO WRITE
C LENNLN - NUMBER OF CHARACTERS IN NAMELIST NAME (DIMENSION OF NLNAME)
C  VNAME - VARIABLE NAMES FOR THIS NAMELIST (AN ARRAY)
C DVNAME - DIMENSION OF VNAME ARRAY
C  LENVN - ARRAY WHICH DEFINES THE NUMBER OF CHARACTERS IN EACH
C          VARIABLE NAME
C  NUMVN - NUMBER OF VARIABLE NAMES DEFINED
C  VDIME - NUMBER OF CONSTANTS PERMITTED FOR EACH VARIABLE
C          (DIMENSION OF EACH VARIABLE; USE NEGATIVE FOR LOGICAL
C           VARIABLES)
C COMBLK - STARTING LOCATION FOR RESULTS
C          (USUALLY THE FIRST LOCATION OF A COMMON BLOCK)
C MAXCOM - MAXIMUM DIMENSION OF STORAGE ARRAY
C          (USUALLY THE LENGTH OF THE COMMON BLOCK)
C    LOC - POINTER TO STORAGE ARRAY FOR THE STARTING LOCATION
C          OF EACH INPUT VARIABLE (IF ZERO THE CONSTANT IS NOT PRINTED)
C
C***  NOTES -
C
C***  ALL INPUT STRING ARRAYS MUST BE DECLARED INTEGER
C
C***  ALL VARIABLES ARE ASSUMMED TO BE REAL; IF VDIME IS NEGATIVE
C***  THE VARIABLE IS ASSUMMED TO BE LOGICAL.  ALTHOUGH THE CODE IS
C***  DESIGNED TO HANDLE INTEGERS AS WELL, THIS OPTION IS NOT
C***  CURRENTLY USED.  IT CAN BE ACTIVATED BY PASSING ANOTHER ARRAY,
C***  VTYPE, TO DEFINE THE TYPE OF VARIABLE, WITH THE CODE
C***  0=LOGICAL, 1=INTEGER, OR 2=REAL; THEN REPLACE THE NAME VTYPE
C***  WITH THE NAME VTYPE(NVN) IN THE REMAINING CODE.
C
C***  IF ANY ELEMENT OF ARRAY LOC IS ZERO, THAT VARIABLE WILL NOT
C***  BE INCLUDED IN THE LISTING.
C
      INTEGER DVNAME,VTYPE,VDIME
      CHARACTER*4 BLANK,EQUAL,KOL,VNAME,KAND,NLNAME
C
      DIMENSION NLNAME(LENNLN),VNAME(DVNAME),LENVN(NUMVN),
     1          VDIME(NUMVN),COMBLK(MAXCOM),LOC(NUMVN)
      DIMENSION KOL(8)
C
      DATA BLANK / '    ' /
      DATA EQUAL / '=   ' /
C
      WRITE(IUNIT,1030)KAND,(NLNAME(I),I=1,LENNLN)
C
      INDX=1
C
      DO 1020 NVN=1,NUMVN
C
         IF(NVN .GT. 1)INDX=INDX+MAXC
C
         MAXC=LENVN(NVN)
C
         DO 1000 J=1,8
            KOL(J)=BLANK
 1000    CONTINUE
C
         DO 1010 I=1,MAXC
            KOL(I)=VNAME(I+INDX-1)
 1010    CONTINUE
C
         KOL(8)=EQUAL
C
         KK=LOC(NVN)
         IF(KK .LT. 1)GO TO 1020
         JJ=IABS(VDIME(NVN))
C
C ... A NEGATIVE DIMENSION MEANS LOGICAL
C ... ELSE VARIABLE TYPE IS REAL
C
         IF(VDIME(NVN) .LT. 0)VTYPE=0
         IF(VDIME(NVN) .GT. 0)VTYPE=2
C
         IF(VTYPE .EQ. 0)CALL FORLOG(IUNIT, KOL, COMBLK(KK), JJ)
         IF(VTYPE .EQ. 1)CALL FORINT(IUNIT, KOL, COMBLK(KK), JJ)
         IF(VTYPE .EQ. 2)CALL FORREA(IUNIT, KOL, COMBLK(KK), JJ)
C
 1020 CONTINUE
C
      WRITE(IUNIT,1040)KAND
C
      RETURN
C
 1030 FORMAT('1',8A1)
 1040 FORMAT('0',A1,'END')
C
      END
