      SUBROUTINE CONVRT(VALUE,NPTS,ITYPE)
C
C**  ROUTINE TO CHANGE THE UNITS OF A PARAMETER
C
C***  INPUTS
C
C  VALUE -- VALUE OR ARRAY OF VALUES TO BE CONVERTED
C   NPTS -- NUMBER OF POINTS
C  ITYPE -- TYPE OF CONVERSION TO BE PERFORMED
C   VALUE OF ITYPE            CONVERSION PERFORMED
C        -3                VOLUME FROM SPECIFIED UNITS TO FEET**3
C        -2                AREA FROM SPECIFIED UNITS TO FEET**2
C        -1                LENGTH FROM SPECIFIED UNITS TO FEET
C         1                LENGTH FROM FEET TO SPECIFIED UNITS
C         2                AREA FROM FEET**2 TO SPECIFIED UNITS
C         3                VOLUME FROM FEET**3 TO SPECIFIED UNITS
C
      COMMON /CONST/ PI,RAD,UNUSED,KAND
C
      COMMON /LOGIC/ LDMPCS,LDAMP,LBUILD,LNACA,LDERDG,
     1               LDERRD,LPART,LNAME,LPLOT,
     2               LFLT,LREFQ,LAXIS,LFIN1,LFIN2,LFIN3,LFIN4,
     3               LDEFL,LTRIM,LDIMIN,LDIMFT,LDIMCM,LDIMM,LELLB,
     4               LINLET,LEXPR,LICRMT,LSPIN,LARBOD
      LOGICAL        LDMPCS,LDAMP,LBUILD,LNACA,LDERDG,
     1               LDERRD,LPART,LNAME,LPLOT,
     2               LFLT,LREFQ,LAXIS,LFIN1,LFIN2,LFIN3,LFIN4,
     3               LDEFL,LTRIM,LDIMIN,LDIMFT,LDIMCM,LDIMM,LELLB,
     4               LINLET,LEXPR,LICRMT,LSPIN,LARBOD
C
      DIMENSION VALUE(NPTS)
C
      IF(NPTS .LE. 0)GO TO 1070
      IF(.NOT.(LDIMIN .OR. LDIMCM .OR. LDIMM))GO TO 1070
C
      JUMP=ITYPE+4
      IF(JUMP .LT. 1 .OR. JUMP .GT. 7)GO TO 1070
C
      DO 1060 I=1,NPTS
C
      IF(VALUE(I) .EQ. UNUSED)GO TO 1060
C
      GO TO (1000,1010,1020,1060,1030,1040,1050), JUMP
C
C  CONVERT VOLUME TO FEET**3
C
 1000 CONTINUE
      IF(LDIMM)VALUE(I)=VALUE(I)*(3.28)**3
      IF(LDIMIN)VALUE(I)=VALUE(I)/(12.)**3
      IF(LDIMCM)VALUE(I)=VALUE(I)/(2.54*12.)**3
      GO TO 1060
C
C  CONVERT AREA TO FEET**2
C
 1010 CONTINUE
      IF(LDIMM)VALUE(I)=VALUE(I)*(3.28)**2
      IF(LDIMIN)VALUE(I)=VALUE(I)/(12.)**2
      IF(LDIMCM)VALUE(I)=VALUE(I)/(2.54*12.)**2
      GO TO 1060
C
C  CONVERT LENGTH TO FEET
C
 1020 CONTINUE
      IF(LDIMM)VALUE(I)=VALUE(I)*3.28
      IF(LDIMIN)VALUE(I)=VALUE(I)/12.
      IF(LDIMCM)VALUE(I)=VALUE(I)/(2.54*12.)
      GO TO 1060
C
 1030 CONTINUE
C
C  CONVERT LENGTH FROM FEET
C
      IF(LDIMM)VALUE(I)=VALUE(I)/3.28
      IF(LDIMIN)VALUE(I)=VALUE(I)*12.
      IF(LDIMCM)VALUE(I)=VALUE(I)*(2.54*12.)
      GO TO 1060
C
 1040 CONTINUE
C
C  CONVERT AREA FROM FEET**2
C
      IF(LDIMM)VALUE(I)=VALUE(I)/(3.28)**2
      IF(LDIMIN)VALUE(I)=VALUE(I)*(12.)**2
      IF(LDIMCM)VALUE(I)=VALUE(I)*(2.54*12.)**2
      GO TO 1060
C
 1050 CONTINUE
C
C  CONVERT VOLUME FROM FEET**3
C
      IF(LDIMM)VALUE(I)=VALUE(I)/(3.28)**3
      IF(LDIMIN)VALUE(I)=VALUE(I)*(12.)**3
      IF(LDIMCM)VALUE(I)=VALUE(I)*(2.54*12.)**3
C
 1060 CONTINUE
C
 1070 CONTINUE
C
      RETURN
      END
