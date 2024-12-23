      SUBROUTINE FORLOG(IUNIT, NAME, COMBLK, II)
C
C***  FORCE A LOGICAL PRINT ONTO UNIT IUNIT
C
C  IUNIT - OUTPUT UNIT NUMBER
C   NAME - ARRAY WHICH DEFINES THE NAMELIST VARIABLE NAME
C COMBLK - DATA FOR NAMELIST NAME
C     II - NUMBER OF ELEMENTS TO PRINT
C
      CHARACTER*4 NAME
      LOGICAL COMBLK
C
      DIMENSION NAME(8),COMBLK(II)
C
      WRITE(IUNIT,1000)(NAME(I),I=1,8),(COMBLK(I),I=1,II)
C
      RETURN
C
 1000 FORMAT('0',8A1,8(13X,L1,',')/(9X,8(13X,L1,',')))
C
      END
