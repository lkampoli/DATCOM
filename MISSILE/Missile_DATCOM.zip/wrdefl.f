      SUBROUTINE WRDEFL
C
C***  WRITE INCIDENCE CONTROL DEVICE INPUTS
C
      COMMON /INCID/ AA(40)
C
      COMMON /CONST/ PI,RAD,UNUSED,KAND
C      
      CHARACTER*4 KAND,NLNAME,IVNAME
C
      DIMENSION NLNAME(6),IVNAME(34),LEN(6),IDIM(6),LOC(6)
C
      DATA NLNAME /  'D   ','E   ','F   ','L   ','C   ','T   '/
      DATA IVNAME /  'D   ','E   ','L   ','T   ','A   ','1   ','D   ',
     1 'E   ','L   ','T   ','A   ','2   ','D   ','E   ','L   ','T   ',
     2 'A   ','3   ','D   ','E   ','L   ','T   ','A   ','4   ','X   ',
     3 'H   ','I   ','N   ','G   ','E   ','S   ','K   ','E   ','W   '/
      DATA IDIM   / 8,8,8,8,4,4 /
      DATA LEN    / 6,6,6,6,6,4 /
      DATA LOC    / 1,9,17,25,33,37 /
C
      CALL NAMEW(KAND,6,NLNAME,6,IVNAME,34,LEN,6,IDIM,AA,40,LOC)
C
      RETURN
      END
