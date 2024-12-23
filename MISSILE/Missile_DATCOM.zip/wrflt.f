      SUBROUTINE WRFLT
C
C***  WRITE FLIGHT CONDITION INPUTS
C
      COMMON /FLC/   AA(144)
C
      COMMON /CONST/ PI,RAD,UNUSED,KAND
C      
      CHARACTER*4 KAND,NLNAME,IVNAME
C      
      DIMENSION NLNAME(6),IVNAME(45),LEN(11),IDIM(11),LOC(11)
C
      DATA NLNAME /  'F   ','L   ','T   ','C   ','O   ','N   '/
      DATA IVNAME /  'N   ','A   ','L   ','P   ','H   ','A   ','A   ',
     1 'L   ','P   ','H   ','A   ','B   ','E   ','T   ','A   ','P   ',
     2 'H   ','I   ','N   ','M   ','A   ','C   ','H   ','M   ','A   ',
     3 'C   ','H   ','A   ','L   ','T   ','R   ','E   ','N   ','V   ',
     4 'I   ','N   ','F   ','T   ','I   ','N   ','F   ','P   ','I   ',
     5 'N   ','F   '/
      DATA IDIM   / 1,20,1,1,1,20,20,20,20,20,20 /
      DATA LEN    / 6,5,4,3,5,4,3,3,4,4,4 /
      DATA LOC    / 1,2,22,23,24,25,45,65,85,105,125 /
C
      CALL NAMEW(KAND,6,NLNAME,6,IVNAME,45,LEN,11,IDIM,AA,144,LOC)
C
      RETURN
      END
