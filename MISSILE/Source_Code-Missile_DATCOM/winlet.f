      SUBROUTINE WINLET
C
C***  WRITE INLET INPUTS
C
      COMMON /INLETN/ AA(64)
C
      COMMON /CONST/  PI,RAD,UNUSED,KAND
C      
      CHARACTER*4 KAND,NLNAME,IVNAME
C
      DIMENSION NLNAME(5),IVNAME(47),LEN(14),IDIM(14),LOC(14)
C
      DATA NLNAME /  'I   ','N   ','L   ','E   ','T   '/
      DATA IVNAME /  'N   ','I   ','N   ',
     1 'I   ','N   ','T   ','Y   ','P   ','E   ',
     2 'X   ','I   ','N   ','L   ','T   ',
     3 'X   ','D   ','I   ','V   ',
     4 'H   ','D   ','I   ','V   ',
     5 'L   ','D   ','I   ','V   ',
     6 'P   ','H   ','I   ',
     7 'X   ',
     8 'H   ',
     9 'W   ',
     A 'C   ','O   ','V   ','E   ','R   ',
     B 'R   ','A   ','M   ','P   ',
     C 'A   ','D   ','D   ',
     D 'M   ','F   ','R   '/
      DATA IDIM   / 1,1,1,1,1,1,20,5,5,5,-1,1,-1,20 /
      DATA LEN    / 3,6,5,4,4,4,3,1,1,1,5,4,3,3 /
      DATA LOC    / 1,2,3,4,5,6,7,27,32,37,42,43,44,45 /
C
      CALL NAMEW(KAND,6,NLNAME,5,IVNAME,47,LEN,14,IDIM,AA,64,LOC)
C
      RETURN
      END
