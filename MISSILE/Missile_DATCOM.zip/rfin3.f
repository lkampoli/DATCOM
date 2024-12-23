      SUBROUTINE RFIN3(LFIN3, AA)
C
C***  READ FINSET3 INPUTS
C
C  WRITTEN BY S. STOY, MCDONNELL DOUGLAS
C
      COMMON /CONST/ PI,RAD,UNUSED,KAND
C      
      CHARACTER*4 KAND,NLNAME,IVNAME
C
      DIMENSION AA(399),NLNAME(6),IVNAME(113),LEN(23),IDIM(23),LOC(23)
C
      LOGICAL LFIN3,IEOF
C
      DATA NLNAME /  'F   ','I   ','N   ','S   ','E   ','T   '/
      DATA IVNAME /  'S   ','E   ','C   ','T   ','Y   ','P   ','S   ',
     1 'S   ','P   ','A   ','N   ','L   ','M   ','A   ','X   ','U   ',
     2 'L   ','F   ','L   ','A   ','T   ','U   ','L   ','M   ','A   ',
     3 'X   ','L   ','L   ','F   ','L   ','A   ','T   ','L   ','C   ',
     4 'H   ','O   ','R   ','D   ','Z   ','U   ','P   ','P   ','E   ',
     5 'R   ','Z   ','L   ','O   ','W   ','E   ','R   ','S   ','W   ',
     6 'E   ','E   ','P   ','S   ','T   ','A   ','X   ','L   ','E   ',
     7 'N   ','P   ','A   ','N   ','E   ','L   ','X   ','C   ','O   ',
     8 'R   ','D   ','M   ','E   ','A   ','N   ','T   ','H   ','I   ',
     9 'C   ','K   ','Y   ','U   ','P   ','P   ','E   ','R   ','Y   ',
     A 'L   ','O   ','W   ','E   ','R   ','F   ','I   ','N   ','P   ',
     B 'H   ','I   ','L   ','E   ','R   ','G   ','A   ','M   ','P   ',
     C 'H   ','I   ','F   ','C   ','F   ','O   ','C   '/
      DATA IDIM   / 1,10,10,10,10,10,10,10,10,10,10,10,1,50,50,50,50,
     1              50,1,10,8,8,10 /
      DATA LEN    / 6,5,5,6,5,6,5,6,6,5,3,3,6,5,4,5,6,6,6,3,3,4,4 /
      DATA LOC    / 1,2,12,22,32,42,52,62,72,82,92,102,112,113,163,213,
     1              263,313,363,364,374,382,390 /
C
      LFIN3=.TRUE.
C
      CALL NAMER(KAND,2,NLNAME,6,IVNAME,113,LEN,23,IDIM,AA,399,LOC,
     1 IEOF)
C
      RETURN
      END
