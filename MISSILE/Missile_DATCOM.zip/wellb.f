      SUBROUTINE WELLB
C
C***  WRITE ELLIPTICAL BODY INPUTS
C
      COMMON /ABODIN/ AA(881)
C
      COMMON /CONST/  PI,RAD,UNUSED,KAND
C      
      CHARACTER*4 KAND,NLNAME,IVNAME
C
      DIMENSION NLNAME(6),IVNAME(152),LEN(35),IDIM(35),LOC(35)
C
      DATA NLNAME /  'E   ','L   ','L   ','B   ','O   ','D   '/
      DATA IVNAME /  'N   ','X   ','X   ','O   ','X   ','0   ','X   ',
     1 'R   ','W   ','T   ','N   ','O   ','S   ','E   ','L   ','N   ',
     2 'O   ','S   ','E   ','W   ','N   ','O   ','S   ','E   ','B   ',
     3 'N   ','O   ','S   ','E   ','T   ','R   ','U   ','N   ','C   ',
     4 'L   ','C   ','E   ','N   ','T   ','R   ','W   ','C   ','E   ',
     5 'N   ','T   ','R   ','T   ','A   ','F   ','T   ','L   ','A   ',
     6 'F   ','T   ','W   ','A   ','F   ','T   ','P   ','O   ','W   ',
     7 'E   ','R   ','D   ','I   ','S   ','C   ','O   ','N   ','E   ',
     8 'L   ','L   ','I   ','P   ','H   ','E   ','N   ','O   ','S   ',
     9 'E   ','E   ','C   ','E   ','N   ','T   ','R   ','E   ','A   ',
     A 'F   ','T   ','D   ','E   ','X   ','I   ','T   ','P   ','R   ',
     B 'O   ','T   ','U   ','B   ','N   ','P   ','R   ','O   ','T   ',
     C 'P   ','T   ','Y   ','P   ','E   ','X   ','P   ','R   ','O   ',
     D 'T   ','N   ','L   ','O   ','C   ','B   ','L   ','D   ','M   ',
     E 'E   ','M   ','B   ','L   ','D   ','T   ','Y   ','P   ','L   ',
     F 'P   ','R   ','O   ','T   ','W   ','P   ','R   ','O   ','T   ',
     G 'H   ','P   ','R   ','O   ','T   ','O   ','P   ','R   ','O   ',
     H 'T   '/
      DATA IDIM   / 1,1,1,50,50,50,1,1,1,1,-1,1,1,1,1,1,1,20,50,50,1,1,
     1              1,1,-1,1,20,20,20,20,100,100,100,100,100 /
      DATA LEN    / 2,2,2,1,1,1,5,5,5,5,5,6,6,4,4,4,5,6,5,1,5,6,4,5,
     1              6,5,5,5,4,6,6,5,5,5,5 /
      DATA LOC    / 1,2,2,3,53,53,103,104,105,106,107,108,109,110,111,
     1              112,113,114,134,184,234,235,236,237,
     2              300,301,302,322,342,362,382,482,582,682,782 /
C
      CALL NAMEW(KAND,6,NLNAME,6,IVNAME,152,LEN,35,IDIM,AA,881,LOC)
C
      RETURN
      END
