      SUBROUTINE RAXIS(LAXIS, AA)
C
C***  READ AXISYMMETRIC BODY INPUTS
C
      COMMON /CONST/  PI,RAD,UNUSED,KAND
C      
      CHARACTER*4 KAND,NLNAME,IVNAME
C
      DIMENSION AA(881),NLNAME(6),IVNAME(152),LEN(34),IDIM(34),LOC(34)
C
      LOGICAL LAXIS,IEOF
C
      DATA NLNAME /  'A   ','X   ','I   ','B   ','O   ','D   '/
      DATA IVNAME /  'N   ','X   ','X   ','O   ','X   ','0   ','X   ',
     1 'R   ','T   ','N   ','O   ','S   ','E   ','L   ','N   ','O   ',
     2 'S   ','E   ','D   ','N   ','O   ','S   ','E   ','B   ','N   ',
     3 'O   ','S   ','E   ','T   ','R   ','U   ','N   ','C   ','L   ',
     4 'C   ','E   ','N   ','T   ','R   ','D   ','C   ','E   ','N   ',
     5 'T   ','R   ','T   ','A   ','F   ','T   ','L   ','A   ','F   ',
     6 'T   ','D   ','A   ','F   ','T   ','P   ','O   ','W   ','E   ',
     7 'R   ','D   ','I   ','S   ','C   ','O   ','N   ','D   ','E   ',
     8 'X   ','I   ','T   ','B   ','A   ','S   ','E   ','B   ','E   ',
     9 'T   ','A   ','N   ','J   ','M   ','A   ','C   ','H   ','P   ',
     A 'R   ','A   ','T   ','T   ','R   ','A   ','T   ','P   ','R   ',
     B 'O   ','T   ','U   ','B   ','N   ','P   ','R   ','O   ','T   ',
     C 'P   ','T   ','Y   ','P   ','E   ','X   ','P   ','R   ','O   ',
     D 'T   ','N   ','L   ','O   ','C   ','B   ','L   ','D   ','M   ',
     E 'E   ','M   ','B   ','L   ','D   ','T   ','Y   ','P   ','L   ',
     F 'P   ','R   ','O   ','T   ','W   ','P   ','R   ','O   ','T   ',
     G 'H   ','P   ','R   ','O   ','T   ','O   ','P   ','R   ','O   ',
     H 'T   '/
      DATA IDIM   / 1,1,1,50,50,1,1,1,1,-1,1,1,1,1,1,1,20,1,-1,1,
     1  20,20,20,-1,1,20,20,20,20,100,100,100,100,100 /
      DATA LEN    / 2,2,2,1,1,5,5,5,5,5,6,6,4,4,4,5,6,5,4,5,5,4,4,
     1  6,5,5,5,4,6,6,5,5,5,5 /
      DATA LOC    / 1,2,2,3,53,103,104,105,106,107,108,109,110,111,112,
     1              113,114,237,238,239,240,260,280,
     2             300,301,302,322,342,362,382,482,582,682,782 /
C
      LAXIS=.TRUE.
C
      CALL NAMER(KAND,2,NLNAME,6,IVNAME,152,LEN,34,IDIM,AA,881,LOC,IEOF)
C
      RETURN
      END
