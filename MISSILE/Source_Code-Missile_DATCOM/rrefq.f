      SUBROUTINE RREFQ(LREFQ, AA)
C
C***  READ REFERENCE QUANTITY INPUTS
C
      COMMON /CONST/ PI,RAD,UNUSED,KAND
C      
      CHARACTER*4 KAND,NLNAME,IVNAME
C
      DIMENSION AA(9),NLNAME(4),IVNAME(39),LEN(9),IDIM(9),LOC(9)
C
      LOGICAL LREFQ,IEOF
C
      DATA NLNAME /  'R   ','E   ','F   ','Q   '/
      DATA IVNAME /  'S   ','R   ','E   ','F   ','L   ','R   ','E   ',
     1 'F   ','L   ','A   ','T   ','R   ','E   ','F   ','R   ','O   ',
     2 'U   ','G   ','H   ','X   ','C   ','G   ','Z   ','C   ','G   ',
     3 'S   ','C   ','A   ','L   ','E   ','B   ','L   ','A   ','Y   ',
     4 'E   ','R   ','R   ','H   ','R   '/
      DATA IDIM   / 1,1,1,1,1,1,1,1,1 /
      DATA LEN    / 4,4,6,5,3,3,5,6,3 /
      DATA LOC    / 1,2,3,4,5,6,7,8,9 /
C
      LREFQ=.TRUE.
C
      CALL NAMER(KAND,2,NLNAME,4,IVNAME,39,LEN,9,IDIM,AA,9,LOC,IEOF)
C
      RETURN
      END
