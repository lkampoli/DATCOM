      SUBROUTINE RTRIM(LTRIM, AA)
C
C***  READ TRIM INCIDENCE CONTROL INPUTS
C
      COMMON /CONST/  PI,RAD,UNUSED,KAND
C      
      CHARACTER*4 KAND,NLNAME,IVNAME
C
      DIMENSION AA(15),NLNAME(4),IVNAME(59),LEN(12),IDIM(12),LOC(12)
C
      LOGICAL LTRIM,IEOF
C
      DATA NLNAME /  'T   ','R   ','I   ','M   '/
      DATA IVNAME /  'S   ','E   ','T   ','P   ','A   ','N   ','L   ',
     1 '1   ','P   ','A   ','N   ','L   ','2   ','P   ','A   ','N   ',
     2 'L   ','3   ','P   ','A   ','N   ','L   ','4   ','P   ','A   ',
     3 'N   ','L   ','5   ','P   ','A   ','N   ','L   ','6   ','P   ',
     4 'A   ','N   ','L   ','7   ','P   ','A   ','N   ','L   ','8   ',
     5 'D   ','E   ','L   ','M   ','I   ','N   ','D   ','E   ','L   ',
     6 'M   ','A   ','X   ','A   ','S   ','Y   ','M   '/
      DATA IDIM   / 1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,-4 /
      DATA LEN    / 3,5,5,5,5,5,5,5,5,6,6,4 /
      DATA LOC    / 1,2,3,4,5,6,7,8,9,10,11,12 /
C
      LTRIM=.TRUE.
C
      CALL NAMER(KAND,2,NLNAME,4,IVNAME,59,LEN,12,IDIM,AA,15,LOC,IEOF)
C
      RETURN
      END
