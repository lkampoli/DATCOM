      SUBROUTINE RARBOD(LARBOD,AA)
C
C***  READ NAMELIST PROTUB USING THE NAMELIST EMULATOR
C
C***  THIS ROUTINE READS THE PROTUBERANCE NAMELIST AND SETS
C***  LARBOD EQUAL TO TRUE.  IN ROUTINE AERO, LOGICAL VARIABLE
C***  PROTUB IS SET TO TRUE IF LARBOD IS TRUE, WHICH TURNS ON THE
C***  PROTUBERANCE CALCULATIONS.
C
      COMMON /CONST/ PI,RAD,UNUSED,KAND
C
      DIMENSION AA(881),LEN10(11),LDM10(11),LOC(11)
      DIMENSION IARBOD(57),NLNAME(6)
C
      LOGICAL LARBOD,IEOF
      CHARACTER*4 NLNAME,IARBOD,KAND
C
      DATA NLNAME/'P   ','R   ','O   ','T   ','U   ','B   '/
      DATA IARBOD /'P   ','R   ',
     B 'O   ','T   ','U   ','B   ','N   ','P   ','R   ','O   ','T   ',
     C 'P   ','T   ','Y   ','P   ','E   ','X   ','P   ','R   ','O   ',
     D 'T   ','N   ','L   ','O   ','C   ','B   ','L   ','D   ','M   ',
     E 'E   ','M   ','B   ','L   ','D   ','T   ','Y   ','P   ','L   ',
     F 'P   ','R   ','O   ','T   ','W   ','P   ','R   ','O   ','T   ',
     G 'H   ','P   ','R   ','O   ','T   ','O   ','P   ','R   ','O   ',
     H 'T   '/
      DATA LEN10 /6,5,5,5,4,6,6,5,5,5,5/
      DATA LDM10 /-1,1,20,20,20,20,100,100,100,100,100/
      DATA LOC   /300,301,302,322,342,362,382,482,582,682,782 /

C
      LARBOD=.TRUE.
C
      CALL NAMER(KAND,2,NLNAME,6,IARBOD,57,LEN10,11,LDM10,AA,881,
     1           LOC,IEOF)
C
      RETURN
      END
