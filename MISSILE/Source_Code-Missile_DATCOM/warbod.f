      SUBROUTINE WARBOD
C
C***  WRITE NAMELIST PROTUB USING THE NAMELIST EMULATOR
C
C***  THIS ROUTINE FILLS THE BODY COMMON BLOCK ABODIN
C***  WITH THE PROTUBERANCE VALUES READ IN THRU NAMELIST
C***  PROTUB
C
      COMMON /CONST/ PI,RAD,UNUSED,KAND
      COMMON /ABODIN/ ARBOD(881)
C
      CHARACTER*4 KAND,IARBOD,NLNAME
C
      DIMENSION LEN10(11),LDM10(11),LOC(11),IARBOD(57),NLNAME(6)
C
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
      CALL NAMEW(KAND,6,NLNAME,6,IARBOD,57,LEN10,11,LDM10,
     1           ARBOD,881,LOC)
C
      RETURN
      END
