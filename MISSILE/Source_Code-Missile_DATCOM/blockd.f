      BLOCK DATA BLOCKD
C
C***  THIS ROUTINE SETS ALL OF THE CONSTANTS USED IN THE PROGRAM
C
      COMMON /CONST/  PI,RAD,UNUSED,KAND
      COMMON /INPCON/ LOC(11),LEN(11),NLNAME(56)
      COMMON /TRACE/  LEVEL,IROUTN(10,2)
C
C***  TO CONSERVE CORE, DATA REQUIRED FOR THE INPUT ERROR CHECKING
C***  MODULE IS OVERLAYED INTO GENERAL DATA STORAGE.  IT IS CLEARED
C***  PRIOR TO EXECUTION OF THE INPUT CASES.
C
      COMMON /VARNAM/ LEN1(11),LDM1(11),FLTCON(45),
     1                LEN2( 9),LDM2( 9),REFQ(39),
     2                LEN3(34),LDM3(34),AXIBOD(152),
     3                LEN4(23),LDM4(23),FINSET(113),
     4                LEN5( 6),LDM5( 6),DEFLCT(34),
     5                LEN6(12),LDM6(12),TRIM(59),
     6                LEN7(35),LDM7(35),ELLBOD(152),
     7                LEN8(14),LDM8(14),INLET(47),
     8                LEN9(15),LDM9(15),EXPR(53),
     9                LEN10(11),LDM10(11),ARBBOD(57)
C
      CHARACTER*4 FLTCON,REFQ,AXIBOD,FINSET,DEFLCT,TRIM,ELLBOD,INLET,
     1        EXPR,ARBBOD,KAND,NLNAME
C
C***  PROGRAM NUMERIC CONSTANTS
C
      DATA PI  / 3.14159265359 /
      DATA RAD / 57.29577951308 /
C
C***  ALL DATA LOCATIONS ARE INITIALIZED TO UNUSED, NOT ZERO

C***  BOTH THE REAL AND STRING CONSTANTS BELOW MUST BE CONSISTENT
C
      DATA UNUSED / 1.E-30 /
C
C***  VARIABLE KAND IS THE NAMELIST INPUT PARAMETER
C***  IT IS SET TO $ FOR CDC AND VAX COMPUTER SYSTEMS
C***  IT MAY NEED TO BE CHANGED FOR OTHER SYSTEMS
C
      DATA KAND / '$   ' /
C
C***  SET THE NAMELIST NAMES FOR CONERR
C
C***  LOCATION OF THE FIRST LETTER OF EACH NAME
C
      DATA LOC / 1,4,10,14,20,26,32,36,42,47,51 /
C
C***  NUMBER OF CHARACTERS IN EACH NAME
C
      DATA LEN / 3,6,4,6,6,6,4,6,5,4,6 /
C
C***  SET NAMELIST NAMES
C
      DATA NLNAME / 'E   ','N   ','D   ','F   ','L   ','T   ','C   ',
     1 'O   ','N   ','R   ','E   ','F   ','Q   ','A   ','X   ','I   ',
     2 'B   ','O   ','D   ','F   ','I   ','N   ','S   ','E   ','T   ',
     3 'D   ','E   ','F   ','L   ','C   ','T   ','T   ','R   ','I   ',
     4 'M   ','E   ','L   ','L   ','B   ','O   ','D   ','I   ','N   ',
     5 'L   ','E   ','T   ','E   ','X   ','P   ','R   ','P   ','R   ',
     6 'O   ','T   ','U   ','B   '/
C
C***  SET THE VARIABLE NAMES TO BE CHECKED IN CONERR
C
C***  NUMBER OF CHARACTERS IN EACH NAME
C
      DATA LEN1 / 6,5,4,3,5,4,3,3,4,4,4 /
C
C***  DIMENSION OF EACH VARIABLE (NEGATIVE MEANS LOGICAL)
C
      DATA LDM1 / 1,20,1,1,1,20,20,20,20,20,20 /
C
C***  SET THE CHARACTERS IN EACH NAME
C
      DATA FLTCON / 'N   ','A   ','L   ','P   ','H   ','A   ','A   ',
     1 'L   ','P   ','H   ','A   ','B   ','E   ','T   ','A   ','P   ',
     2 'H   ','I   ','N   ','M   ','A   ','C   ','H   ','M   ','A   ',
     3 'C   ','H   ','A   ','L   ','T   ','R   ','E   ','N   ','V   ',
     4 'I   ','N   ','F   ','T   ','I   ','N   ','F   ','P   ','I   ',
     5 'N   ','F   '/
      DATA LEN2 / 4,4,6,5,3,3,5,6,3 /
      DATA LDM2 / 1,1,1,1,1,1,1,1,1 /
      DATA REFQ / 'S   ','R   ','E   ','F   ','L   ','R   ','E   ',
     1 'F   ','L   ','A   ','T   ','R   ','E   ','F   ','R   ','O   ',
     2 'U   ','G   ','H   ','X   ','C   ','G   ','Z   ','C   ','G   ',
     3 'S   ','C   ','A   ','L   ','E   ','B   ','L   ','A   ','Y   ',
     4 'E   ','R   ','R   ','H   ','R   '/
      DATA LEN3 / 2,2,2,1,1,5,5,5,5,5,6,6,4,4,4,5,6,5,4,5,5,4,4,
     1 6,5,5,5,4,6,6,5,5,5,5 /
      DATA LDM3 / 1,1,1,50,50,1,1,1,1,-1,1,1,1,1,1,1,20,1,-1,1,20,
     1 20,20,-1,1,20,20,20,20,100,100,100,100,100 /
      DATA AXIBOD / 'N   ','X   ','X   ','O   ','X   ','0   ','X   ',
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
      DATA LEN4 / 6,5,5,6,5,6,5,6,6,5,3,3,3,6,5,4,5,6,6,6,3,4,4 /
      DATA LDM4 / 1,10,10,10,10,10,10,10,10,10,10,10,10,1,
     1 50,50,50,50,50,1,8,8,10 /
      DATA FINSET / 'S   ','E   ','C   ','T   ','Y   ','P   ','S   ',
     1 'S   ','P   ','A   ','N   ','L   ','M   ','A   ','X   ','U   ',
     2 'L   ','F   ','L   ','A   ','T   ','U   ','L   ','M   ','A   ',
     3 'X   ','L   ','L   ','F   ','L   ','A   ','T   ','L   ','C   ',
     4 'H   ','O   ','R   ','D   ','Z   ','U   ','P   ','P   ','E   ',
     5 'R   ','Z   ','L   ','O   ','W   ','E   ','R   ','S   ','W   ',
     6 'E   ','E   ','P   ','S   ','T   ','A   ','L   ','E   ','R   ',
     7 'X   ','L   ','E   ','N   ','P   ','A   ','N   ','E   ','L   ',
     8 'X   ','C   ','O   ','R   ','D   ','M   ','E   ','A   ','N   ',
     9 'T   ','H   ','I   ','C   ','K   ','Y   ','U   ','P   ','P   ',
     A 'E   ','R   ','Y   ','L   ','O   ','W   ','E   ','R   ','F   ',
     B 'I   ','N   ','P   ','H   ','I   ','G   ','A   ','M   ','P   ',
     C 'H   ','I   ','F   ','C   ','F   ','O   ','C   '/
      DATA LEN5 / 6,6,6,6,6,4 /
      DATA LDM5 / 8,8,8,8,4,4 /
      DATA DEFLCT / 'D   ','E   ','L   ','T   ','A   ','1   ','D   ',
     1 'E   ','L   ','T   ','A   ','2   ','D   ','E   ','L   ','T   ',
     2 'A   ','3   ','D   ','E   ','L   ','T   ','A   ','4   ','X   ',
     3 'H   ','I   ','N   ','G   ','E   ','S   ','K   ','E   ','W   '/
      DATA LEN6 / 3,5,5,5,5,5,5,5,5,6,6,4 /
      DATA LDM6 / 1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,-8 /
      DATA TRIM / 'S   ','E   ','T   ','P   ','A   ','N   ','L   ',
     1 '1   ','P   ','A   ','N   ','L   ','2   ','P   ','A   ','N   ',
     2 'L   ','3   ','P   ','A   ','N   ','L   ','4   ','P   ','A   ',
     3 'N   ','L   ','5   ','P   ','A   ','N   ','L   ','6   ','P   ',
     4 'A   ','N   ','L   ','7   ','P   ','A   ','N   ','L   ','8   ',
     5 'D   ','E   ','L   ','M   ','I   ','N   ','D   ','E   ','L   ',
     6 'M   ','A   ','X   ','A   ','S   ','Y   ','M   '/
      DATA LEN7 / 2,2,2,1,1,5,5,5,5,5,6,6,4,4,4,5,6,5,1,1,5,6,4,5,
     1 6,5,5,5,4,6,6,5,5,5,5 /
      DATA LDM7 / 1,1,1,50,50,1,1,1,1,-1,1,1,1,1,1,1,20,50,50,50,1,1,1,
     1 1,-1,1,20,20,20,20,100,100,100,100,100 /
      DATA ELLBOD / 'N   ','X   ','X   ','O   ','X   ','0   ','X   ',
     1 'R   ','T   ','N   ','O   ','S   ','E   ','L   ','N   ','O   ',
     2 'S   ','E   ','W   ','N   ','O   ','S   ','E   ','B   ','N   ',
     3 'O   ','S   ','E   ','T   ','R   ','U   ','N   ','C   ','L   ',
     4 'C   ','E   ','N   ','T   ','R   ','W   ','C   ','E   ','N   ',
     5 'T   ','R   ','T   ','A   ','F   ','T   ','L   ','A   ','F   ',
     6 'T   ','W   ','A   ','F   ','T   ','P   ','O   ','W   ','E   ',
     7 'R   ','D   ','I   ','S   ','C   ','O   ','N   ','E   ','L   ',
     8 'L   ','I   ','P   ','W   ','H   ','E   ','N   ','O   ','S   ',
     9 'E   ','E   ','C   ','E   ','N   ','T   ','R   ','E   ','A   ',
     A 'F   ','T   ','D   ','E   ','X   ','I   ','T   ','P   ','R   ',
     B 'O   ','T   ','U   ','B   ','N   ','P   ','R   ','O   ','T   ',
     C 'P   ','T   ','Y   ','P   ','E   ','X   ','P   ','R   ','O   ',
     D 'T   ','N   ','L   ','O   ','C   ','B   ','L   ','D   ','M   ',
     E 'E   ','M   ','B   ','L   ','D   ','T   ','Y   ','P   ','L   ',
     F 'P   ','R   ','O   ','T   ','W   ','P   ','R   ','O   ','T   ',
     G 'H   ','P   ','R   ','O   ','T   ','O   ','P   ','R   ','O   ',
     H 'T   '/
      DATA LEN8 / 3,6,5,4,4,4,3,1,1,1,5,4,3,3 /
      DATA LDM8 / 1,1,1,1,1,1,20,5,5,5,-1,1,-1,20 /
      DATA INLET   / 'N   ','I   ','N   ',
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
      DATA LEN9 / 4,6,5,4,4,6,3,3,4,2,2,2,2,3,3 /
      DATA LDM9 / 1,1,20,1,1,1,1,1,1,20,20,20,20,20,20 /
      DATA EXPR / 'M   ','A   ','C   ','H   ','N   ','A   ',
     1 'L   ','P   ','H   ','A   ','A   ','L   ','P   ','H   ','A   ',
     2 'S   ','R   ','E   ','F   ','L   ','R   ','E   ','F   ','L   ',
     3 'A   ','T   ','R   ','E   ','F   ','X   ','C   ','G   ','Z   ',
     4 'C   ','G   ','C   ','O   ','N   ','F   ','C   ','N   ','C   ',
     5 'M   ','C   ','A   ','C   ','Y   ','C   ','S   ','N   ','C   ',
     6 'S   ','L   '/
C
      DATA ARBBOD /'P   ','R   ',
     B 'O   ','T   ','U   ','B   ','N   ','P   ','R   ','O   ','T   ',
     C 'P   ','T   ','Y   ','P   ','E   ','X   ','P   ','R   ','O   ',
     D 'T   ','N   ','L   ','O   ','C   ','B   ','L   ','D   ','M   ',
     E 'E   ','M   ','B   ','L   ','D   ','T   ','Y   ','P   ','L   ',
     F 'P   ','R   ','O   ','T   ','W   ','P   ','R   ','O   ','T   ',
     G 'H   ','P   ','R   ','O   ','T   ','O   ','P   ','R   ','O   ',
     H 'T   '/
      DATA LEN10 /6,5,5,5,4,6,6,5,5,5,5/
      DATA LDM10 /-1,1,20,20,20,20,100,100,100,100,100/
C
      END
