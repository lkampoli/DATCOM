      SUBROUTINE FLAP2(CFOC,TANPHE,ADEL)
C
C  THIS ROUTINE CALCULATES THE TWO-DIMENSIONAL FLAP 
C  EFFECTIVENESS OF A PLAIN TRAILING EDGE FLAP
C
C  WRITTEN BY W. BLAKE, WL/FIGC
C
      COMMON /CONST/ PI,RAD,UNUSED,KAND
      DIMENSION XTAB(6),CTAB(16),YTAB(96)
C
C                   FIGURE 6.1.1.1-39B
C
      DATA XTAB
     1  / 0.05,  0.10,  0.15,  0.20,  0.25,  0.50 /
      DATA CTAB
     1  / 0.70,  0.72,  0.74,  0.76,  0.78,  0.80,  0.82,  0.84,  0.86,
     2    0.88,  0.90,  0.92,  0.94,  0.96,  0.98,  1.00 /
      DATA YTAB
     1 /  .356,    .382,    .409,    .431,    .452,    .548,
     2    .399,    .426,    .452,    .477,    .498,    .583,
     3    .442,    .471,    .499,    .523,    .543,    .619,
     4    .485,    .521,    .548,    .569,    .589,    .659,
     5    .530,    .569,    .594,    .613,    .630,    .693,
     6    .578,    .614,    .639,    .657,    .671,    .729,
     7    .619,    .655,    .678,    .692,    .709,    .761,
     8    .659,    .696,    .713,    .733,    .746,    .793,
     9    .700,    .734,    .750,    .765,    .778,    .819,
     A    .742,    .771,    .789,    .800,    .810,    .850,
     B    .784,    .809,    .824,    .838,    .843,    .875,
     C    .826,    .843,    .860,    .865,    .873,    .900,
     D    .865,    .885,    .895,    .900,    .903,    .921,
     E    .910,    .921,    .928,    .931,    .933,    .938,
     F    .951,    .962,    .964,    .966,    .967,    .968,
     G   1.000,   1.000,   1.000,   1.000,   1.000,   1.000/
C
C  THEORETICAL 2-D EFFECTIVENESS - DATCOM FIG. 6.1.4.1-14 (INSET)
C
      ADELT=(ACOS(2.*CFOC-1.)-SQRT(1.-(2.*CFOC-1.)**2))/PI - 1.
C
C  EMPIRICAL CORRECTION - DATCOM FIG. 6.1.1.1-39B
C
      CLARAT=.9-.15/.14*TANPHE
      CALL MVLOOK(1,16,6,96,1,CTAB,XTAB,YTAB,CFOC,CLARAT,1,ANS)
      ADEL=ADELT*ANS
C
      RETURN
      END
