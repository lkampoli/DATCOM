      SUBROUTINE INZFLC
C
C***  INITIALIZE ALL FLIGHT CONDITIONS
C
C***  FLIGHT CONDITION INPUTS
C
      COMMON /FLC/    NALPHA,ALPHA(20),BETA,PHI,NMACH,MACH(20),
     1                ALT(20),REN(20),VINF(20),TINF(20),PINF(20)
      REAL NALPHA,NMACH,MACH
C
      COMMON /TOTALC/ BALPHA(20),BBETA(20),BPHI(20),ALPTOT(20)
C
C***  INTERNAL COMPUTATION VALUES (CONFIGURATION INCREMENTING)
C
      COMMON /INC/ COREC(128),ZIP,ALP1(20),NAP
C
C***  PROGRAM CONSTANTS
C
      COMMON /CONST/  PI,RAD,UNUSED,KAND
C
      NALPHA=UNUSED
      NAP=UNUSED
      BETA=UNUSED
      PHI=UNUSED
      NMACH=UNUSED
      DO 1000 I=1,20
         ALT(I)=UNUSED
         ALPHA(I)=UNUSED
         MACH(I)=UNUSED
         REN(I)=UNUSED
         VINF(I)=UNUSED
         TINF(I)=UNUSED
         PINF(I)=UNUSED
         BALPHA(I)=UNUSED
         BBETA(I)=UNUSED
         BPHI(I)=UNUSED
         ALPTOT(I)=UNUSED
         ALP1(I)=UNUSED
 1000 CONTINUE
C
C     SET INCREMENTING CONSTANTS TO UNUSED
C
      ZIP=0.
C
      END
