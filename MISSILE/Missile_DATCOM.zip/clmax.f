      SUBROUTINE CLMAX(CBAR, DELTAY, XO, THN, CAM, XNM, REN, SCLMAX)
C ***
C ***
C *** CALCULATE SUBSONIC SECTION MAXIMUM LIFT 
C ***
C *** REFERENCE   DATCOM SECTION 4.1.1.4
C ***
C *** INPUT   CBAR    WING MEAN AERODYNAMIC CHORD
C ***         DELTAY  DIFFERENCE IN AIRFOIL ORDINATE BETWEEN
C ***                 .15% AND 6% CHORD,PER CENT CHORD
C ***         XO      AIRFOIL CHORD STATIONS
C ***         THN     AIRFOIL THICKNESS DISTRIBUTION
C ***         CAM     AIRFOIL CAMBER LINE
C ***         XNM     NUMBER OF MACH NUMBERS
C ***         REN     REYNOLDS NUMBER PER FOOT
C ***
C *** OUTPUT  SCLMAX  SECTION MAXIMUM LIFT COEFFICIENT
C ***
C
      COMMON /CONST/ PI,RAD,UNUSED,KAND
C
C ***
      DIMENSION SCLMAX(20), XO(50), THN(50), CAM(50), REN(20)
      DIMENSION PARM58(26), EVAL58(52), PARM59(30), EVAL59(160),
     1          PARM60(20), EVAL60(30), PARM61(16), EVAL61(32),
     2          PARM62(8),  EVAL62(8),  IROUT(2)
      LOGICAL   CAMBER
C
C    DATCOM FIGURE 4.1.1.4-5
C
      DATA PARM58 /0.,1.,1.1,1.25,1.50,2.00,2.25,2.50,3.00,3.50,4.00,
     1    4.50,5.00,.3,.35,.4,.45,9*0./
      DATA EVAL58 /2*.8,.81,.85,.98,1.2,1.31,1.43,1.58,1.59,1.55,1.47,
     1    1.42,2*.8,.81,.85,.98,1.2,1.31,1.43,1.51,1.52,1.48,1.41,1.41,
     2    2*.8,.81,.85,.98,1.2,1.31,1.39,1.44,1.45,1.43,1.35,1.35,
     3    2*.8,.81,.85,.98,1.2,1.29,1.33,1.35,1.35,1.35,1.35,1.35/
C
C    DATCOM FIGURE 4.1.1.4-6
C
      DATA PARM59 /0.,1.,1.5,2.,2.5,3.,3.5,4.,4.5,5.,0.,.02,.04,.06,
     1    6*0.,.15,.30,.40,.50,6*0./
      DATA EVAL59 /10*0.,
     1 0.,.12,.23,.32,.23,.12,.07,.07,.06,.03,
     2 0.,.27,.36,.40,.30,.16,3*.1,.06,
     3 0.,.27,.36,.40,.30,.16,3*.1,.06,
     4 10*0.,
     5 2*0.,.17,.22,.25,.15,.08,.06,.08,.02,
     6 0.,.15,.27,.40,.33,.18,.08,.06,.07,.01,
     7 0.,.53,.64,.56,.38,.22,.10,2*.06,0.,
     8 10*0.,
     9 2*0.,.05,.19,.23,.14,.07,2*.06,.02,
     A 0.,.16,.27,.37,.34,.19,.10,.07,.09,.05,
     B 0.,.39,.50,.50,.40,.25,.13,.10,.13,.09,
     C 10*0.,
     D 2*0.,.04,.10,.15,.14,.09,.07,.10,.08,
     E 0.,.10,.19,.27,.30,.22,.15,.13,.18,.12,
     F 0.,.26,.36,.43,.45,.30,.21,.20,.23,.20 /
C
C    DATCOM FIGURE 4.1.1.4-7A
C
      DATA PARM60 /0.,.25,1.,1.75,2.,2.75,3.,3.5,4.,4.5,.35,.4,.45,7*0./
      DATA EVAL60 /0.,.16,.19,.20,.18,.08,.05,-.01,-.03,-.04,
     1             0.,.16,.19,.20,.18,.08,.05,.03,.04,.02,
     2             0.,.16,.19,.20,.18,.08,.08,.10,.12,.08 /
C
C    DATCOM FIGURE 4.1.1.4-7B
C
      DATA PARM61 /0.,1.,1.5,2.,2.5,3.,3.5,4.,3.E06,6.E06,9.E06,2.5E07,
     1    4*0./
      DATA EVAL61 /0.,-.09,2*-.13,-.09,-.11,-.20,-.24,
     1             2*0.,-.08,-.07,2*-.03,-.07,-.10,
     2             8*0.,0.,.12,.14,.07,.01,.11,.18,.20 /
C
C    DATCOM FIGURE 4.1.1.4-8A
C
      DATA PARM62 /0.,1.,1.5,2.,2.5,3.,3.5,4./
      DATA EVAL62 /2*0.,-.19,-.37,-.46,3*-.5/
      DATA DUM    /1.0/
C
C  CALCULATE AIRFOIL GEOMETRIC PARAMETERS
C
      XOVC = UNUSED
      COVC = UNUSED
      CXVC = UNUSED
      DO 10 I=2,50
        IF(XO(I) .LE. UNUSED) GO TO 20
        IF(THN(I) .GT. THN(I-1)) XOVC = XO(I)
        IF(CAM(I) .GT. CAM(I-1)) COVC = CAM(I)
        IF(CAM(I) .GT. CAM(I-1)) CXVC = XO(I)
   10 CONTINUE
   20 CONTINUE
      CAMBER = COVC .GT. UNUSED
      NMACH  = XNM + 0.5
C
C  CLMAX BASE
C
      IF(ABS(XOVC-.3) .LE. 1.E-5) XOVC = .3
      CALL MVLOOK(1, 4, 13, 52, DUM, PARM58(14), PARM58(1), EVAL58,
     1            DELTAY, XOVC, 1.0, CLBASE)
C
C  DELTA CLMAX
C
      DEL1=0.0
      DEL2=0.0
      IF(.NOT.CAMBER) GO TO 1000
      CALL MVLOOK(4, 4, 10, 160, PARM59(21), PARM59(11), PARM59(1),
     1            EVAL59, DELTAY, COVC, CXVC, DEL1)
      IF(XOVC .EQ. 0.30) GO TO 1000
      CALL MVLOOK(1, 3, 10, 30, DUM, PARM60(11), PARM60(1), EVAL60,
     1            DELTAY, COVC, 1.0, DEL2)
 1000 CONTINUE
      IN=0
      DO 1010 I=1,NMACH
         RN = REN(I)*CBAR
         IF(RN .LT. 1.0) RN = 9.0E6
         CALL MVLOOK(1, 4, 8, 32, DUM, PARM61(9), PARM61(1), EVAL61,
     1               DELTAY, RN, 1.0, DEL3)
         CALL LNTRP(PARM62, EVAL62, 8, DELTAY, DEL4)
         CLMAX0=CLBASE+DEL1+DEL2+DEL3
         SCLMAX(I)=CLMAX0
 1010 CONTINUE
      RETURN
      END
