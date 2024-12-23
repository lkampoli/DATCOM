      SUBROUTINE CDCS(ALPHA,RE,CHARL,MACH,CDC)
C
C***  SUBROUTINE TO CALCULATE CROSSFLOW DRAG COEFFICIENT
C***  USING THE METHOD OF JORGENSEN (NASA TN D-6996, FIGS. 1,2,3)
C
C***  BAKER REPORT AEDC-TR-75-124 USED AS GUIDE IN DATA FAIRING
C
C***  INPUTS
C
C ALPHA - ANGLE OF ATTACK, DEG.
C    RE - REYNOLDS NUMBER PER FOOT
C CHARL - CHARACTERISTIC LENGTH, FEET
C  MACH - FREESTREAM MACH NUMBER
C
C***  OUTPUT
C
C   CDC - CROSS FLOW DRAG COEFFICIENT
C
      COMMON /CONST/ PI,RAD,UNUSED,KAND
      COMMON /REFQN/  SREF,LREF,BREF,ROUGH,XCG,ZCG,SCALE,BLAYER,RHR
C
      REAL MACHC,MACH
C
      DIMENSION X1(20),TAB1(20),X4(6),TAB4(6),X2(12),TAB2(12),
     1          X3(10),TAB3(10,5),TMACH(5)
C
C ... DATA FOR FIGURE 1, UPPER CURVE (CDC VERSUS CROSS FLOW MACH)
C     DATA IN MACH RANGE .8 TO 1.5 ALTERED TO REFLECT BAKER CORRELATION
C
      DATA X1/0.,.4,.5,.6,.7,.8,.9,1.,1.1,1.2,1.3,1.4,1.5,1.6,
     1        1.8,2.0,2.2,2.8,4.0,4.8/
      DATA TAB1/1.2,1.29,1.36,1.50,1.50,1.50,1.41,1.35,1.32,1.28,
     1          1.25,1.23,1.21,1.20,1.20,1.20,1.20,1.20,1.20,1.20/
C
C ... DATA FOR FIGURE 1, LOWER CURVE (CDC VERSUS CROSS FLOW MACH)
C     ALTERED TO REFLECT BAKER AND MARTIN CORRELATIONS
C
      DATA X4/0.,.1,.2,.3,.4,.5/
      DATA TAB4/.28,.38,.48,.74,1.0,1.36/
C
C ... DATA FOR FIGURE 2 (CDC VERSUS LOG10 REYNOLDS NUMBER)
C
      DATA X2/4.0,4.5441,4.9542,5.1761,5.5563,5.6021,5.6990,5.7782,
     1        5.8451,5.9542,6.0,7.0/
      DATA TAB2/1.2,1.2,1.22,1.22,.72,.63,.26,.25,.26,.28,.29,.64/
C
C ... DATA FOR FIGURE 3 (CDC VERSUS LOG10 REYNOLDS NUMBER)
C
      DATA X3/6.0,6.301,6.4771,6.6021,6.699,6.7782,6.8451,6.9031,
     1        6.9542,7.0/
      DATA TAB3/.20,.45,.54,.59,.60,.59,.58,.55,.53,.52,
     1          .20,.45,.54,.59,.60,.59,.58,.55,.53,.52,
     2          .30,.48,.56,.60,.62,.61,.605,.60,.59,.58,
     3          .30,.45,.53,.60,.64,.69,.70,.64,.62,.60,
     4          .50,.53,.57,.60,.64,.69,.76,.88,.67,.63/
      DATA TMACH/0.00,0.25,0.30,0.35,0.40/
C
      MACHC=MACH*SIN(ABS(ALPHA)/RAD)
      REC=RE*CHARL*SIN(ABS(ALPHA)/RAD)
C
C***  CURVE SELECTION LOGIC
C
C     IF(MACHC-0.4)1000,1000,1030
C1000 IF(REC-1.E+4) 1060,1060,1010
C1010 IF(REC-1.E+6) 1070,1070,1020
C1020 IF(REC-1.E+7) 1080,1080,1040
C1030 IF(MACHC-0.5)1050,1050,1040
C
      IF(BLAYER.EQ.1.) GO TO 1030
C
C  TURBULENT BOUNDARY LAYER, INTERPOLATE DATA FOR FIGURE 1
C
      CALL LNTRP(X1,TAB1,20,MACHC,CDC1)
      CALL LNTRP(X4,TAB4,6,MACHC,CDC2)
      IF(MACHC.GE.0.5) CDC=CDC1
      IF(MACHC.LT.0.5) CDC=CDC2+0.5*(CDC1-CDC2)
      GO TO 1090
 1030 CONTINUE
C
C  NATURAL TRANSITION, USE MACHC AND REC CHARTS
C
      IF(MACHC.LE.0.4) GO TO 1000
      IF(MACHC.LT.0.5.AND.REC.GT.1.E+5) GO TO 1050
C
C ... MACHC > 0.5,  USING DATA FOR FIGURE 1, UPPER
C
 1040 CALL LNTRP(X1,TAB1,20,MACHC,CDC)
      GO TO 1090
C
C ... 0.4 < MACHC < 0.5, USING DATA FOR FIGURE 1, LOWER
C
 1050 CALL LNTRP(X4,TAB4,6,MACHC,CDC)
      GO TO 1090
C
C ... MACHC < 0.4, USE REC FOR CDC CALCULATION
C
 1000 IF(REC.GT.1.E+4.AND.REC.LE.1.E+6) GO TO 1070
      IF(REC.GT.1.E+6) GO TO 1080
C
C ... REC < 1.E+4, SHORT CIRCUIT FIGURE 2, USE CDC=1.2
C
 1060 CDC=1.2
      GO TO 1090
C
C ... 1E+4 < REC < 1E+6, USING DATA FOR FIGURE 2
C
 1070 CALL LNTRP(X2,TAB2,12,ALOG10(REC),CDC)
      GO TO 1090
C
C ... REC > 1E+6, USING DATA FOR FIGURE 3
C
 1080 IF(REC.GT.1.E+7) GO TO 1040
      CALL MVLOOK(1,5,10,50,0.,TMACH,X3,TAB3,ALOG10(REC),MACHC,0.,CDC)
C
 1090 CONTINUE
C
      RETURN
      END
