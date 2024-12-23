      SUBROUTINE FCLMSB(AR, MACH, DELY, SLE, TAPER,
     1                  C1, C2, CLM, ACLM)
C ***
C *** COMPUTES SUBSONIC MAXIMUM LIFT AND ANGLE-OF-ATTACK
C *** FOR MAXIMUM LIFT FOR LOW ASPECT RATIO WINGS
C ***
C *** REFERENCE   DATCOM SECTION 4.1.3.4
C ***             METHOD 3
C ***
C *** LIMITATIONS   MACH .LE. 0.6
C ***               AR .LE. 4.0/((C1+1)*COS(SLE))
C ***
C *** INPUT   AR      EXPOSED FIN ASPECT RATIO OF TWO PANELS
C ***         MACH    MACH NUMBER
C ***         DELY    DIFFERENCE IN AIRFOIL ORDINATE BETWEEN
C ***                 .15% AND 6% CHORD,PER CENT CHORD
C ***         SLE     LEADING EDGE SWEEP ANGLE, DEG
C ***         TAPER   EXPOSED FIN TAPER RATIO
C ***
C *** OUTPUT  C1,C2   TAPER RATIO CORRECTION FACTORS(DATC FIG 4.1.3.4-24
C ***         CLM     MAXIMUM LIFT COEFFICIENT,REFERENCED TO EXPOSED
C ***                 PLANFORM AREA OF TWO PANELS
C ***         ACLM    ANGLE-OF-ATTACK FOR CLM,DEGREES
C ***
C ***
C
      COMMON /CONST/PI,RAD,UNUSED,KAND
C
      DIMENSION XC1(11), YC1(11)
      DIMENSION XC2(11), YC2(11)
      DIMENSION XCLMB(19), YCLMB(6),  ZCLMB(114)
      DIMENSION XDCLM(15), YDCLM(3), ZDCLM(45)
      DIMENSION XACLMB(10), YACLMB(10)
      DIMENSION X1DA(10), Y1DA(10), Z1DA(100)
      DIMENSION X2DA(20), Y2DA(3), Z2DA(60)
C
      REAL MACH
C
C ***
C *** DATA STATEMENTS
C ***
      DATA DUM / 1.0 /
C ***
C *** DATCOM FIGURE 4.1.3.4-24B   (C1 AND C2)
C ***
      DATA XC1 /0.,.1,.2,.3,.4,.5,.6,.7,.8,.9,1./
      DATA YC1 /0.,.225,.47,.496,.43,.32,.21,.125,.075,.0475,.0/
      DATA XC2 /0.,.1,.2,.3,.4,.5,.6,.7,.8,.9,1./
      DATA YC2 /0.,.21,.5,.9,1.08,1.05,1.,.94,.9,.86,.85/
C ***
C *** DATCOM FIGURE 4.1.3.4-23B  (CLMAX BASE)
C ***
      DATA XCLMB /  0.00,  .20,  .40,  .60,  .80, 1.00, 1.20, 1.40,
     *              1.60, 1.80, 2.00, 2.20, 2.40, 2.60, 2.80, 3.00,
     *              3.20, 3.40, 3.60 /
      DATA YCLMB /  0.00,  .25,  .50,  .75, 1.00, 1.35 /
      DATA ZCLMB /
     *      .68, 1.18, 1.37, 1.43, 1.44, 1.37, 1.24, 1.17, 1.10, 1.06,
     *     1.02,  .99,  .96,  .94,  .92,  .91,  .90,  .89,  .88,
     *      .65, 1.11, 1.29, 1.36, 1.37, 1.31, 1.20, 1.13, 1.07, 1.03,
     *      .99,  .96,  .94,  .92,  .90,  .89,  .88,  .87,  .86,
     *      .60, 1.07, 1.23, 1.29, 1.30, 1.25, 1.16, 1.10, 1.05, 1.01,
     *      .97,  .94,  .92,  .90,  .88,  .87,  .86,  .85,  .84,
     *      .55,  .99, 1.15, 1.23, 1.25, 1.21, 1.14, 1.09, 1.04,  .99,
     *      .95,  .92,  .90,  .88,  .86,  .85,  .84,  .83,  .83,
     *      .50,  .91, 1.08, 1.17, 1.20, 1.17, 1.13, 1.07, 1.03,  .98,
     *      .94,  .90,  .88,  .86,  .84,  .83,  .82,  .82,  .82,
     *      .50,  .91, 1.08, 1.17, 1.20, 1.17, 1.11, 1.06, 1.01,  .96,
     *      .91,  .88,  .85,  .83,  .82,  .81,  .80,  .80,  .80 /
C ***
C *** DATCOM FIGURE 4.1.3.4-24A  (DCLMAX)
C ***
      DATA XDCLM /    0.,  1.,  2.,  3.,  4.,  5.,  6.,  7.,  8.,  9.,
     *               10., 11., 12., 13., 14.  /
      DATA YDCLM /   .20, .40, .60  /
      DATA ZDCLM /  -.11,-.10,-.09,-.05,0.00, .06, .12, .18, .23, .29,
     *               .34, .37, .36, .32, .23,
     *              -.11,-.10,-.09,-.05,0.00, .05, .10, .15, .20, .26,
     *               .30, .33, .32, .28, .19,
     *              -.11,-.10,-.09,-.05,0.00, .04, .08, .12, .15, .18,
     *               .21, .23, .21, .16, .08  /
C ***
C *** DATCOM FIGURE 4.1.3.4-25A  (ACLM BASE)
C ***
      DATA XACLMB /  0.00,  .40,  .80, 1.20, 1.60, 2.00, 2.40, 2.80,
     *               3.20, 3.60  /
      DATA YACLMB /  35.0, 35.0, 35.0, 32.0, 28.0, 25.0, 23.2, 22.0,
     *               21.5, 21.0  /
C ***
C *** DATCOM FIGURE 4.1.3.4-25B  (DACLM PART 1 - X .LE. 4.5)
C ***
      DATA X1DA /    0.00,  .50, 1.00, 1.50, 2.00, 2.50, 3.00, 3.50,
     *               4.00, 4.50  /
      DATA Y1DA /     0.0,  2.0,  3.0,  4.0,  5.0,  6.0,  7.0,  8.0,
     *                9.0, 30.0  /
      DATA Z1DA /
     *     10.0,  8.5,  6.9,  5.5,  4.0,  2.6,  1.5,   .7,   .1,  0.0,
     *      8.7,  7.3,  5.3,  4.2,  2.6,  1.4,   .5,  -.2,  -.5,  0.0,
     *      7.5,  5.9,  4.2,  2.5,  1.2,  0.0,  -.7, -1.1,  -.8,  0.0,
     *      5.5,  3.4,  1.6,  0.0, -1.3, -2.1, -2.5, -2.0,  -.8,  0.0,
     *      3.0,   .7, -1.4, -3.3, -4.3, -4.3, -3.1, -2.0,  -.8,  0.0,
     *       .3, -2.5, -4.7, -5.8, -5.3, -4.3, -3.1, -2.0,  -.8,  0.0,
     *     -2.2, -5.0, -6.7, -6.3, -5.3, -4.3, -3.1, -2.0,  -.8,  0.0,
     *     -3.3, -6.6, -7.2, -6.3, -5.3, -4.3, -3.1, -2.0,  -.8,  0.0,
     *     -4.2, -7.0, -7.2, -6.3, -5.3, -4.3, -3.1, -2.0,  -.8,  0.0,
     *     -8.5, -7.9, -7.2, -6.3, -5.3, -4.3, -3.1, -2.0,  -.8,  0.0 /
C ***
C *** DATCOM FIGURE 4.1.3.4-25B  (DACLM PART 2 - X .GE. 4.5)
C ***
      DATA X2DA /     4.5,  5.0,  5.5,  6.0,  6.5,  7.0,  7.5,  8.0,
     *                8.5,  9.0,  9.5, 10.0, 10.5, 11.0, 11.5, 12.0,
     *               12.5, 13.0, 13.5, 14.0  /
      DATA Y2DA /     .20,  .40,  .60   /
      DATA Z2DA /
     *      0.0,   .5,   .9,  1.4,  1.9,  2.5,  3.3,  4.0,  4.6,  5.6,
     *      6.4,  7.3,  8.2,  9.2, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0,
     *      0.0,   .2,   .4,   .7,  1.2,  1.7,  2.4,  3.0,  3.7,  4.6,
     *      5.2,  6.0,  6.9,  7.8,  8.6,  9.5, 10.4, 11.4, 12.3, 13.5,
     *      0.0,  0.0,   .1,   .2,   .5,   .7,  1.0,  1.3,  1.6,  2.0,
     *      2.5,  3.0,  3.6,  4.3,  4.9,  5.5,  6.2,  7.0,  7.6,  8.5 /
C
C ***
C *** CALCULATE C1, C2, AND ARTEST - 4.0/((C1+1)*COS(SLE))
C ***
      CALL LNTRP(XC1, YC1, 11, TAPER, C1)
      CALL LNTRP(XC2, YC2, 11, TAPER, C2)
      ARTEST = 4.0/((C1+1.0)*COS(SLE/RAD))
C ***
C *** CALCULATE CLM
C ***
      BETA = SQRT(1.0 - MACH*MACH)
      XARG = (C1+1.0)*AR*COS(SLE/RAD)/BETA
      IF(XARG .GT. 3.60) XARG = 3.60
      YARG = DELY
      IF(YARG .GT. 1.35) YARG = 1.35
      CALL MVLOOK(1, 6, 19, 114, DUM, YCLMB, XCLMB, ZCLMB,
     *            XARG, YARG, 1.0, CLMB)
      XARG = (C2+1.0)*AR*TAN(SLE/RAD)
      YARG = MACH
      IF(YARG .LT. 0.20) YARG = 0.20
      CALL MVLOOK(1, 3, 15, 45, DUM, YDCLM, XDCLM, ZDCLM,
     *            XARG, YARG, 1.0, DCLM)
      CLM = CLMB + DCLM
C ***
C *** CALCULATE ACLM
C ***
      XARG = (C1+1.0)*AR*COS(SLE/RAD)/BETA
      IF(XARG .GT. 3.60) XARG = 3.60
      CALL LNTRP(XACLMB, YACLMB, 10, XARG, ACLMB)
      XARG = (C2+1.0)*AR*TAN(SLE/RAD)
      YARG = AR*COS(SLE/RAD)*(1.0+4.0*TAPER*TAPER)
      IF(XARG .LE. 4.5) CALL MVLOOK(1, 10, 10, 100, DUM, Y1DA, X1DA,
     *                              Z1DA, XARG, YARG, 1.0, DACLM)
      YARG = MACH
      IF(YARG .LT. 0.20) YARG = 0.20
      IF(XARG .GT. 4.5) CALL MVLOOK(1, 3, 20, 60, DUM, Y2DA, X2DA,
     *                              Z2DA, XARG, YARG, 1.0, DACLM)
      ACLM = ACLMB + DACLM
C ***
C
      RETURN
      END
