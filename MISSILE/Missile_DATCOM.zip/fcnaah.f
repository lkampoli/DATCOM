      SUBROUTINE FCNAAH(MACH, DELY, SLE, ALPHA, CNA, CNAA)
C ***
C *** COMPUTES SUPERSONIC NON-LINEAR NORMAL FORCE PER SINE(ALP)**2
C ***
C *** REFERENCE   DATCOM SECTION 4.1.3.3
C ***
C *** LIMITATIONS   ALP .LT. 90 DEG
C ***               STRAIGHT TAPERED FINS
C ***
C *** INPUT   MACH    MACH NUMBER
C ***         DELY    DIFFERENCE IN AIRFOIL ORDINATE BETWEEN
C ***                 .15% AND 6% CHORD,PER CENT CHORD LENGTH
C ***         SLE     LEADING EDGE SWEEP ANGLE, DEG
C ***         ALP     ANGLE-OF-ATTACK, DEG
C ***         CNA     LINEAR CN, CN PER ALP AT ALP = 0, 1/DEG
C ***
C *** OUTPUT  CNAA    CN PER SIN(ALP)**2,REFERENCED TO EXPOSED
C ***                 PLANFORM AREA OF TWO PANELS
C ***
C ***
C
      COMMON /CONST/PI,RAD,UNUSED,KAND
C
      DIMENSION XRCNA(11), YRCNA(8), ZRCNAL(88), ZRCNAR(88)
      DIMENSION XC(9), YC(11), ZC(99)
      DIMENSION X59BL(6), Y59B(10), Z59BL(60), X59BR(11), Z59BR(110)
      DIMENSION X60AL(6), Y60A(12), Z60AL(72), X60AR(13), Z60AR(156)
      DIMENSION Z60AR1(78), Z60AR2(78)
      DIMENSION X61A(9), Y61A(9)
C
      EQUIVALENCE (Z60AR(1),Z60AR1(1)), (Z60AR(79),Z60AR2(1))
C
      REAL MACH
C
C ***
C *** DATA STATEMENTS
C ***
      DATA DUM / 1.0 /
C ***
C *** DATCOM FIGURE 4.1.3.2-60    (CNA/CNAT)
C ***
      DATA XRCNA / 0.00,  .10,  .20,  .30,  .40,  .50,  .60,  .70,
     *              .80,  .90, 1.00 /
      DATA YRCNA / 0.0,  4.0,  8.0, 12.0, 20.0, 30.0, 50.0, 70.0 /
      DATA ZRCNAL /
     *     1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000,
     *     1.000, 1.000, 1.000,
     *     1.050, 1.050, 1.050, 1.050, 1.050, 1.050,  .985,  .945,
     *      .915,  .900,  .900,
     *     1.040, 1.040, 1.040, 1.040, 1.040, 1.040,  .965,  .908,
     *      .870,  .850,  .840,
     *     1.120, 1.120, 1.120, 1.120, 1.120, 1.015,  .940,  .880,
     *      .838,  .810,  .796,
     *     1.110, 1.110, 1.110, 1.110, 1.110, 1.000,  .903,  .840,
     *      .795,  .765,  .750,
     *     1.080, 1.080, 1.080, 1.080, 1.080,  .954,  .865,  .800,
     *      .750,  .720,  .700,
     *     1.200, 1.200, 1.200, 1.200, 1.043,  .907,  .817,  .750,
     *      .707,  .675,  .660,
     *     1.140, 1.140, 1.140, 1.140,  .975,  .857,  .772,  .717,
     *      .675,  .650,  .632 /
      DATA ZRCNAR /
     *     1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000,
     *     1.000, 1.000, 1.000,
     *     1.150, 1.150, 1.150, 1.150, 1.095, 1.040,  .990,  .960,
     *      .935,  .915,  .900,
     *     1.120, 1.120, 1.120, 1.120, 1.050,  .985,  .940,  .905,
     *      .880,  .857,  .840,
     *     1.150, 1.150, 1.150, 1.080, 1.005,  .945,  .902,  .870,
     *      .842,  .820,  .796,
     *     1.220, 1.140, 1.050,  .980,  .930,  .890,  .853,  .823,
     *      .795,  .770,  .750,
     *     1.130, 1.050,  .980,  .925,  .880,  .845,  .810,  .782,
     *      .752,  .730,  .700,
     *     1.020,  .942,  .895,  .855,  .820,  .790,  .760,  .735,
     *      .710,  .685,  .660,
     *     1.000,  .920,  .870,  .825,  .790,  .755,  .728,  .700,
     *      .678,  .655,  .632 /
C ***
C *** DATCOM FIGURE 4.1.3.3-59A   (C)
C ***
      DATA XC /  0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0 /
      DATA YC /  .70,  .80,  .90, 1.00, 1.10, 1.25, 1.50, 2.00, 2.50,
     *          3.00, 3.25 /
      DATA ZC / 0.00, 3.50, 4.95, 5.85, 6.40, 6.83, 7.25, 7.57, 7.85,
     *          0.00, 3.00, 4.19, 4.95, 5.58, 6.04, 6.49, 6.80, 7.00,
     *          0.00, 2.34, 3.40, 4.00, 4.49, 4.91, 5.28, 5.63, 5.92,
     *          0.00, 1.86, 2.78, 3.20, 3.53, 3.90, 4.25, 4.60, 4.85,
     *          0.00, 1.42, 2.20, 2.68, 3.05, 3.34, 3.62, 3.87, 4.06,
     *          0.00, 1.10, 1.62, 1.94, 2.19, 2.40, 2.64, 2.80, 2.95,
     *          0.00,  .92, 1.35, 1.65, 1.84, 2.00, 2.17, 2.28, 2.35,
     *          0.00,  .73, 1.10, 1.35, 1.54, 1.67, 1.76, 1.83, 1.85,
     *          0.00,  .62,  .94, 1.00, 1.10, 1.12, 1.12, 1.10, 1.09,
     *          0.00,  .54,  .72,  .68,  .42, 0.00, 0.00, 0.00, 0.00,
     *          0.00,  .45,  .63,  .51,  .23, 0.00, 0.00, 0.00, 0.00 /
C ***
C *** DATCOM FIGURE 4.1.3.3-59B   (SUBSONIC LE CNAA)
C ***
      DATA X59BL /  0.,   .5,   .6,   .7,   .8,  1.0 /
      DATA Y59B  /     .6,   .8,  1.0,  1.2,  1.6,  2.0,  2.4,  2.6,
     *                2.8,  3.0 /
      DATA Z59BL /  2*2.03, 1.95, 1.90, 1.86, 1.78,
     *              2*1.94, 1.87, 1.82, 1.77, 1.70,
     *              2*1.82, 1.75, 1.69, 1.64, 1.57,
     *              2*1.71, 1.63, 1.58, 1.54, 1.46,
     *              2*1.51, 1.44, 1.37, 1.32, 1.24,
     *              2*1.32, 1.24, 1.16, 1.10,  .99,
     *               2*.98,  .89,  .82,  .75,  .65,
     *               2*.58,  .52,  .47,  .42,  .37,
     *               2*.18,  .15,  .12,  .13,  .17,
     *              2*0.00, 0.00, 0.00, 0.00, 0.00 /
      DATA X59BR /    0.0,   .1,   .2,   .3,   .4,   .5,   .6,   .7,
     *                 .8,   .9,  1.0 /
      DATA Z59BR /   1.69, 1.69, 1.69, 1.69, 1.69, 1.69, 1.69, 1.70,
     *               1.72, 1.75, 1.77,
     *               1.69, 1.66, 1.64, 1.62, 1.60, 1.60, 1.61, 1.63,
     *               1.65, 1.67, 1.69,
     *               1.69, 1.64, 1.59, 1.56, 1.52, 1.51, 1.51, 1.51,
     *               1.52, 1.54, 1.57,
     *               1.69, 1.61, 1.55, 1.49, 1.44, 1.41, 1.39, 1.40,
     *               1.41, 1.43, 1.46,
     *               1.69, 1.56, 1.45, 1.36, 1.28, 1.22, 1.18, 1.17,
     *               1.18, 1.20, 1.23,
     *               1.69, 1.52, 1.34, 1.20, 1.08,  .99,  .94,  .91,
     *                .91,  .94,  .98,
     *               1.69, 1.40, 1.14,  .94,  .80,  .70,  .65,  .62,
     *                .61,  .63,  .65,
     *               1.69, 1.32, 1.08,  .89,  .74,  .64,  .54,  .47,
     *                .42,  .38,  .37,
     *               1.69, 1.32, 1.04,  .84,  .67,  .54,  .45,  .34,
     *                .27,  .20,  .17,
     *               1.69, 1.31, 1.02,  .79,  .61,  .46,  .33,  .21,
     *                .12,  .03, 0.00 /
C ***
C *** DATCOM FIGURE 4.1.3.3-60A   (SUPERSONIC LE, DETACHED SHOCK CNAA)
C ***
      DATA X60AL /   0.00,  .20,  .40,  .60,  .80, 1.00 /
      DATA Y60A  /   1.00, 1.20, 1.40, 1.60, 1.80, 2.00, 2.20, 2.26,
     *               2.40, 2.60, 2.80, 3.00 /
      DATA Z60AL /   1.60, 1.35, 1.12,  .94,  .79,  .68,
     *               1.60, 1.35, 1.12,  .94,  .79,  .68,
     *               1.60, 1.35, 1.12,  .94,  .79,  .68,
     *               1.60, 1.35, 1.12,  .94,  .79,  .68,
     *               1.60, 1.35, 1.12,  .94,  .79,  .68,
     *               1.60, 1.35, 1.12,  .94,  .79,  .68,
     *               1.60, 1.35, 1.12,  .94,  .79,  .68,
     *               1.60, 1.35, 1.12,  .94,  .79,  .68,
     *               1.48, 1.25, 1.03,  .85,  .70,  .60,
     *                .84,  .63,  .50,  .42,  .37,  .37,
     *                .34,  .25,  .17,  .13,  .11,  .16,
     *               0.00, 0.00, 0.00, 0.00, 0.00, 0.00 /
      DATA X60AR /
     *     0.000,  .200,  .220,  .240,  .270,  .300,  .320,  .375,
     *      .400,  .471,  .600,  .800, 1.000 /
      DATA Z60AR1 /  1.68, 1.46, 1.45, 1.40, 1.31, 1.23, 1.16, 1.05,
     *               1.00,  .88,  .72,  .64,  .68,
     *               1.68, 1.41, 1.40, 1.40, 1.31, 1.23, 1.16, 1.05,
     *               1.00,  .88,  .72,  .64,  .68,
     *               1.68, 1.37, 1.35, 1.33, 1.31, 1.23, 1.16, 1.05,
     *               1.00,  .88,  .72,  .64,  .68,
     *               1.68, 1.30, 1.26, 1.24, 1.21, 1.18, 1.16, 1.05,
     *               1.00,  .88,  .72,  .64,  .68,
     *               1.68, 1.24, 1.20, 1.16, 1.14, 1.11, 1.10, 1.05,
     *               1.00,  .88,  .72,  .64,  .68,
     *               1.68, 1.16, 1.11, 1.10, 1.07, 1.04, 1.02,  .96,
     *                .94,  .88,  .72,  .64,  .68 /
      DATA Z60AR2 /  1.68, 1.11, 1.07, 1.03, 1.00,  .97,  .95,  .89,
     *                .87,  .81,  .72,  .64,  .68,
     *               1.68, 1.10, 1.03, 1.02,  .98,  .95,  .93,  .87,
     *                .85,  .79,  .72,  .64,  .68,
     *               1.68, 1.07, 1.00,  .98,  .95,  .90,  .87,  .81,
     *                .79,  .72,  .63,  .56,  .60,
     *               1.68, 1.04,  .98,  .94,  .90,  .86,  .83,  .76,
     *                .74,  .66,  .54,  .42,  .37,
     *               1.68, 1.03,  .97,  .92,  .86,  .77,  .76,  .69,
     *                .65,  .57,  .42,  .25,  .16,
     *               1.68, 1.02,  .96,  .90,  .84,  .78,  .75,  .67,
     *                .63,  .52,  .34,  .10, 0.00 /
C ***
C *** DATCOM FIGURE 4.1.3.3-61A   (DELTA ALPHA FOR FULLY DETACHED SHOCK)
C ***
      DATA X61A /    1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0, 6.0, 7.0  /
      DATA Y61A /  59.375, 41.667, 33.000, 27.778, 23.611, 16.667,
     *             12.500,  8.750,  6.000 /
C
C ***
C *** CALCULATE  L.E. SWEEP PARM BTLE = BETA/TAN(SLE),
C ***            L.E. PARAMETER E, AND RCNA (CNA/CNAT)
C ***
      ALP  = ABS(ALPHA)
      BETA = SQRT(MACH*MACH-1.0)
      TSLE = TAN(SLE/RAD)
      IF(TSLE .LT. 0.0001) TSLE = 0.0001
      BTLE = BETA/TSLE
C ***
      DELN = RAD*ATAN(DELY/5.85)
      E    = CNA*RAD
      IF(TSLE .LE. 1.92) GO TO 10
        YARG = CNA*RAD*TSLE/1.92
        CALL MVLOOK(1, 11, 9, 99, DUM, YC, XC, ZC, DELN, YARG, 1.0, C)
        E = CNA*RAD*(TSLE/1.92+C*(TSLE/1.92-1.0))
   10 CONTINUE
C ***
      XARG = BTLE
      IF(BTLE .GT. 1.0) XARG = 1.0/BTLE
      IF(BTLE .LE. 1.0) CALL MVLOOK(1, 8, 11, 88, DUM, YRCNA, XRCNA,
     *                              ZRCNAL, XARG, DELN, 1.0, RCNA)
      IF(BTLE .GT. 1.0) CALL MVLOOK(1, 8, 11, 88, DUM, YRCNA, XRCNA,
     *                              ZRCNAR, XARG, DELN, 1.0, RCNA)
      IF(RCNA .GT. 1.0) RCNA = 1.0
C ***
C *** SUBSONIC L.E. - BTLE .LE. 1.0
C ***
      IF(BTLE .GT. 1.0) GO TO 20
        XARG = RCNA*BETA*TAN(ALP/RAD)
        YARG = E*BTLE
        IF(XARG .LE. 1.0) CALL MVLOOK(1, 10, 6, 60, DUM, Y59B, X59BL,
     *                                Z59BL, XARG, YARG, 1.0, CNAA)
        IF(XARG .LT. 0.0001) XARG = 0.0001
        XARG = 1.0/XARG
        IF(XARG .LT. 1.0) CALL MVLOOK(1, 10, 11, 110, DUM, Y59B, X59BR,
     *                                Z59BR, XARG, YARG, 1.0, CNAA)
   20 CONTINUE
C ***
C *** SUPERSONIC L.E. - BTLE .GT. 1.0
C ***
      IF(BTLE .LE. 1.0) GO TO 80
        CALL FSDETA(MACH, DELN, SLE, ALPS)
C       ***
C       *** SHOCK DETACHED AT ALP = 0.0 - ALPS .LE. 0.0
C       ***
        IF(ALPS .GT. 0.0) GO TO 30
          XARG = RCNA*BETA*TAN(ALP/RAD)
          IF(XARG .LE. 1.0) CALL MVLOOK(1, 12, 6, 72, DUM, Y60A, X60AL,
     *                                  Z60AL, XARG, E, 1.0, CNAA)
          IF(XARG .LT. 0.0001) XARG = 0.0001
          XARG = 1.0/XARG
          IF(XARG .LT. 1.0) CALL MVLOOK(1, 12, 13, 156, DUM, Y60A,
     *                                 X60AR, Z60AR, XARG, E, 1.0, CNAA)
   30   CONTINUE
C       ***
C       *** SHOCK ATTACHED AT ALP = 0.0
C       ***
        IF(ALPS .LE. 0.0) GO TO 70
          XARG = BETA*TAN(ALPS/RAD)
          CALL FIG60B(BETA, XARG, CNAAS)
          CALL LNTRP(X61A, Y61A, 9, BETA, SLOPE)
          DALP = SLOPE*CNAAS
          IF(ALPS+DALP .GE. 90.0) DALP = 89.9-ALPS
C         ***
C         *** ATTACHED SHOCK - ALP .LE. ALPS
C         ***
          IF(ALP .GT. ALPS) GO TO 40
            XARG = BETA*TAN(ALP/RAD)
            CALL FIG60B(BETA, XARG, CNAA)
   40     CONTINUE
C         ***
C         *** INCIPIENT SEPARATION - ALPS .LT. ALP .LT. ALPS+DALP
C         ***
          IF(ALP .LE. ALPS)      GO TO 50
          IF(ALP .GT. ALPS+DALP) GO TO 50
            XARG = RCNA*BETA*TAN((ALPS+DALP)/RAD)
            IF(XARG .LE. 1.0) CALL MVLOOK(1, 12, 6, 72, DUM, Y60A,
     *                             X60AL, Z60AL, XARG, E, 1.0, CNAAP)
            XARG = 1.0/XARG
            IF(XARG .LT. 1.0) CALL MVLOOK(1, 12, 13, 156, DUM, Y60A,
     *                             X60AR, Z60AR, XARG, E, 1.0, CNAAP)
            CNAA = CNAAS-(ALP-ALPS)*(CNAAS-CNAAP)/DALP
   50     CONTINUE
C         ***
C         *** FULLY SEPARATED SHOCK - ALP .GE. ALPS+DALP
C         ***
          IF(ALP .LT. ALPS+DALP) GO TO 60
            XARG = RCNA*BETA*TAN(ALP/RAD)
            IF(XARG .LE. 1.0) CALL MVLOOK(1, 12, 6, 72, DUM, Y60A,
     *                             X60AL, Z60AL, XARG, E, 1.0, CNAA)
            XARG = 1.0/XARG
            IF(XARG .LT. 1.0) CALL MVLOOK(1, 12, 13, 156, DUM, Y60A,
     *                             X60AR, Z60AR, XARG, E, 1.0, CNAA)
   60     CONTINUE
   70   CONTINUE
   80 CONTINUE
C
      RETURN
      END
