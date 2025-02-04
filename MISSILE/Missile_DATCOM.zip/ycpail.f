      SUBROUTINE YCPAIL(AR,TAPER,MACH,SWEEP,SSPAN,CFOC,YUP,CLD,YCP)
C
C  SUBROUTINE TO COMPUTE LATERAL C.P. WITH FLAP DEFLECTION
C  METHOD USES DATCOM SECTION 6.2.1.1 
C
C  WRITTEN BY W. BLAKE, WL/FIGC
C
C  VARIABLES
C
C  MACH  - MACH NUMBER
C  AR    - FIN ASPECT RATIO
C  TAPER - FIN TAPER RATIO
C  SWEEP - FIN LEADING EDGE SWEEP
C  SSPAN - FIN SEMI-SPAN STATIONS (10)
C  CFOC  - FLAP CHORD RATIO VALUES AT EACH SEMI-SPAN STATION (10)
C  YUP   - AIRFOIL SURFACE COORDINATES (50)
C  CLD   - FLAP LIFT EFFECTIVENESS DERIVATIVE
C  YCP   - LATERAL CENTER OF PRESSURE WITH FLAP DEFLECTION
C
      COMMON /CONST/ PI,RAD,UNUSED,KAND
      REAL MACH
      DIMENSION SSPAN(10),CFOC(10),YUP(50)
      DIMENSION X11123(3),X21123(4),X31123(11),X41123(3),
     1          F21123(3),Y1(44),Y2(44),Y3(44),Y4(44),Y5(44),Y6(44),
     2          Y7(44),Y8(44),Y9(44),YA(132),YB(132),YC(132)
      EQUIVALENCE (Y1(1),YA(1)),(Y2(1),YA(45)),(Y3(1),YA(89)),
     1            (Y4(1),YB(1)),(Y5(1),YB(45)),(Y6(1),YB(89)),
     2            (Y7(1),YC(1)),(Y8(1),YC(45)),(Y9(1),YC(89))
C
C                  FIGURE 6.2.1.1-23A-C
C
      DATA X31123
     1 / 0., .1, .2, .3,  .4, .5, .6, .7, .8, .9, 1. /
      DATA X21123
     1 / -40., 0., 40., 60. /
      DATA X11123
     1 / 2., 4., 8. /
      DATA X41123
     1 / 0., 0.5, 1. /
      DATA Y1
     1 / 0.000,  0.005,  0.014,  0.030,  0.055,  0.087,  0.126,
     2   0.163,  0.197,  0.222,  0.245,
     3   0.000,  0.005,  0.017,  0.036,  0.063,  0.100,  0.144,
     4   0.185,  0.224,  0.262,  0.282,
     5   0.000,  0.005,  0.017,  0.036,  0.064,  0.103,  0.149,
     6   0.192,  0.230,  0.264,  0.282,
     7   0.000,  0.005,  0.017,  0.036,  0.062,  0.098,  0.136,
     8   0.178,  0.215,  0.243,  0.259 /
      DATA Y2
     1 / 0.000,  0.007,  0.027,  0.054,  0.097,  0.145,  0.197,
     2   0.243,  0.292,  0.333,  0.363,
     3   0.000,  0.008,  0.029,  0.064,  0.113,  0.173,  0.235,
     4   0.300,  0.363,  0.414,  0.445,
     5   0.000,  0.008,  0.029,  0.064,  0.119,  0.180,  0.245,
     6   0.313,  0.374,  0.417,  0.443,
     7   0.000,  0.008,  0.029,  0.063,  0.108,  0.163,  0.220,
     8   0.273,  0.319,  0.350,  0.363 /
      DATA Y3
     1 / 0.000,  0.006,  0.032,  0.080,  0.145,  0.212,  0.275,
     2   0.336,  0.394,  0.446,  0.485,
     3   0.000,  0.012,  0.043,  0.093,  0.165,  0.255,  0.354,
     4   0.445,  0.525,  0.587,  0.627,
     5   0.000,  0.013,  0.045,  0.106,  0.189,  0.280,  0.375,
     6   0.465,  0.542,  0.592,  0.612,
     7   0.000,  0.013,  0.045,  0.106,  0.187,  0.258,  0.327,
     8   0.390,  0.443,  0.475,  0.485 /
      DATA Y4
     1 / 0.000,  0.006,  0.017,  0.038,  0.066,  0.100,  0.142,
     2   0.185,  0.232,  0.273,  0.293,
     3   0.000,  0.007,  0.023,  0.048,  0.079,  0.114,  0.154,
     4   0.200,  0.249,  0.284,  0.311,
     5   0.000,  0.007,  0.023,  0.048,  0.079,  0.114,  0.154,
     6   0.198,  0.240,  0.276,  0.304,
     7   0.000,  0.007,  0.023,  0.043,  0.072,  0.106,  0.148,
     8   0.190,  0.232,  0.264,  0.286 /
      DATA Y5
     1 / 0.000,  0.006,  0.025,  0.060,  0.100,  0.151,  0.214,
     2   0.294,  0.370,  0.430,  0.479,
     3   0.000,  0.009,  0.033,  0.075,  0.125,  0.185,  0.259,
     4   0.348,  0.427,  0.491,  0.538,
     5   0.000,  0.009,  0.033,  0.075,  0.125,  0.185,  0.259,
     6   0.340,  0.413,  0.472,  0.515,
     7   0.000,  0.008,  0.026,  0.065,  0.113,  0.170,  0.231,
     8   0.294,  0.350,  0.398,  0.432 /
      DATA Y6
     1 / 0.000,  0.007,  0.034,  0.072,  0.125,  0.187,  0.270,
     2   0.371,  0.480,  0.580,  0.662,
     3   0.000,  0.014,  0.052,  0.108,  0.180,  0.275,  0.390,
     4   0.510,  0.620,  0.718,  0.800,
     5   0.000,  0.014,  0.052,  0.113,  0.190,  0.280,  0.385,
     6   0.485,  0.578,  0.658,  0.729,
     7   0.000,  0.014,  0.052,  0.103,  0.163,  0.234,  0.308,
     8   0.379,  0.445,  0.504,  0.554 /
      DATA Y7
     1 / 0.000,  0.003,  0.015,  0.037,  0.065,  0.103,  0.147,
     2   0.190,  0.237,  0.280,  0.306,
     3   0.000,  0.004,  0.020,  0.045,  0.079,  0.118,  0.159,
     4   0.205,  0.252,  0.291,  0.315,
     5   0.000,  0.004,  0.020,  0.045,  0.079,  0.114,  0.156,
     6   0.200,  0.246,  0.286,  0.310,
     7   0.000,  0.004,  0.020,  0.045,  0.073,  0.108,  0.147,
     8   0.187,  0.230,  0.268,  0.291 /
      DATA Y8
     1 / 0.000,  0.008,  0.033,  0.063,  0.108,  0.162,  0.225,
     2   0.303,  0.389,  0.453,  0.500,
     3   0.000,  0.008,  0.033,  0.072,  0.122,  0.187,  0.260,
     4   0.348,  0.437,  0.504,  0.558,
     5   0.000,  0.008,  0.033,  0.072,  0.122,  0.187,  0.258,
     6   0.335,  0.413,  0.474,  0.522,
     7   0.000,  0.008,  0.033,  0.070,  0.110,  0.165,  0.229,
     8   0.290,  0.347,  0.400,  0.445 /
      DATA Y9
     1 / 0.000,  0.010,  0.035,  0.070,  0.118,  0.192,  0.291,
     2   0.405,  0.520,  0.626,  0.721,
     3   0.000,  0.014,  0.049,  0.104,  0.179,  0.275,  0.393,
     4   0.520,  0.653,  0.768,  0.850,
     5   0.000,  0.014,  0.050,  0.109,  0.186,  0.284,  0.393,
     6   0.505,  0.602,  0.690,  0.763,
     7   0.000,  0.014,  0.040,  0.094,  0.155,  0.230,  0.306,
     8   0.380,  0.458,  0.519,  0.578 /
C      
      IBEGIN=0
      IEND=10
      NSTA=1
      BETA=SQRT(1.-MACH**2)
      BAR=BETA*AR
      TANC4=TAN(SWEEP/RAD)-1./AR*(1.-TAPER)/(1.+TAPER)
      SWEEPB=ATAN(TANC4/BETA)*RAD
      TANPHE=(YUP(45)-YUP(49))/2./0.09
C
C     DETERMINE THE NUMBER OF SPAN STATIONS INPUT
C     AND CALCULATE THE INBOARD AND OUTBOARD FLAP POSITIONS
C      
      DO 1000 I=1,10
        IF(SSPAN(I) .NE. UNUSED)NSTA = I
 1000 CONTINUE
      BOVER2 = SSPAN(NSTA) - SSPAN(1)
      DO 1100 I=1,NSTA
        IF(CFOC(I) .GT. UNUSED .AND. IBEGIN .EQ. 0)IBEGIN=I
        IF(CFOC(I) .GT. UNUSED .AND. IBEGIN .NE. 0)IEND = I
 1100 CONTINUE
      ETAI=(SSPAN(IBEGIN)-SSPAN(1))/BOVER2
      ETAO=(SSPAN(IEND)-SSPAN(1))/BOVER2
C
C  ROLLING MOMENT CALCULATIONS
C
      T=0.
 2000 CONTINUE
      ETA=ETAI
      IF(T.EQ.1.) ETA=ETAO
C
C  TAPER RATIO = 0 TABLE LOOK-UP
C
      CALL MVLOOK(3,4,11,132,X11123,X21123,X31123,YA,
     1            ETA,SWEEPB,BAR,ANSD)
C
C  TAPER RATIO = 0.5 TABLE LOOK-UP
C
      CALL MVLOOK(3,4,11,132,X11123,X21123,X31123,YB,
     1            ETA,SWEEPB,BAR,ANS5)
C
C  TAPER RATIO = 1 TABLE LOOK=UP
C
      CALL MVLOOK(3,4,11,132,X11123,X21123,X31123,YC,
     1            ETA,SWEEPB,BAR,ANS1)
C
C  INBOARD SEGMENT
C
      IF(T.EQ.1.) GO TO 2100
         IF(TAPER.LT.0.5) ANSI=ANSD+(ANSD-ANS5)*TAPER/0.5
         IF(TAPER.GE.0.5) ANSI=ANS5+(ANS1-ANS5)*(TAPER-0.5)/0.5
      T=1.
      GO TO 2000
C
C  OUTBOARD SEGMENT
C
 2100 CONTINUE
         IF(TAPER.LT.0.5) ANSO=ANSD+(ANSD-ANS5)*TAPER/0.5
         IF(TAPER.GE.0.5) ANSO=ANS5+(ANS1-ANS5)*(TAPER-0.5)/0.5
C
C  CALCULATE EFFECTIVE CFOC, 2-D FLAP EFFECTIVENESS
C
      TOP=TAPER+(1.-TAPER)*(1.-(SSPAN(IEND)-SSPAN(1))/BOVER2)
      BOT=TAPER+(1.-TAPER)*(1.-(SSPAN(IBEGIN)-SSPAN(1))/BOVER2)
      FTAP=TOP/BOT
      CFOCE=(CFOC(IBEGIN)+FTAP*CFOC(IEND)) / (1.+FTAP)
      CALL FLAP2(CFOCE,TANPHE,ADEL)
      CLDPRM=(ANSO-ANSI)/BETA
      CLDEL=CLDPRM*ABS(ADEL)
C
C  YCP = CL-DELTA (ROLL) / CL-DELTA (LIFT)
C
      CLDL=0.5*CLD
      YCP=CLDEL/CLDL
      IF(YCP.GT.0.95) YCP=0.95
C
      RETURN
      END
