      SUBROUTINE FCNASP (M,TAPER,AR,SWAVLE,DEL,CNAW)
C
C **
C **  COMPUTES SUPERSONIC LIFTING SURFACE CN/ALPHA.
C **  REFERENCE DATCOM FIGURES 4.1.3.2-56 AND 4.1.3.2-60
C **             AND DOUGLAS REPORT DAC SM-13110
C **  LIMITATIONS -
C **          MACH MUST FALL IN THE INTERVAL 1.2-5.0 INCLUSIVE.
C **          TAPER MUST FALL IN THE INTERVAL 0.-1.0 INCLUSIVE.
C **          NO FORWARD SWEPT FINS.
C **          DEL MUST FALL IN THE INTERVAL 0.-70. INCLUSIVE.
C **  NOTE -    INPUT UNITS ON SREF AND SWING MUST BE CONSISTENT.
C **  INPUT -
C **          M = MACH NO.
C **          TAPER = EXPOSED TAPER RATIO
C **          AR = EXPOSED FIN ASPECT RATIO OF TWO PANELS
C **          SWAVLE = FIN LEADING EDGE SWEEP ANGLE,DEGREES
C **          DEL = LEADING EDGE SEMI WEDGE ANGLE, DEG
C **  OUTPUT -
C **          CNAW = NORMAL FORCE CURVE SLOPE REFERENCED TO
C **                 EXPOSED FIN AREA, PER DEGREE
C
C  MODIFIED BY K. DETERS, MCDONNELL DOUGLAS
C
      COMMON /CONST/PI,RAD,UNUSED,KAND
C
      DIMENSION G(6),Z(8),TX(6),A(288),B(288)
      DIMENSION D(16),E(9),IROUT(2),CT(2),ST(2)
      DIMENSION ALT0(48),ALT2(48),ALT25(48),ALT333(48),
     1          ALT5(48),ALT10(48)
      DIMENSION BRT0(48),BRT2(48),BRT25(48),BRT333(48),
     1          BRT5(48),BRT10(48)
C
      REAL M
C
      EQUIVALENCE ( A(  1) ,  ALT0 (1)) , ( A( 49) , ALT2  (1)) ,
     1            ( A( 97) ,  ALT25(1)) , ( A(145) , ALT333(1)) ,
     2            ( A(193) ,  ALT5 (1)) , ( A(241) , ALT10 (1))
      EQUIVALENCE ( B(  1) ,  BRT0 (1)) , ( B( 49) , BRT2  (1)) ,
     1            ( B( 97) ,  BRT25(1)) , ( B(145) , BRT333(1)) ,
     2            ( B(193) ,  BRT5 (1)) , ( B(241) , BRT10 (1))
C
C **  EXPLANATION OF DATA -
C **          G = VALUE OF EACH GRAPH, (TAPER RATIO).
C **          Z = VALUE OF EACH CURVE, (AR*TLES).
C **          TX = X VALUE OF EACH DATA POINT, (BETA/TLES OR TLES/BETA).
C
      DATA KG,KC,KP,KD/6,8,6,288/
      DATA G/.0,.2,.25,.3333,.5,1./
      DATA Z/.25,.5,1.,2.,3.,4.,5.,6./
      DATA TX/.0,.2,.4,.6,.8,1./
C
C **  DATA TABLE FOR LEFT SIDE OF DATCOM FIG.4.1.3.2-56
C **  LEFT SIDE OF THE GRAPH TAPER = 0.  ROWS CORRESPOND TO VALUES
C **  OF Z.  COLUMNS CORRESPOND TO VALUES OF TX.
C
      DATA ALT0/
     1   .392,  .392,  .392,  .400,  .408,  .431
     2,  .784,  .784,  .784,  .800,  .831,  .863
     3, 1.569, 1.569, 1.584, 1.631, 1.678, 1.765
     4, 3.137, 3.145, 3.153, 3.176, 3.255, 3.373
     5, 4.706, 4.784, 5.020, 4.525, 4.078, 3.686
     6, 6.196, 5.922, 5.412, 4.878, 4.408, 4.000
     7, 6.353, 6.353, 5.741, 5.122, 4.667, 4.235
     8, 6.353, 6.353, 6.094, 5.506, 4.957, 4.471/
C **  LEFT SIDE OF THE GRAPH TAPER = .2.
      DATA ALT2/
     1   .392,  .392,  .392,  .400,  .408,  .431
     2,  .784,  .784,  .784,  .800,  .831,  .863
     3, 1.569, 1.608, 1.647, 1.671, 1.725, 1.804
     4, 3.137, 3.216, 3.592, 3.624, 3.467, 3.255
     5, 4.706, 5.082, 4.824, 4.431, 3.992, 3.686
     6, 5.569, 5.584, 5.412, 4.902, 4.455, 4.039
     7, 5.710, 5.741, 5.718, 5.310, 4.808, 4.353
     8, 5.796, 5.882, 5.882, 5.647, 5.114, 4.643/
C **  LEFT SIDE OF THE GRAPH TAPER = .25.
      DATA ALT25/
     1   .392,  .392,  .392,  .400,  .408,  .431
     2,  .784,  .784,  .800,  .831,  .847,  .871
     3, 1.569, 1.608, 1.624, 1.655, 1.733, 1.835
     4, 3.137, 3.412, 3.702, 3.631, 3.443, 3.184
     5, 4.706, 5.027, 4.792, 4.392, 4.008, 3.686
     6, 5.373, 5.420, 5.325, 4.871, 4.431, 4.627
     7, 5.576, 5.631, 5.576, 5.286, 4.784, 4.392
     8, 5.725, 5.733, 5.725, 5.631, 5.114, 4.118/
C **  LEFT SIDE OF THE GRAPH TAPER = .333.
      DATA ALT333/
     1   .392,  .392,  .392,  .400,  .408,  .431
     2,  .784,  .784,  .808,  .839,  .855,  .878
     3, 1.569, 1.569, 1.584, 1.647, 1.733, 1.922
     4, 3.137, 3.294, 3.710, 3.655, 3.427, 3.161
     5, 4.706, 4.784, 4.690, 4.353, 3.914, 3.624
     6, 5.176, 5.176, 5.137, 4.839, 4.400, 4.000
     7, 5.404, 5.412, 5.373, 5.255, 4.784, 4.329
     8, 5.529, 5.569, 5.561, 5.514, 5.114, 4.627/
C **  LEFT SIDE OF THE GRAPH TAPER = .5.
      DATA ALT5/
     1   .392,  .392,  .392,  .400,  .408,  .431
     2,  .784,  .784,  .863,  .824,  .863,  .918
     3, 1.569, 1.624, 1.765, 1.835, 1.898, 1.953
     4, 3.137, 3.373, 3.647, 3.459, 3.255, 3.027
     5, 4.392, 4.384, 4.369, 4.220, 3.867, 3.545
     6, 4.863, 4.784, 4.745, 4.643, 4.314, 3.937
     7, 5.035, 5.035, 5.020, 4.957, 4.722, 4.314
     8, 5.161, 5.176, 5.192, 5.176, 5.090, 4.471/
C **  LEFT SIDE OF THE GRAPH TAPER = 1.0.
      DATA ALT10/
     1   .392,  .400,  .439,  .486,  .557,  .627
     2,  .784,  .824,  .863,  .933, 1.043, 1.200
     3, 1.569, 1.584, 1.631, 1.725, 1.843, 2.000
     4, 3.137, 3.051, 3.035, 2.980, 2.910, 2.894
     5, 3.725, 3.788, 3.686, 3.537, 3.443, 3.373
     6, 4.078, 4.141, 4.016, 3.922, 3.843, 3.827
     7, 4.314, 4.408, 4.275, 4.196, 4.220, 4.235
     8, 4.549, 4.604, 4.525, 4.431, 4.510, 4.565/
C
C **  DATA TABLE FOR RIGHT SIDE OF DATCOM FIG. 4.1.3.2-56
C **  RIGHT SIDE OF THE GRAPH TAPER = 0.
C
      DATA BRT0/
     1  4.000, 1.859, 1.020,  .690,  .533,  .431
     2, 4.000, 3.239, 1.922, 1.349, 1.035,  .863
     3, 4.000, 3.843, 3.333, 2.604, 2.094, 1.765
     4, 4.000, 3.922, 3.804, 3.671, 3.522, 3.373
     5, 4.000, 3.922, 3.882, 3.843, 3.765, 3.686
     6, 4.000, 4.000, 4.000, 4.000, 4.000, 4.000
     7, 4.000, 4.008, 4.039, 4.078, 4.149, 4.235
     8, 4.000, 4.016, 4.078, 4.157, 4.314, 4.471/
C **  RIGHT SIDE OF THE GRAPH TAPER = .2.
      DATA BRT2/
     1  4.000, 2.157, 1.082,  .690,  .510,  .431
     2, 4.000, 3.506, 2.235, 1.490, 1.106,  .863
     3, 4.000, 3.820, 3.506, 2.902, 2.251, 1.804
     4, 4.000, 3.922, 3.843, 3.702, 3.506, 3.255
     5, 4.000, 4.000, 4.000, 4.000, 3.898, 3.686
     6, 4.000, 4.024, 4.314, 4.102, 4.157, 4.039
     7, 4.000, 4.047, 4.078, 4.220, 4.384, 4.353
     8, 4.000, 4.016, 4.118, 4.314, 4.557, 4.643/
C **  RIGHT SIDE OF THE GRAPH TAPER = .25.
      DATA BRT25/
     1  4.000, 2.282, 1.137,  .706,  .525,  .431
     2, 4.000, 3.490, 2.275, 1.490, 1.082,  .871
     3, 4.000, 3.788, 3.467, 2.949, 2.290, 1.835
     4, 4.000, 3.922, 3.835, 3.686, 3.451, 3.184
     5, 4.000, 4.000, 4.000, 4.000, 3.859, 3.686
     6, 4.000, 4.008, 4.078, 4.118, 4.157, 4.627
     7, 4.000, 4.016, 4.102, 4.235, 4.416, 4.392
     8, 4.000, 4.024, 4.118, 4.314, 4.588, 4.118/
      DATA BRT333/
C **  RIGHT SIDE OF THE GRAPH TAPER = .333.
     1  4.000, 2.353, 1.106,  .714,  .525,  .431
     2, 4.000, 3.435, 2.298, 1.498, 1.106,  .878
     3, 4.000, 3.773, 3.412, 2.965, 2.447, 1.922
     4, 4.000, 3.922, 3.835, 3.678, 3.435, 3.161
     5, 4.000, 4.000, 4.000, 4.000, 3.851, 3.624
     6, 4.000, 4.008, 4.071, 4.157, 4.173, 4.000
     7, 4.000, 4.016, 4.118, 4.275, 4.431, 4.329
     8, 4.000, 4.024, 4.157, 3.584, 4.627, 4.627/
C **  RIGHT SIDE OF THE GRAPH TAPER = .5.
      DATA BRT5/
     1  4.000, 2.447, 1.216,  .784,  .533,  .431
     2, 4.000, 3.388, 2.447, 1.710, 1.176,  .918
     3, 4.000, 3.741, 3.365, 2.878, 2.376, 1.953
     4, 4.000, 3.882, 3.788, 3.647, 3.318, 3.027
     5, 4.000, 3.961, 4.000, 4.000, 3.804, 3.545
     6, 4.000, 3.984, 4.024, 4.149, 4.165, 3.937
     7, 4.000, 4.016, 4.078, 4.282, 4.455, 4.314
     8, 4.000, 4.039, 4.141, 4.392, 4.643, 4.471/
C **  RIGHT SIDE OF THE GRAPH TAPER = 1.0.
      DATA BRT10/
     1  4.000, 2.431, 1.263,  .902,  .745,  .627
     2, 4.000, 3.176, 2.337, 1.749, 1.435, 1.200
     3,4.000, 3.647, 3.208, 2.722, 2.314, 2.000
     4,4.000, 3.843, 3.741, 3.506, 3.161, 2.894
     5,4.000, 3.922, 3.961, 3.937, 3.702, 3.373
     6,4.000, 3.953, 4.039, 4.173, 4.094, 3.827
     7,4.000, 3.984, 4.078, 4.314, 4.290, 4.235
     8,4.000, 4.024, 4.118, 4.463, 4.706, 4.565/
C
C **  CURVE FIT FOR LEFT SIDE OF DATCOM FIG. 4.1.3.2-60
C
      DATA D/1.56127, -1.07271, 0.0,.449111,-4.13978E-3,
     1      0.0,-7.32931E-3,0.0,0.0,1.34777E-4,0.0,0.0,
     2      -4.57070E-7,0.0,0.0,0.0/
C
C **  CURVE FIT FOR RIGHT SIDE OF DATCOM FIG. 4.1.3.2-60
C
      DATA E/1.33824,-.673748,.243197,-1.02618E-2,
     1       1.43125E-3,0.0,7.46318E-5,0.0,0.0/
C
      BETA=SQRT(ABS(1.-M*M))
C
C
C***  LUCERO'S METHOD LIMITATIONS
      SUBLE=M*COS(SWAVLE/RAD)
      XSWP=0.
      XRECT=0.
      IF(TAPER.GT.0.5 .OR.  SWAVLE.EQ.0.) XRECT=BETA*AR
      IF(TAPER.LE.0.5 .AND. SWAVLE.NE.0.) XSWP=BETA/TAN(SWAVLE/RAD)
C
      IF(M .GE. 2.5 .AND. XRECT .LE. 1.35 .AND. XSWP .LE. 1.35 
     &   .AND. SWAVLE .GE. 0. .AND. SUBLE .GT. 1.)THEN
C
C***    SUBROUTINE CALL STATEMENTS 
        IF(SWAVLE .EQ. 0.)THEN
        XTERM=1./(BETA*AR)
        CALL LUCERO(BETA,XTERM,CNAW)
        ELSE IF(TAPER .EQ. 0.)THEN
        XTERM=TAN(SWAVLE/RAD)/BETA
        CALL LUCERO(BETA,XTERM,CNAW)
        ELSE
        XTERM1=1./(BETA*AR)
        CALL LUCERO(BETA,XTERM1,CNA1)
        XTERM2=TAN(SWAVLE/RAD)/BETA
        CALL LUCERO(BETA,XTERM2,CNA2)
        CNAW=CNA1*TAPER+CNA2*(1.-TAPER)
        ENDIF
      ELSE
C
      IF(ABS(SWAVLE) .LE. UNUSED .AND. TAPER .EQ. 1.)GO TO 1040
C
      TLES=TAN(SWAVLE/RAD)
      R=AR*TLES
      IF(R .LT. 0.25)R=.25
C
C **  CNAW = THEORETICAL CN/ALPHA PER DATCOM FIG. 4.1.3.2-56
C
      IF(BETA.GE.TLES) GO TO 1000
      Q=BETA/TLES
      CALL MVLOOK(KG,KC,KP,KD,G,Z,TX,A,Q,R,TAPER,CNAW)
      CNAW=CNAW/TLES
      GO TO 1010
 1000 Q=TLES/BETA
      CALL MVLOOK(KG,KC,KP,KD,G,Z,TX,B,Q,R,TAPER,CNAW)
      CNAW=CNAW/BETA
 1010 IF(BETA.GE.TLES) GO TO 1020
C
C **  CORR = CORRECTION FOR SONIC L.E. REGION DATCOM FIG. 4.1.3.2-60
C
      CORR=D(1)+D(2)*Q+D(3)*Q**2+D(4)*Q**3+
     1     DEL*(D(5)+D(6)*Q+D(7)*Q**2+D(8)*Q**3)+
     2     DEL**2*(D(9)+D(10)*Q+D(11)*Q**2+D(12)*Q**3)+
     3     DEL**3*(D(13)+D(14)*Q+D(15)*Q**2+D(16)*Q**3)
      GO TO 1030
 1020 CORR=E(1)+E(2)*Q+E(3)*Q**2+
     1     DEL*(E(4)+E(5)*Q+E(6)*Q**2)+
     2     DEL**2*(E(7)+E(8)*Q+E(9)*Q**2)
 1030 IF(CORR.GT.1.) CORR=1.
      CNAW=CNAW*CORR/RAD
C
      IF(R .GT. 0.25)GO TO 1050
      CNAWW=CNAW
C
C **  LEADING EDGE SWEEP = 0 (DATCOM FIG. 4.1.3.2-56G)
C
 1040 AB=AR*BETA
C ...
C ... USING EQUATIONS FROM DAC SM-13110 FOR RECTANGULAR WINGS
C ...
      IF(AB.GE..5 .AND. AB.LT.1.)BCNA=4./(PI*AB)*((2.*AB-1.)*ASIN(AB)+
     1 AB*(AB-2.)*ALOG((1.+SQRT(1.-AB**2))/AB)+(1.+AB)*SQRT(1.-AB**2))
      IF(AB .LT. .5)BCNA=0.7928
      IF(AB .GE. 1.)BCNA=4.*(1.-.5/AB)
      CNAW=BCNA/BETA/RAD
      IF(ABS(SWAVLE) .LE. UNUSED .AND. TAPER .EQ. 1.)GO TO 1050
      IF(R .GT. 0.25)GO TO 1050
C
C ... LINEARILY INTERPOLATE BETWEEN AR*TAN(LE) AND RECTANGULAR
C
      ST(1)=0.
      ST(2)=ATAN(R/AR)
      CT(1)=CNAWW
      CT(2)=CNAW
      CALL LNTRP(ST,CT,2,AR*TAN(SWAVLE/RAD),CNAW)
C
 1050 CONTINUE
C
      ENDIF
C
      RETURN
      END
