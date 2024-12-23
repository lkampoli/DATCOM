      SUBROUTINE FCLMSH(DELY, SLE, MACH, SCLM, CLA, CLM, ACLM)
C ***
C *** COMPUTES SUBSONIC MAXIMUM LIFT AND ANGLE-OF-ATTACK
C *** FOR MAXIMUM LIFT FOR HIGH ASPECT RATIO WINGS
C ***
C *** REFERENCE   DATCOM SECTION 4.1.3.4
C ***             METHOD 2
C ***
C *** LIMITATIONS   MACH .LE. 0.6
C ***               AR .GE. 3.0/((C1+1)*COS(SLE))
C ***
C *** INPUT   DELY    DIFFERENCE IN AIRFOIL ORDINATE BETWEEN
C ***                 .15% AND 6% CHORD,PER CENT CHORD
C ***         SLE     LEADING EDGE SWEEP ANGLE, DEG
C ***         MACH    MACH NUMBER
C ***         SCLM    SECTION MAXIMUM LIFT COEFFICIENT
C ***         CLA     WING LIFT CURVE SLOPE
C ***
C *** OUTPUT  CLM     MAXIMUM LIFT COEFFICIENT,REFERENCED TO EXPOSED
C ***                 PLANFORM AREA OF TWO PANELS
C ***         ACLM    ANGLE-OF-ATTACK FOR CLM,DEGREES
C ***
C ***
C
      COMMON /CONST/PI,RAD,UNUSED,KAND
      DIMENSION SALE(13), DELTAY(7), CLL(91)
      DIMENSION AMACH(5), DYA(6), SALE4(4), DCAR(120)
      DIMENSION DY(4), DACLL(52)
      REAL MACH
C
C ***
C *** DATA STATEMENTS
C ***
      DATA DUM / 1.0 /
C
C     ----4.1.3.4-22
C
      DATA AMACH/.2,.3,.4,.5,.6/
      DATA DYA  /2.,2.25,2.5,3.,4.,4.5/
      DATA SALE4/0.,20.,40.,60./
      DATA DCAR/0.,2*-.02,2*0., 0.,-.13,-.19,-.2,-.19 , 0.,-.185,-.32,
     1-.4,-.445 , 0.,-.21,-.36,-.45,-.5 , 0.,-.24,-.42,-.545,-.64 ,
     2-0.,-.24,-.455,-.605,-.72 , 0.,-.04,-.045,-.02,0. , 0.,-.095,-.165
     3,-.22,-.25 , 0.,-.105,-.19,-.26,-.29 , 0.,-.125,-.225,-.3,-.345 ,
     4 0.,-.15,-.265,-.37,-.45 , 0.,-.15,-.29,-.41,-.51 , 0.,-.04,-.07,
     5-.095,-.1 , 0.,-.043,-.080,-.108,-.123 , 0.,-.045,-.085,-.12,-.145
     6 , 0.,-.05,-.095,-.14,-.19 , 0.,-.07,-.125,-.19,-.26 , 0.,-.07,
     7-.14,-.215,-.3,4*0.,-.02,4*0.,-.022,4*0.,-.03,3*0.,-.02,-.07,2*0.,
     8-.02,-.055,-.085,0.,-.04,-.085,-.14,-.2/
C
C     ----4.1.3.4-21 A
C
      DATA SALE/0.,5.,10.,15.,20.,25.,30.,35.,40.,45.,50.,55.,60./
      DATA DELTAY/1.4,1.6,1.8,2.0,2.2,2.4,2.5/
      DATA CLL /.9,.915,.93,.95,.975,1.,1.03,1.07,1.1,1.15,1.19,1.25,1.3
     1 , .9,.91,.92,.94,.96,.98,1.,1.02,1.05,1.08,1.11,1.15,1.19 ,   .9,
     2 .91,.92,.93,.94,.95,.96,.965,.975,.99,1.,1.01,1.03 , 6*.9, 2*.89,
     3.885,2*.88,.875,.87 , .9,.895,.89,.88,.87,.86,.85,.835,.82,.8,.78,
     4.755,.73 , .9,.89,.88,.87,.85,.83,.81,.79,.76,.725,.695,.65,.59 ,
     5 .9,.89,.87,.86,.84,.82,.795,.77,.73,.7,.65,.59,.52 /
C
C     ----4.1.3.4-21 B
C
      DATA DY/1.2,2.,3.,4./
      DATA DACLL/1.75,1.9,2.2,2.7,3.4,4.15,5.1,6.1,7.3,8.7,10.15,11.75,
     113.3 , .1,.5,1.05,1.65,2.3,3.1,3.9,4.7,5.7,6.7,7.7,8.75,9.8,
     21.2,1.4,1.7,2.,2.4,2.85,3.35,3.7,4.25,4.7,5.3,5.9,6.65,
     32.2,2.1,2*2.,2.1,2.15,2.3,2.4,2.55,2.7,2.9,3.05,3.3/
C
C  LOOK UP RCLM, DACLM, DCLM
C
      XDELY = DELY
      IF(XDELY .LT. 1.4) XDELY = 1.4
      IF(XDELY .GT. 2.5) XDELY = 2.5
      CALL MVLOOK(1, 7, 13, 91, DUM, DELTAY, SALE, CLL, SLE, XDELY,
     1            1.0, RCLM)
      XDELY = DELY
      IF(XDELY .LT. 1.2) XDELY = 1.2
      CALL MVLOOK(1, 4, 13, 52, DUM, DY, SALE, DACLL, SLE, XDELY,
     1            1.0, DACLM)
C
C  REV 3/99 CORRECTION TO DELTA-CLMAX FOR THIN AIRFOILS
C  PREVENTS BAD VALUE FROM TABLE LOOK-UP
C
      IF(XDELY .LT. 2.0) XDELY = 2.0
      CALL MVLOOK(4, 6, 5, 120, SALE4, DYA, AMACH, DCAR, MACH, XDELY,
     1            SLE, DCLM)
C
      CLM  = RCLM*SCLM + DCLM
      ACLM = CLM/CLA + DACLM
C
      RETURN
      END
