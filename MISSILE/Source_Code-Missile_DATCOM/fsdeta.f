      SUBROUTINE FSDETA(MACH, DELN, SLE, ALPD)
C ***
C *** CALCULATES SHOCK DETACHMENT ANGLE-OF-ATTACK ON FINS
C ***
C *** REFERENCE   DATCOM SECTION 4.1.3.3
C ***                    PAGE 4.1.3.3-33  STEP 2B
C ***                    ALSO USES NACA 1135 EQUATIONS 168 AND 138
C ***                    AS PROGRAMMED IN ANGDET
C ***
C *** INPUT   MACH    MACH NUMBER
C ***         DELN    SLOPE AT AIRFOIL LEADING EDGE(.15% TO 6% CHORD),DE
C ***         SLE     LEADING EDGE SWEEP ANGLE, DEG
C ***
C *** OUTPUT  ALPD    ANGLE-OF-ATTACK FOR SHOCK DETACHMENT, DEG
C ***
C ***
      REAL MACH
C ***
C *** CONSTANT DATA
C ***
      COMMON /CONST/PI,RAD,UNUSED,KAND
      DATA EPS / 0.00001 /
C ***
C *** LOOP 10 TIMES OR UNTIL ABS(DALP) .LT. EPS
C ***
      DELR = DELN/RAD
      SSLE = SIN(SLE/RAD)
      CSLE = COS(SLE/RAD)
      ALPD = 10.0/RAD
      DO 10 I=1,10
            XMN = MACH*SQRT(1.0-SSLE*SSLE*(COS(ALPD)**2))
            CALL ANGDET(XMN, DELD)
            ALPN = DELR + ATAN(TAN(ALPD)/CSLE)
            DALP = DELD - ALPN
         IF(ABS(DALP) .LT. EPS) GO TO 20
            XMN = MACH*SQRT(1.0-SSLE*SSLE*(COS(ALPD+0.01)**2))
            CALL ANGDET(XMN, DELD2)
            DDELD = (DELD2-DELD)/0.01
            DALPN = 1.0/(CSLE*(COS(ALPD)**2)+(SIN(ALPD)**2)/CSLE)
            DALPD = DALP/(DALPN-DDELD)
            ALPD  = ALPD + DALPD
   10 CONTINUE
C ***
   20 ALPD = ALPD*RAD
      RETURN
      END
