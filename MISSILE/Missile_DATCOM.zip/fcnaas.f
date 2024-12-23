      SUBROUTINE FCNAAS(MACH, AR, DELY, SLE, TAPER, ALPHA, CNA, SCLM,
     1                  CNAA)
C ***
C *** COMPUTES SUBSONIC NON-LINEAR NORMAL FORCE PER SINE(ALPHA)**2
C ***
C *** REFERENCE   DATCOM SECTION 4.1.3.3
C ***
C *** LIMITATIONS   ALPHA .LT. 90 DEG
C ***               STRAIGHT TAPERED FINS
C ***               NO CAMBER
C ***
C *** INPUT   MACH    MACH NUMBER
C ***         AR      EXPOSED FIN ASPECT RATIO OF TWO PANELS
C ***         DELY    DIFFERENCE IN AIRFOIL ORDINATE BETWEEN
C ***                 .15% AND 6% CHORD,PER CENT CHORD LENGTH
C ***         SLE     LEADING EDGE SWEEP ANGLE, DEG
C ***         TAPER   EXPOSED FIN TAPER RATIO
C ***         ALPHA   ANGLE-OF-ATTACK, DEG
C ***         CNA     LINEAR CN, CN PER ALPHA AT ALPHA = 0, 1/DEG
C ***         SCLM    AIRFOIL SECTION MAX LIFT
C ***
C *** OUTPUT  CNAA    CN PER SIN(ALPHA)**2,REFERENCED TO EXPOSED
C ***                 PLANFORM AREA OF TWO PANELS
C ***
C ***
C
      COMMON /CONST/PI,RAD,UNUSED,KAND
C
      DIMENSION XDCNAA(7), YDCNAA(12), ZDCNAA(84)
      DIMENSION XTAB(11), YD(11), YCNL(11), YCNR(11)
C
      REAL MACH, J
C
C ***
C *** DATA STATEMENTS
C ***
      DATA DUM / 1.0 /
C ***
C *** DATCOM FIGURE 4.1.3.3-55A   (DCNAA)
C ***
      DATA XDCNAA /   .20,  .40,  .46,  .50,  .55,  .60, 1.00  /
      DATA YDCNAA / -4.00,-2.00,-1.00, 0.00,  .15,  .25,  .50, 1.00,
     *               1.50, 2.00, 2.50, 3.00 /
      DATA ZDCNAA /
     *     -.9000, -.6760, -.6070, -.5640, -.5060, -.4520, 0.0000,
     *     -.4000, -.3000, -.2700, -.2500, -.2250, -.2000, 0.0000,
     *     -.1000, -.0750, -.0650, -.0625, -.0562, -.0500, 0.0000,
     *      .2500,  .2500,  .2250,  .2083,  .1874,  .1666, 0.0000,
     *      .6000,  .6000,  .5400,  .5000,  .4500,  .4000, 0.0000,
     *      .8800,  .7800,  .7350,  .7000,  .6500,  .6000, 0.0000,
     *     1.1000, 1.1000,  .9900,  .9170,  .8250,  .7340, 0.0000,
     *     1.6000, 1.6000, 1.4400, 1.3340, 1.2000, 1.0680, 0.0000,
     *     2.0000, 2.0000, 2.0000, 1.8520, 1.6660, 1.4800, 0.0000,
     *     2.3500, 2.3500, 2.3500, 2.3500, 2.1150, 1.8800, 0.0000,
     *     2.6500, 2.6500, 2.6500, 2.6500, 2.6500, 2.3600, 0.0000,
     *     2.8500, 2.8500, 2.8500, 2.8500, 2.8500, 2.8500, 0.0000 /
C ***
C *** DATCOM FIGURE 4.1.3.3-55A  (D)
C ***
      DATA XTAB /  0.00,  .10,  .20,  .30,  .40,  .50,  .60,
     *              .70,  .80,  .90, 1.00 /
      DATA YD   /  0.00, -.35, -.70,-1.00,-1.27,-1.46,-1.55,
     *            -1.40,-1.00, -.21, 0.00 /
C ***
C *** DATCOM FIGURE 4.1.3.3-55B  (CNAAPI)
C ***        (USE XTAB FOR ORDINATE)
C ***
      DATA YCNL /  2.00, 1.32, 1.20, 1.20, 1.20, 1.20, 1.20,
     *             1.20, 1.20, 1.20, 1.20 /
      DATA YCNR /  2.00, 1.40, 1.26, 1.21, 1.20, 1.20, 1.20,
     *             1.20, 1.20, 1.20, 1.20 /
C
C ***
C *** CALCULATE C1, C2, CLM, ACLM, BETA, AND CNP (CN AT CLM)
C ***
      CALL FCLMSB(AR, MACH, DELY, SLE, TAPER, C1, C2, CLML, ACLML)
      CALL FCLMSH(DELY, SLE, MACH, SCLM, CNA, CLMH, ACLMH)
      AR1   = 3.0/((C1+1.0)*COS(SLE/RAD))
      AR2   = 4.0/((C1+1.0)*COS(SLE/RAD))
      IF(AR .GT. AR1) GO TO 2
        CLM  = CLML
        ACLM = ACLML
        GO TO 6
   2  IF(AR .LT. AR2) GO TO 4
        CLM  = CLMH
        ACLM = ACLMH
        GO TO 6
   4  CONTINUE
        CLM  = CLML + (CLMH-CLML)*(AR-AR1)/(AR2-AR1)
        ACLM = ACLML + (ACLMH-ACLML)*(AR-AR1)/(AR2-AR1)
   6  CONTINUE
      ALP   = ABS(ALPHA)
      CNP   = CLM/COS(ACLM/RAD)
      CNAAR = (CNP-0.5*CNA*RAD*SIN(2.0*ACLM/RAD))/(SIN(ACLM/RAD)**2)
      BETA  = SQRT(1.0-MACH*MACH)
C ***
C *** ANGLE-OF-ATTACK BELOW STALL - ALP .LE. ACLM
C ***
      IF(ALP .GT. ACLM) GO TO 10
        J = 0.3*(C1+1.0)*(AR/BETA)*COS(SLE/RAD)*
     *      ((C1+1.0)*(C2+1.0)-((C2+1.0)*AR*TAN(SLE/RAD)/7.0)**3)
        IF(J .GT. 3.0) J = 3.0
        TRAT = TAN(ALP/RAD)/TAN(ACLM/RAD)
        IF(TRAT .LT. 0.20) TRAT = 0.20
        CALL MVLOOK(1, 12, 7, 84, DUM, YDCNAA, XDCNAA, ZDCNAA,
     *              TRAT, J, 1.0, DCNAA)
        CNAA = CNAAR + DCNAA
   10 CONTINUE
C ***
C *** ANGLE-OF-ATTACK ABOVE STALL - ALP .GT. ACLM
C ***
      IF(ALP .LE. ACLM) GO TO 20
        TRAT = TAN(ACLM/RAD)/TAN(ALP/RAD)
        CALL LNTRP(XTAB, YD, 11, TRAT, D)
        XARG = AR
        IF(AR .GT. 1.0) XARG = 1.0/AR
        IF(AR .LE. 1.0) CALL LNTRP(XTAB, YCNL, 11, XARG, CNAAPI)
        IF(AR .GT. 1.0) CALL LNTRP(XTAB, YCNR, 11, XARG, CNAAPI)
        CNAA = CNAAR+(CNAAPI-CNAAR)*(1.0-TRAT)
     *         +BETA*BETA*D*CNA*RAD*(CLM/CLML)**2/2.3
   20 CONTINUE
C ***
C
      RETURN
      END
