      SUBROUTINE ALPEQ(DOB,ALPBOD,AR,PHI,IFIN,DELT,MACH,DAQVW,
     1 LODN,RN,XW,RW,TAPER,ISURF,A14,A24,A34,A44,L12,L24,AEQ,
     2 KW,KKW,CDC,BLUNT,SWEEP0,AREA,ALO,KSET,DFLAG)
C
C SUBROUTINE TO COMPUTE THE EQUIVALENT ANGLE OF ATTACK OF
C A FIN PANEL WHICH IS PART OF A PLANAR OR CRUCIFORM SET OF FINS
C FOR ANY ARBITRARY ATTITUDE/ROLL ORIENTATION
C
C  MODIFIED BY W. BLAKE, WL/FIGC
C
C METHOD SUMMARIZED IN AIAA JOURNAL OF SPACECRAFT AND ROCKETS
C                      VOL. 20, PP. 356-362, JULY-AUG 1983
C
C *** INPUTS ***
C
C    DOB - BODY DIAMETER TO FIN SPAN RATIO (FIN SPAN INCLUDES BODY)
C ALPBOD - BODY TOTAL ANGLE OF ATTACK, DEG.
C     AR - EXPOSED ASPECT RATIO OF TWO FIN PANELS (SIDE-TO-SIDE AT ROOT)
C    PHI - FIN ROLL ANGLE
C   IFIN - FIN NUMBER BEING EVALUATED
C   DELT - DEFLECTION ANGLE FOR FINS 1 TO 4
C   MACH - FREESTREAM MACH NUMBER
C  DAQVW - DELTA ALPHA EQUIV DUE TO VORTICES
C   LODN - OGIVE NOSE FINENESS RATIO
C     RN - BODY RADIUS AT NOSE/CENTERBODY JUNCTURE
C     XW - DISTANCE FROM NOSE APEX TO FIN EXPOSED ROOT L.E.
C     RW - BODY RADIUS AT FIN
C  TAPER - FIN EXPOSED TAPER RATIO
C  ISURF - TYPE OF SURFACES: 1=PLANAR, 2=CRUCIFORM
C    ANN - PANEL DEFLECTION INFLUENCE COEFFICIENTS
C     KW - WING DUE TO BODY CARRY OVER INTERFERENCE FACTOR (ALPHA)
C    CDC - CROSS FLOW DRAG COEFFICIENT
C  BLUNT - BODY NOSE TIP RADIUS TO BODY RADIUS (BLUNTNESS RATIO)
C SWEEP0 - L.E. SWEEP ANGLE OF PANEL, DEGREES
C   AREA - SINGLE FIN PANEL PLANFORM AREA
C   KSET - INDEX OF FIN SET BEING EVALUATED
C  DFLAG - INDEX FOR ROTATION AXIS (0.=STATIC, 1.=PITCH,
C                                   2.=YAW,    3.=ROLL)
C
C *** OUTPUT ***
C
C    AEQ - EQUIVALENT ANGLE OF ATTACK, DEG.
C
C *** OTHER TERMS USED ***
C
C   KPHI - FIN-BODY INTERFERENCE FACTOR DUE TO BANK ANGLE
C DALPEQ - EQUIVALENT ANGLE OF ATTACK DUE TO BODY VORTICES, DEG.
C
      REAL KW,KPHI,MACH,LODN,L12,L24,LREF,LATREF
C
      COMMON /CONST/ PI,RAD,UNUSED,KAND
      COMMON /REFQN/  REFQ(9)
      COMMON /PAERO/  AKBW(4),AKWB(4),AKKBW(4),AKKWB(4),XCPBW(4),
     1                XCPWB(4),TEMP(4000)
C
      DIMENSION DELT(4), IIN(4), DELX(4)
      EQUIVALENCE (IIN(1),IIM1), (IIN(2),II), (IIN(3),IIP1),
     1            (IIN(4),IIP2)
C
      DIMENSION TDB(11),TKPHIC(11),TKPHIP(11)
      DATA TDB/0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0/
      DATA TKPHIC/0.382,0.447,0.490,0.508,0.502,0.471,0.417,0.342,
     1           0.244,0.127,0.000/
      DATA TKPHIP/0.637,0.687,0.681,0.649,0.597,0.529,0.447,0.352,
     1            0.246,0.128,0./
C
C***  PANEL CORRECTION FACTOR FOR SLENDER BODY THEORY CARRYOVER
C
      IF(DOB .NE. 0.)TANS=(TAN(SWEEP0/RAD)*RW*(1./DOB-1.))**2/(2.*AREA)
      IF(DOB .EQ. 0.)TANS=TAN(SWEEP0/RAD)
C      
C*** RETRIEVE GEOMETRY PARAMETERS FROM APPROPRIATE COMMON BLOCK
C
      LREF=REFQ(2)
      LATREF=REFQ(3)
      DO 500 I=1,4
         DELX(I)=XCPWB(I)*LREF
  500 CONTINUE
C
C***  CONVERT INPUT ANGLES TO RADIANS AND IDENTIFY ADJACENT PANELS
C
      IIN(1)=IFIN-1
      DO 1000 I=2,4
         IIN(I)=IIN(I-1)+1
 1000 CONTINUE
      DO 1010 I=1,4
        IF(IIN(I) .LE. 0) IIN(I)=IIN(I)+4
        IF(IIN(I) .GT. 4) IIN(I)=IIN(I)-4
 1010 CONTINUE
      DELI=(DELT(II)-DELT(IIP2))/2.0
      DELO=(DELT(IIM1)-DELT(IIP1))/2.0
      IF(ISURF .NE. 2) DELO=0.0
      ALPR=ALPBOD/RAD
C
C***  PANEL DIHEDRAL
C
      PHIR=PHI/RAD
      CALL DIHED(PHI,GAMMA)
C
C***  COMPUTE NOSE APEX HALF-ANGLE
C
      ROGIVE=LODN**2+.25
      CTHETA=SQRT(1.-LODN**2/ROGIVE**2)
      THETA=ACOS(CTHETA)*RAD
      BO2=RW/DOB
C
C***  COMPUTE THE FRACTION OF LIFT GENERATED BY BODY VORTICES, THIS IS
C***  DELTA-ALPHA-EQUIVALENT DUE TO VORTICES  (DALPEQ)
C
      IF(ALPBOD.LT.0.) GAMMA=-GAMMA
      CALL CLVR(MACH,CDC,BLUNT,RN,THETA,XW,RW,AR,BO2,TAPER,GAMMA,
     1   ABS(ALPBOD),DAQ)
      IF(ALPBOD.LT.0.) DAQ=-DAQ
      IF(PHI .LT. 0. .OR. PHI .GE. 180.)DAQ=-DAQ
      DAQ=DAQ+DAQVW/RAD
C
C***  SPLINE SLENDER BODY COEFFICIENT TABLES AND INTERPOLATE AT D/B
C
      IF(ISURF.EQ.2)CALL SPLINE(TDB,TKPHIC,11,2,2,0.,0.,DOB,1,KPHI,DUM)
      IF(ISURF.NE.2)CALL SPLINE(TDB,TKPHIP,11,2,2,0.,0.,DOB,1,KPHI,DUM)
C
C***   CALCULATE ALPHA EQUIVALENT FOR THE UNDEFLECTED PANEL
C
C  REV 3/99 MODIFIED TO GIVE CORRECT RESULT FOR AOA>90 OR AOA<-90
C  ABS(COS(ALPR)) ADDED
C
      TAEQ0=(KW*SIN(ALPR)*SIN(PHIR)-TANS*KPHI*((SIN(ALPR))**2)*
     1 SIN(PHIR)*COS(PHIR))/ABS(COS(ALPR)) + TAN(DAQ)
C
C  REV 1/96 UPDATE  -  DYNAMIC DERIVATIVES
C*** CALCULATE ALPHA EQUIVALENT FOR DYNAMIC DERIVATIVES 
C
      IF(DFLAG.GE.1.) THEN
         RATE=2.*0.02
C
C*** PITCH ROTATION
C
         IF(DFLAG.EQ.1.) THEN
         DXX=DELX(KSET)/LREF
         TAEQ0=(KW*(SIN(PHIR)*SIN(ALPR)+SIN(PHIR)*RATE*DXX)
     1     -TANS*KPHI*((SIN(ALPR))**2+RATE**2*DXX**2)
     2     *SIN(PHIR)*COS(PHIR))/ABS(COS(ALPR)) + TAN(DAQ)
C
C*** YAW ROTATION
C
         ELSEIF(DFLAG.EQ.2.) THEN
         DXX=DELX(KSET)/LATREF
         TAEQ0=(KW*(SIN(PHIR)*SIN(ALPR)-COS(PHIR)*RATE*DXX)
     1     -TANS*KPHI*((SIN(ALPR))**2+RATE**2*DXX**2)
     2     *SIN(PHIR)*COS(PHIR))/ABS(COS(ALPR)) + TAN(DAQ)
C
C*** ROLL ROTATION
C
         ELSEIF(DFLAG.EQ.3.) THEN
         CALL YCP(AR,TAPER,MACH,0.126,YCB)
         YC=YCB*(BO2-RW)+RW
         TAEQR=RATE*YC/LATREF
         TAEQ0=(KW*SIN(ALPR)*SIN(PHIR)-TANS*KPHI*((SIN(ALPR))**2)*
     1   SIN(PHIR)*COS(PHIR))/ABS(COS(ALPR)) + TAN(DAQ) + TAEQR
         ENDIF
      ENDIF
C
      AEQ0=ATAN(TAEQ0)*RAD
C 
      TOL=0.05
      IF(ABS(ABS(ALPBOD)-90.).LE.TOL.AND.ABS(PHI-180.).LE.TOL) AEQ0=0.
C
C***   PANEL DEFLECTION AND ADJACENT PANEL INTERFERENCE CORRECTION
C
C  REV 3/99 L12 AND L24 INCLUDED IN ADJACENT FIN DEFLECTION TERMS
C
      AEQD=L12*A14*DELT(IIM1)+L24*A24*DELT(IIP2)+L12*A34*DELT(IIP1)
     1   +A44*DELT(II)
C
C***   FINAL EQUIVALENT ANGLE OF ATTACK
C
      AEQ=AEQ0+AEQD
C
C***  CORRECT AEQ ZERO LIFT ANGLE OF ATTACK FOR SUBSONIC MACH #'S
C     AND FIN POSTION AROUND THE BODY, REVERSING THE SENSE OF ALO
C     FOR PHI'S IN THE 3RD AND 4TH QUADRANTS
C
      IF (MACH .GT. 0.8) GO TO 1030
      IF (PHI .LT. 0.0  .OR.  PHI .GT. 180.0) GO TO 1020
       AEQ=AEQ-ALO
      GO TO 1030
C
 1020 CONTINUE
       AEQ=AEQ+ALO
C
 1030 CONTINUE
C
      RETURN
      END
