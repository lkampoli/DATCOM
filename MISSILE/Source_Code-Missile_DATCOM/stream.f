      SUBROUTINE STREAM(MACH,RPUL,DEL,XB,H,W,L,CDSTR)
      REAL MACH,L,MSUB,MSUP
C
C  WRITTEN BY K. DETERS, MCDONNELL DOUGLAS
C
      MSUB=.6
      MSUP=1.
      G=1.4
      S=H*W
      TMACH=0.0
      IF(MACH.GT.MSUB) GO TO 315
C
C*** SUBSONIC DRAG CALCULATION
 310  FR=L/H
        IF (FR .LT. 20.0) THEN
        CD0=.1349-.02761*FR+.0026596*(FR**2)-1.1082E-4*(FR**3)
     &  +1.7526E-6*(FR**4)
        ELSE
        CD0=.002*FR
        ENDIF
      DELXB=.015
      HX1=.08
      HX2=.3
      CD1=CD0*(1.0-.25*(DELXB/HX1))
      CD2=.037
      CDCH1=CD0*(1.-.25*(DELXB/HX1))
      CDCH2=CD0*(1.-.25*(DELXB/HX2))
      HX=H/XB
      IF (HX .LT. HX1) THEN
        HDEL=HX/DELXB
        IF (HDEL .LE. 1.0) THEN
        CD=.75*CD0*(HDEL**(1./3.))      
        ELSE
        DELH=DELXB/HX
        CD=CD0*(1.-.25*DELH)
        ENDIF
      ELSE IF (HX .GT. HX2) THEN
        IF (CDCH1 .LT. CD2 .AND. CDCH2 .LT. CD2) THEN 
        DELH=DELXB/HX
        CD=CD0*(1.-.25*DELH)
        ELSE
        CD=-7.2165E-5*HX+CD2
        ENDIF
      ELSE
        IF (CDCH1 .LT. CD2) THEN 
          IF (CDCH2 .LT. CD2) THEN
          DELH=DELXB/HX
          CD=CD0*(1.-.25*DELH)
          ELSE
          SLOPE=(CD2-CDCH1)/(HX2-HX1)
          YINT=CDCH1-(SLOPE*HX1)
          CD=SLOPE*HX+YINT
          ENDIF
        ELSE
C
C***  CUBIC SPLINE INTERPOLATION
        DY1=0.0
        DY2=0.0
        CALL CUBIC(HX,HX1,HX2,CD1,CD2,DY1,DY2,CDY)
        CD=CDY
C
        ENDIF
      ENDIF
      CD=CD/(SQRT(1.0-MACH**2))
      IF(TMACH.EQ.0.) GO TO 345
      GO TO 320
 315  IF(MACH.LT.MSUP) GO TO 325
C
C*** SUPERSONIC DRAG CALCULATION
C
C***  NUMBER OF INCREMENTS IN X AND Y DIRECTIONS
 330  NX=100
      NY=100     
C
C***  CONSTANTS
      PI=3.141593
C
C***  INITIALIZE SUMMING VARIABLES
      CDCUM=0.0
      K=0
C
C***  CALCULATE THE ELLIPSOID PARAMETERS
      A=L/2.0
      B=W/2.0
      C=H
C
C***  CALCULATE THE X AND Y  INCREMENTS
      DELX=A/FLOAT(NX)
      DELY=B/FLOAT(NY)
      DO 200 J=1,NY
        IF (J.EQ.1) THEN
          Y=DELY
          YT=DELY/2.0
        ELSE
          Y=YPREV+DELY
          YT=(YPREV+Y)/2.0
        ENDIF
        YPREV=Y
        IF (YT.GT.B) GOTO 300
        DO 100 I=1,NX
          IF (I.EQ.1) THEN
            X=DELX
            XT=DELX/2.0
          ELSE
            X=XPREV+DELX
            XT=(XPREV+X)/2.0
          ENDIF
          XPREV=X
          IF (XT.GT.A*SQRT(1.-(YT/B)**2)) GOTO 200
          ZT=C*SQRT(1-(XT/A)**2-(YT/B)**2)
          SINE=XT/A**2/SQRT(XT**2/A**4+YT**2/B**4+ZT**2/C**4)
          FX=-C*XT/A**2/SQRT(1.-(XT/A)**2-(YT/B)**2)
          FY=-C*YT/B**2/SQRT(1.-(XT/A)**2-(YT/B)**2)
          K=K+1
          DS=SQRT(FX**2+FY**2+1.)*DELX*DELY
          CDD=SINE**3*DS
          CDCUM=CDCUM+CDD
 100    CONTINUE
 200  CONTINUE
 300  CONTINUE
      RMACH=5.
      EQ1=(((G+1.))**2*RMACH**2)/((4.0*G*RMACH**2)-2.0*(G-1.0))
      EQ2=(1.0-G+(2.0*G*RMACH**2))/(G+1.0)
      CPMAX=(2.0/(G*RMACH**2))*((EQ1**(G/(G-1.0)))*EQ2-1.0)
      CD=4.0*CDCUM*CPMAX/(PI*B*C)
        IF (H .GT. DEL) THEN
        CD=CD*(1.0-.25*(DEL/H))
        ELSE
        CD=CD*(.75*((H/DEL)**(1.0/3.0)))
        ENDIF
      CALL CROSS(MACH,CDT)
      CD=CD*(CDT/1.2)
      IF(TMACH.EQ.0.) GO TO 345
      GO TO 340
 325  CONTINUE
C
C*** TRANSONIC DRAG CALCULATION
      TMACH=MACH
      MACH=MSUB
      GO TO 310
 320  CDSUB=CD
      SUBM=CD*1.172
      MACH=MSUP
      GO TO 330
 340  CDSUP=CD
      SUPM=0.
      CALL CUBIC(TMACH,MSUB,MSUP,CDSUB,CDSUP,SUBM,SUPM,CDTR)
      MACH=TMACH
      CD=CDTR
 345  CONTINUE
C
C***  FINAL CALCULATION OF DRAG COEFFICIENT
      CDSTR=CD*S
      RETURN
      END
