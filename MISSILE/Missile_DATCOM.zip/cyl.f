      SUBROUTINE CYL(MACH,RPUL,DEL,H,W,L,OFF,CDCYL)
      REAL MACH,L,MSUB,MSUP
C
C  WRITTEN BY K. DETERS, MCDONNELL DOUGLAS
C
      MSUB=.6
      MSUP=1.
      G=1.4
      S=W*H
      HTOT=H+OFF
      TMACH=0.0
      IF(MACH.GT.MSUB) GO TO 215
C
C*** SUBSONIC DRAG CALCULATION
 210  REL=RPUL*L
      CF=.074/(REL**.2)
      CDF=CF*(4.0+2.0*(L/W)+120.0*(W/L)**2)
      DH=W/H
      CALL ARCOR(DH,CDCIR)
      CD0=(CDF/1.2)*CDCIR
      CD=CD0/(SQRT(1.0-MACH**2))
      IF(TMACH.EQ.0.) GO TO 245
      GO TO 220
 215  IF(MACH.LT.MSUP) GO TO 225
C
C*** SUPERSONIC DRAG CALCULATION
C
C***  NUMBER OF X INCREMENTS
 230  NX=200
C
C***  CONSTANTS
      PI=3.141593
C
C***  INITIALIZE SUMMING VARIABLES
      CDCUM=0.0
C
C***  CALCULATE THE ELLIPSE PARAMETERS
      A=L/2.0
      B=W/2.0
C
C***  INITIALIZE XPREV AND YPREV
      XPREV=0.0
      YPREV=B
C
C***  CALCULATE THE X INCREMENTS
      DELX=A/FLOAT(NX)
      DO 200 I=1,NX
        X=XPREV+DELX
        XT=(XPREV+X)/2.0
        IF (X.GT.A) THEN
          X=A
          XT=(XPREV+X)/2.0
          DELX=X-XPREV
        ENDIF
        Y=B*SQRT(1.-(X/A)**2)
        THETA=ATAN(B*XT/A**2/SQRT(1.-(XT/A)**2))
        DELS=SQRT(DELX**2+(YPREV-Y)**2)
        CDD=(SIN(THETA))**3*DELS
        CDCUM=CDCUM+CDD
        YPREV=Y
        XPREV=X
 200  CONTINUE
      RMACH=5.
      EQ1=(((G+1.))**2*RMACH**2)/((4.0*G*RMACH**2)-2.0*(G-1.0))
      EQ2=(1.0-G+(2.0*G*RMACH**2))/(G+1.0)
      CPMAX=(2.0/(G*RMACH**2))*((EQ1**(G/(G-1.0)))*EQ2-1.0)
      CD=CDCUM*CPMAX/B
      CALL CROSS(MACH,CDT)
      CD=CD*(CDT/1.2)
      IF(TMACH.EQ.0.) GO TO 245
      GO TO 240
 225  CONTINUE
C
C*** TRANSONIC DRAG CALCULATION
      TMACH=MACH
      MACH=MSUB
      GO TO 210
 220  CDSUB=CD
      SUBM=CD*1.172
      MACH=MSUP
      GO TO 230
 240  CDSUP=CD
      SUPM=0.0
      CALL CUBIC(TMACH,MSUB,MSUP,CDSUB,CDSUP,SUBM,SUPM,CDTR)
      MACH=TMACH
      CD=CDTR
 245  CONTINUE 
C
C***  FINAL CALCULATION OF DRAG COEFFICIENT
        IF (OFF .EQ. 0.) THEN
          IF (H .GT. DEL) THEN
          CD=CD*(1.0-.25*(DEL/H))
          ELSE
          CD=CD*(.75*((H/DEL)**(1./3.)))
          ENDIF
        ELSE
          IF (OFF .GE. DEL) THEN
          CD=CD
          ELSE
            IF (HTOT .LE. DEL) THEN
            HAVG1=(HTOT+OFF)/2.
            CD=CD*((HAVG1/DEL)**(1./3.))
            ELSE
            HAVG2=(DEL+OFF)/2.
            CD=CD*(((HTOT-DEL)/H)+((DEL-OFF)/H)*((HAVG2/DEL)**(1./3.)))
            ENDIF
          ENDIF
        ENDIF
      CDCYL=CD*S
      RETURN
      END

