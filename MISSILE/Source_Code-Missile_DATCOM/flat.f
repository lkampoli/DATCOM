      SUBROUTINE FLAT(MACH,DEL,H,W,L,OFF,CDFPL)
      REAL L,MACH,MSUB,MSUP
C
C  WRITTEN BY K. DETERS, MCDONNELL DOUGLAS
C
      MSUB=.6
      MSUP=1.
      G=1.4
      S=W*H
      HTOT=H+OFF
      TMACH=0.0
      IF(MACH.GT.MSUB) GO TO 115
C
C*** SUBSONIC DRAG CALCULATION
 110    IF (W .LE. H) THEN
        WH=W/H
        CHK=L/W
        ELSE
        WH=H/W
        CHK=L/H
        ENDIF
      CALL PLATE(WH,CDPL)
        IF (CHK .LT. 1.4) THEN
        CD=CDPL
        ELSE IF (CHK .GT. 3.) THEN 
        CD=.45*CDPL
        ELSE
        CD=(((4.74-1.1*CHK)/1.6)/2.)*CDPL
        ENDIF
      CD=CD/(SQRT(1.0-MACH**2))
      IF(TMACH.EQ.0.) GO TO 145
      GO TO 120
 115  IF(MACH.LT.MSUP) GO TO 125
C
C*** SUPERSONIC DRAG CALCULATION
 130  RMACH=5.
      EQ1=(((G+1.))**2*RMACH**2)/((4.0*G*RMACH**2)-2.0*(G-1.0))
      EQ2=(1.0-G+(2.0*G*RMACH**2))/(G+1.0)
      CPMAX=(2.0/(G*RMACH**2))*((EQ1**(G/(G-1.0)))*EQ2-1.0)
      CD=CPMAX
      CALL CROSS(MACH,CDT)
      CD=CD*(CDT/1.2)
      IF(TMACH.EQ.0.) GO TO 145
      GO TO 140
 125  CONTINUE
C
C*** TRANSONIC DRAG CALCULATION 
      TMACH=MACH
      MACH=MSUB
      GO TO 110
 120  CDSUB=CD
      SUBM=CD*1.172
      MACH=MSUP
      GO TO 130
 140  CDSUP=CD
      SUPM=0.
      CALL CUBIC(TMACH,MSUB,MSUP,CDSUB,CDSUP,SUBM,SUPM,CDTR)
      MACH=TMACH
      CD=CDTR 
 145  CONTINUE
C
C*** FINAL CALCULATION OF DRAG COEEFICIENT
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
      CDFPL=CD*S
      RETURN
      END        

