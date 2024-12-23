      SUBROUTINE SVRINT(HR,VR,CDIH,SDIH,S,RL,TAPER,VRINT)
C **
C **  COMPUTES INTERFERENCE FACTOR FOR VORTEX INTERACTION WITH
C **  LIFTING SURFACE.
C **
C **  REFERENCE - NACA 1307, PITTS, NIELSEN, AND KAATARI (APPENDIX B)
C **
C **  LIMITATIONS -  STRIP AND SLENDER-BODY THEORY.
C **
C **  NOTE - UNITS ON HR,VR,S AND RL MUST BE CONSISTENT.
C **
C **  INPUT -
C **        HR    = HORIZONTAL POSITION OF VORTEX MEASURED FROM
C **                VEHICLE CENTERLINE
C **        VR    = VERTICAL POSITION OF VORTEX MEASURED FROM
C **                VEHICLE CENTERLINE
C **        CDIH  = COSINE OF LIFTING SURFACE DIHEDRAL ANGLE
C **        SDIH  = SINE OF LIFTING SURFACE DIHEDRAL ANGLE
C **        S     = LIFTING SURFACE MAXIMUM SEMISPAN (INCLUDES BODY)
C **        RL    = LOCAL BODY RADIUS AT LIFTING SURFACE
C **        TAPER = TIP TO ROOT CHORD TAPER RATIO
C **
C **  OUTPUT -
C **        VRINT = VORTEX INTERFERENCE FACTOR
C **
C
      REAL L
      DIMENSION L(4)
C
      I=1
      HOR=HR
      VOR=VR
C
C **  HORIZONTAL AND VERTICAL  DISTANCES OF RIGHT HAND VORTEX
C **  FROM RIGHT HAND LIFTING SURFACE.
C
 1000 F=HOR*CDIH+VOR*SDIH
      H=VOR*CDIH-HOR*SDIH
C
C **  CHECK FOR VORTEX ON WING TIP
C
      IF(F.NE.S.OR.H.NE.0.0) GO TO 1010
      VRINT=-1.E10
      GO TO 1040
 1010 CONTINUE
      A=((S-RL*TAPER)-F*(1.-TAPER))/(2.*(S-RL))
      A=A*ALOG((H*H+(F-S)**2)/(H*H+(F-RL)**2))
      HH=H
      IF(H.EQ.0.0) HH=0.0001
      FS=(F-S)/H
      FR=(F-RL)/H
      AA=1.0
      IF(FS.LT.0.0) AA=-AA
      B=S-RL+H*ATAN(ABS(FS))*AA
      AA=1.0
      IF(FR.LT.0.0) AA=-AA
      B=B-H*ATAN(ABS(FR))*AA
      B=B*(1.-TAPER)/(S-RL)
      L(I)=A-B
      IF(I.EQ.4) GO TO 1030
      IF(I.EQ.2) GO TO 1020
C
C **  HORIZONTAL AND VERTICAL DISTANCES OF LEFT HAND VORTEX
C **  FROM RIGHT HAND LIFTING SURFACE.
C
      H=HOR*SDIH+VOR*CDIH
      F=VOR*SDIH-HOR*CDIH
      I=I+1
      GO TO 1010
C
C **  LOCATION OF IMAGE VORTEX
C
 1020 Z2=VOR*VOR+HOR*HOR
      IF(Z2.EQ.0.0) Z2=.0001
      VOR=VOR*RL*RL/Z2
      HOR=HOR*RL*RL/Z2
      I=I+1
      GO TO 1000
 1030 VRINT=(L(1)-L(2)-L(3)+L(4))*2./(1.+TAPER)
C
 1040 CONTINUE
      RETURN
      END
