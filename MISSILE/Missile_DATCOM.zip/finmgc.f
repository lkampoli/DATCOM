      FUNCTION FINMGC(CROOT,TAPER)
C
C***  FUNCTION TO COMPUTE THE MEAN GEOMETRIC CHORD OF A STRAIGHT
C***  TAPERED FIN OR FIN SEGMENT MEASURED FROM ROOT CHORD LEADING EDGE
C
C***  INPUTS
C
C  CROOT -- EXPOSED ROOT CHORD OF PANEL
C  TAPER -- PANEL TIP CHORD DIVIDED BY EXPOSED ROOT CHORD
C
      FINMGC=2./3.*CROOT*(1.+TAPER+TAPER**2)/(1.+TAPER)
C
      RETURN
      END
