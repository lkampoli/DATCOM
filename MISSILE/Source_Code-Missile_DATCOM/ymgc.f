      FUNCTION YMGC(SPAN,CROOT,TAPER)
C
C***  FUNCTION TO COMPUTE THE LATERAL POSITION OF THE FIN
C***  MEAN GEOMETRIC CHORD MEASURED FROM ROOT CHORD
C
C***  INPUTS
C
C   SPAN -- PANEL EXPOSED SPAN (TIP-TO-TIP) FOR TWO PANELS
C           JOINED AT THEIR EXPOSED ROOT CHORDS
C  CROOT -- PANEL EXPOSED ROOT CHORD
C  TAPER -- PANEL TIP CHORD DIVIDED BY EXPOSED ROOT CHORD
C
C***  SECTION MEAN GEOMETRIC CHORD
C
      CBAR=FINMGC(CROOT,TAPER)
C
C***  LATERAL POSITION OF M.G.C. FROM ROOT CHORD
C
      YMGC=SPAN/2.*(1.+2.*TAPER)/(3.*(1.+TAPER))
C
      RETURN
      END
