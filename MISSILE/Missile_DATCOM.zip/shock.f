      SUBROUTINE SHOCK(GAM,AM,SD2,P2,PC,AMX,PT)
C
C  COMPUTE PRESSURE AND MACH NUMBER BEHIND A CONICAL SHOCK
C  REFERENCE - DEJARNETTE (AIAA J. SPACECRAFT, NOV-DEC 1980, P.529)
C
C
C  INPUTS
C
C    GAM -- RATIO OF SPECIFIC HEATS FOR THE GAS
C     AM -- MACH NUMBER
C    SD2 -- SINE SQUARED OF THE DEFLECTION ANGLE
C     P2 -- PRESSURE BEHIND THE SHOCK
C     PC -- CONICAL PRESSURE
C
C  OUTPUTS
C
C    AMX -- MACH NUMBER BEHIND THE SHOCK
C     PT -- TOTAL PRESSURE BEHIND THE SHOCK
C
C
C  SUBROUTINES CALLED -- NONE
C
      ST2=(GAM+1.)/2.*SD2+1./AM**2
      IF(ST2.GT.1.0)ST2=1.0
      T1=ST2*AM**2
C
C  COMPUTE (P/PT)**(1/GAM), NACA 1135 EQUATION 93 TO THE 1./GAM POWER
C
      T2=((GAM+1.)/(2.*GAM*T1-(GAM-1.)))**(1./GAM)
      T3=(GAM+1.)/(GAM-1.)*T1*(AM**2+2./(GAM-1.))/(2./(GAM-1.))
     1   /(T1+2./(GAM-1.))
      AMX=SQRT((T2*T3/PC**((GAM-1.)/GAM)-1.)/((GAM-1.)/2.))
C
C  COMPUTE TOTAL PRESSURE
C
      PT=P2*(1.+(GAM-1.)/2.*AMX**2)**(GAM/(GAM-1.))
      RETURN
      END
