      SUBROUTINE SPLN2(XTERM,XMULT)
C
C***  2ND ORDER POLYNOMIAL SPLINE ROUTINE
C
      X1=.75
      X2=2.0
      Y1=1.0
      Y2=.05
      DY2=.005
       IF (XTERM .GT. X2) THEN
       XMULT=Y2-XTERM*DY2
       ELSE
       A=(Y1-Y2)/(X2-X1)**2+DY2/(X2-X1)
       B=DY2-2.*X2*A
       C=Y1-A*X1**2-B*X1
       XMULT=A*XTERM**2+B*XTERM+C
       ENDIF
      RETURN
      END
