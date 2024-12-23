      SUBROUTINE INTER5(X,X1,X2,X3,X4,X5,F1,F2,F3,F4,F5,F)
C     5 POINT LAGRANGE INTERPOLATION SUBROUTINE
C     X1.LE.X.LE.X5
      SL=(F3-F2)/(X3-X2)
      SLP=(F4-F3)/(X4-X3)
      SLM=(F2-F1)/(X2-X1)
      SLPP=(F5-F4)/(X5-X4)
      SL1=(SLPP-SLP)/(X5-X3)
      SL2=(SLP-SL)/(X4-X2)
      SL3=(SL-SLM)/(X3-X1)
      IF(SL1.EQ.0.0.OR.SL2.EQ.0.0.OR.SL3.EQ.0.0)GO TO 1000
      A1=(X-X2)*(X-X3)*(X-X4)*(X-X5)
      A2=(X-X1)*(X-X3)*(X-X4)*(X-X5)
      A3=(X-X1)*(X-X2)*(X-X4)*(X-X5)
      A4=(X-X1)*(X-X2)*(X-X3)*(X-X5)
      A5=(X-X1)*(X-X2)*(X-X3)*(X-X4)
      D1=(X1-X2)*(X1-X3)*(X1-X4)*(X1-X5)
      D2=(X2-X1)*(X2-X3)*(X2-X4)*(X2-X5)
      D3=(X3-X1)*(X3-X2)*(X3-X4)*(X3-X5)
      D4=(X4-X1)*(X4-X2)*(X4-X3)*(X4-X5)
      D5=(X5-X1)*(X5-X2)*(X5-X3)*(X5-X4)
      C1=A1/D1
      C2=A2/D2
      C3=A3/D3
      C4=A4/D4
      C5=A5/D5
      F=C1*F1+C2*F2+C3*F3+C4*F4+C5*F5
      GO TO 1030
 1000 IF(SL1.NE.0.0)GO TO 1010
      IF(X.GE.X3)F=F3+SLP*(X-X3)
      IF(X.LT.X3)F=F2+(X-X2)*(.5*(SL+SLM-SL3*(X3+X1-2.*X2))+SL3*(X-X2))
      GO TO 1030
 1010 IF(SL2.NE.0.0)GO TO 1020
      IF(X.GE.X4)F=F4+SLPP*(X-X4)
      IF(X.LE.X2)F=F1+SLM*(X-X1)
      IF(X.GT.X2.AND.X.LT.X4)F=F2+SL*(X-X2)
      GO TO 1030
 1020 IF(X.LE.X3)F=F1+SLM*(X-X1)
      IF(X.GT.X3)F=F4+(X-X4)*(.5*(SLPP+SLP-SL1*(X3+X5-2.*X4))+SL1*(X-X4)
     1)
 1030 CONTINUE
      RETURN
      END
