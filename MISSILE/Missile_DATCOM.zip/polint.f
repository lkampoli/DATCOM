      SUBROUTINE POLINT(X,Y,NPT,XVAL,YVAL,DYDX)
C
C***  INTERPOLATES WITHIN THE Y VERSUS X CURVE AT XVAL FOR
C***  YVAL USING A THIRD ORDER POLYNOMINAL INTERPOLATION OR
C***  EXTRAPOLATION.
C***
C***  METHOD --
C***
C***  NPT .GE. 3 - A THIRD ORDER POLYNOMINAL IS DETERMINED
C***      FOR EACH INTERVAL BETWEEN DATA POINTS USING THE
C***      INTERVAL END POINT COORDINATES AND SLOPES.  SECOND
C***      ORDER POLYNOMINALS ARE DETERMINED FOR THE LEFT OR
C***      RIGHT ENDS OF THE CURVE USING THE END POINT COORDINATES
C***      AND THE ADJACENT INTERIOR POINT COORDINATES AND SLOPE.
C***      THE RESULTING INTERPOLATED CURVE AND ITS DERIVATIVES
C***      ARE CONTINUOUS.
C***
C***  NPT .EQ. 2 - A LINEAR CURVE IS DEFINED BY THE TWO POINTS
C***      AND CAN BE INTERPOLATED OR EXTRAPOLATED FOR ALL ABSCISSA
C***      VALUES.
C***
C***  NPT .EQ. 1 - YVAL IS SET TO Y(1).  DYDX IS SET TO 0.0.
C***
C***  REFERENCES -  NONE
C***
C***  LIMITATIONS -  Y VERSUS X MUST BE SINGLE VALUED
C***
C***  INPUTS
C***      X   - ARRAY OF ABSCISSA VALUES
C***      Y   - ARRAY OF ORDINATE VALUES
C***      NPT - NUMBER OF POINTS IN X-Y ARRAYS
C***
C***  OUTPUTS
C***      YVAL- INTERPOLATED ORDINATE VALUE
C***      DYDX- FIRST DERIVATIVE OF Y AT XVAL
C
      INTEGER RP
      DIMENSION X(NPT),Y(NPT),YP(2)
      IF(NPT.GT.2)GO TO 1000
      GO TO (1090,1080),NPT
C
C***  HERE FOR MORE THAN TWO POINTS
C
 1000 CONTINUE
      NP1=NPT-1
      NP2=NPT-2
      IF(XVAL.LT.X(2)) GO TO 1020
      IF(XVAL.GE.X(NP1)) GO TO 1030
C
C***  HERE IF XVAL IS BRACKETTED BY TWO POINTS ON BOTH SIDES
C
      DO 1010 J=2,NP2
         K=J+1
      IF(XVAL.GE.X(K)) GO TO 1010
         LOCATE=2
      GO TO 1040
 1010 CONTINUE
      GO TO 1030
C
C***  HERE IF XVAL.LT.X(2)
C
 1020 J=2
      K=3
      LOCATE=1
      GO TO 1040
C
C***  HERE IF XVAL.GE.X(NPT-1)
C
 1030 J=NP1
      K=NPT
      LOCATE=3
C
C***  HERE TO COMPUTE LEFT AND RIGHT SLOPES
C
 1040 DO 1050 N=1,2
         LP=J+N-2
         MP=J+N-1
         RP=K+N-1
         SL=(Y(LP)-Y(MP))/(X(LP)-X(MP))
         SR=(Y(MP)-Y(RP))/(X(MP)-X(RP))
         YP(N)=TAN((ATAN(SR)+ATAN(SL))*0.5)
      GO TO (1060,1050,1060),LOCATE
 1050 CONTINUE
C
C***  HERE TO DETERMINE POLYNOMIAL
C
      X1S=X(J)*X(J)
      X2S=X(K)*X(K)
      X12F=X(J)-X(K)
      Y12F=Y(J)-Y(K)
      X12S=X1S-X2S
      X12C=X1S*X(J)-X2S*X(K)
      Y12P=YP(1)-YP(2)
C
      TW=2.
      TH=3.
      RED=TW*X(K)*X12F-X12S
      GRN=TH*X2S*X12F-X12C
      YEL=YP(2)*X12F-Y12F
      E=TH*X12S*RED-TW*X12F*GRN
      A=Y12P*RED-TW*X12F*YEL
      A=A/E
      B=TH*X12S*YEL-Y12P*GRN
      B=B/E
      C=(Y12F-A*X12C-B*X12S)/X12F
      D=Y(K)-A*X2S*X(K)-B*X2S-C*X(K)
      GO TO 1070
C
 1060 J=1
      IF(LOCATE.EQ.3) J=NP1
      K=J+1
      L=2
      IF(LOCATE.EQ.3) L=NP1
      Z=(Y(J)-Y(K))/(X(J)-X(K))
      A=0.0
      B=(YP(1)-Z)/(2.0*X(L)-X(J)-X(K))
      C=YP(1)-2.0*B*X(L)
      D=Y(J)-((B*X(J)+C)*X(J))
 1070 YVAL=(((A*XVAL+B)*XVAL)+C)*XVAL+D
      DYDX=(3.0*A*XVAL+2.0*B)*XVAL+C
      GO TO 1100
 1080 CONTINUE
C
C***  HERE IF NPT .EQ. 2
C
      YVAL=Y(1)+(XVAL-X(1))*(Y(2)-Y(1))/(X(2)-X(1))
      DYDX=(Y(2)-Y(1))/(X(2)-X(1))
      GO TO 1100
 1090 CONTINUE
C
C***  HERE IF NPT .EQ. 1
C
      YVAL=Y(1)
      DYDX=0.0
C
 1100 CONTINUE
C
      RETURN
      END
