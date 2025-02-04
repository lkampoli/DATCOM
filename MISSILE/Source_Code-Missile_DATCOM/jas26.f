      SUBROUTINE JAS26(XBI,BETA,RBJM1,XBJM1,XXI,RBI,TAU,F1,F2,CJ,
     1 SUM1,SUM2,SUM3,SUM4,SUM5,SUM6,SIGN,NTERM)
C
C  ROUTINE ADAPTED FROM NSWC AEROPREDICTION CODE
C  ORIGINAL ROUTINE WRITTEN BY F. MOORE, NSWC
C  MODIFIED BY J. JENKINS, WL/FIGC
C
C***  SOLUTION OF J.A.S. EQUATIONS 26
C
      XXI=XBI+BETA*RBJM1-XBJM1
      TAU=BETA*RBI/XXI
      IF(TAU .GE. 1.)TAU=.9999
      F1=ARSECH(TAU)
      F2=SQRT(1.-TAU**2)
      SUM1=SUM1+SIGN*CJ*XXI**2*((1.+.5*TAU**2)*F1-1.5*F2)
      SUM2=SUM2+SIGN*2.*CJ*XXI*(F1-F2)
      SUM3=SUM3-SIGN*BETA*CJ*XXI*(F2/TAU-TAU*F1)
      IF(NTERM .GT. 0 .AND. NTERM .LT. 6)GO TO 1000
      SUM4=SUM4+SIGN*2.*CJ*F1
      SUM5=SUM5-SIGN*2.*BETA*CJ*F2/TAU
      SUM6=SUM6+SIGN*BETA**2*CJ*(F2/TAU**2+F1)
 1000 CONTINUE
      RETURN
      END
