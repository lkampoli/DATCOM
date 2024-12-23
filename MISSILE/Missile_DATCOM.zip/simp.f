      SUBROUTINE SIMP(CPV,JA,JB,RR,SUM1,SUM2,SUM3)
C
C  ROUTINE ADAPTED FROM NSWC AEROPREDICTION CODE
C  ORIGINAL ROUTINE WRITTEN BY F. MOORE, NSWC
C  MODIFIED BY J. JENKINS, WL/FIGC
C
      COMMON /VDARY/ XB(220),RB(220),RBP(220),C(220),C1(220),RB1(220),
     1  RBP1(220),B(220),PSI(220),ZE0X(220),PSIR(220),PHIX(220)
      DIMENSION CPV(220,7)
      DIMENSION F1(220),G(220),G1(220)
      DO 1000 I=JA,JB
      A1=CPV(I,1)+2.*(CPV(I,3)+CPV(I,5))+CPV(I,7)
      A2=4.*(CPV(I,2)+CPV(I,4)+CPV(I,6))
      F1(I)=0.17454*(A1+A2)*RB(I)
      B1=CPV(I,1)-CPV(I,7)+2.*(CPV(I,3)-CPV(I,5))*0.5
      B2=4.*(CPV(I,2)-CPV(I,6))*0.8667
      G(I)=0.17454*(B1+B2)*RB(I)
      G1(I)=G(I)*XB(I)
 1000 CONTINUE
      IF(JA.NE.JB) GO TO 1010
      SUM1=0.
      SUM2=0.
      SUM3=0.
      GO TO 1050
 1010 JBB=JB-1
      DO 1040 I=JA,JBB
      H=(RB(I+1)-RB(I))/6.
      X12=(XB(I+1)+XB(I))/2.
      IF((JB-JA).LT.5) GO TO 1020
      J=JA+2
      CALL INTERP(XB,F1,X12,F12,JB,J)
      CALL INTERP(XB,G,X12,G12,JB,J)
      CALL INTERP(XB,G1,X12,G112,JB,J)
      GO TO 1030
 1020 F12=(F1(I)+F1(I+1))/2.
      G12=(G(I)+G(I+1))/2.
      G112=(G1(I)+G1(I+1))/2.
      IF(JB.GT.2) GO TO 1030
      F1(3)=F1(2)
      G(3)=G(2)
      G1(2)=2./3.*G1(2)
      G1(3)=G1(2)
      G112=2./3.*G112
 1030 SUM1=SUM1+H*(F1(I)+4.*F12+F1(I+1))
      H1=(XB(I+1)-XB(I))/6.
      SUM2=SUM2+H1*(G(I)+4.*G12+G(I+1))
      SUM3=SUM3+H1*(G1(I)+4.*G112+G1(I+1))
 1040 CONTINUE
      SUM3=SUM3+SUM2*RR
 1050 CONTINUE
      RETURN
      END
