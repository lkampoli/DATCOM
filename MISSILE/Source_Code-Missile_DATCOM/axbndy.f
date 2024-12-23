      SUBROUTINE AXBNDY(I,BETA,VX,VR,NN1,NN2,NN3,IK,JL,C,TTX,TTR)
C
      COMMON /VDARY/  XB(220),RB(220),RBP(220),DUMM(220),C1(220),
     1 RB1(220),RBP1(220),B(220),PSI(220),ZE0X(220),PSIR(220),PHIX(220)
      DIMENSION C(220)
C
      C(I) = 0.
      XI = XB(I-1)-BETA*RB(I-1)
      TAU = BETA*RB(I)/(XB(I)-XI)
      IF(TAU .GE. 1.0) GO TO 1020
          TAU1 = BETA*RB(I)/(XB(I)-XB(1)+BETA*RB(1))
          IF(TAU1 .GE. 1.0) GO TO 1000
              TTR = C(1)*BETA*SQRT(1.0-TAU1**2)/TAU1
              TTX = -C(1)*ARSECH(TAU1)
            GO TO 1010
 1000         TTR = 0.0
              TTX = 0.0
 1010     CONTINUE
          F1 = ARSECH(TAU)
          F2 = SQRT(1.-TAU**2)
          C(I) = RBP(I)*(1.0+VX+TTX)-TTR-VR
          DEN = (XB(I)-XI)*(BETA*(F2/TAU-TAU*F1)+2.*RBP(I)*(F1-F2))
          C(I) = C(I)/DEN
 1020 CONTINUE
      IF(IK.LT.2) JL=NN1
      IF(IK.EQ.2) JL=NN2
      IF(IK.GT.2) JL=NN3
 1060 CONTINUE
      RETURN
      END
