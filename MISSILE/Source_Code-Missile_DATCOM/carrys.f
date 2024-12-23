      SUBROUTINE CARRYS(MACH,SWEEP,DIAM,CROOT,DX,KBW,XBW)
C
C***  SUPERSONIC FIN-BODY CARRY OVER WITH FINITE AFTERBODIES
C
C  INPUTS
C
C   MACH -- FREE STREAM MACH NUMBER
C  SWEEP -- FIN L.E. SWEEP ANGLE, DEG.
C   DIAM -- BODY DIAMETER AT FIN, FEET
C  CROOT -- FIN EXPOSED ROOT CHORD, FEET
C     DX -- LENGTH OF BODY AFT OF FIN T.E., FEET
C
C  OUTPUTS
C
C    KBW -- LIFT CARRY OVER FACTOR
C           (MUST BE DIVIDED BY BETA*CLA*(1+TAPER)*(S/R-1)
C            TO GET AERODYNAMIC FACTOR FOR USE)
C    XBW -- CENTER OF PRESSURE OF FIN IN PRESENCE OF BODY
C           (EXPOSED ROOT CHORDS AFT OF FIN ROOT L.E.)
C
C***  REFERENCES
C
C     THE FOLLOWING REFERENCES ARE APPLICABLE -
C     (1) AIAA JOURNAL, VOL. 19, NO. 5, MAY 1981, P. 661
C     (2) AIAA JOURNAL, VOL. 20, NO. 6, JUN 1982, P. 855
C     (3) AIAA JOURNAL, VOL. 20, NO. 8, AUG 1982, P. 1144
C
      COMMON /CONST/ PI,RAD,UNUSED,KAND
C
      REAL MACH,KBW,MBW
C
      ATANH(X)=0.5*(ALOG(1.+X)-ALOG(1.-X))
      ACOSH(X)=ALOG(X+SQRT(X*X-1.))
C
C ... CONSTANTS (SEE REF 2 OR REF 3)
C
      BETA=SQRT(ABS(MACH**2-1.))
      B=BETA/TAN(ABS(SWEEP)/RAD)
      D=BETA*DIAM/CROOT
      P=DX/(BETA*DIAM)
      IF(P .EQ. 0.)P=0.00001
      IF(P .GT. 1.)P=1.
      IF(P .LT. -2.)P=-2.
      R=P+1./D
C
      IF(P .LE. 0.)GO TO 1010
C
C***  POSITIVE AFTERBODY LENGTHS
C
      IF(B .GE. 1.)GO TO 1000
C
C***  SUBSONIC LEADING EDGE
C
C ... R .LT. 1
C
      KBW=16.*SQRT(B)*D/(PI*(B+1.))*(B**1.5/(D**2*(1.+B))*
     1 (SQRT((B+(1.+B)*P*D)/B)-2.)-B/(1.+B)/SQRT(D)*(B*R+P)**1.5+
     2 B*(1.+B)*R**2*ATAN(SQRT(1./(D*(B*R+P)))))
      MBW=(-4.*D**2*R**2/(3.*(B+1.)*SQRT(B))
     1 *(B**2+B*P/2./R-(B+3.)*P**2/4./R**2)*SQRT(D*(B*R+P))
     2 +SQRT(B**2+B*(B+1.)*P*D)/(9.*B*(B+1.)**3)*((8.*B+24.)*B**2
     3 +(14.*B+6.)*(B+1.)*B*P*D+3.*(B-3.)*(B+1.)**2*D**2*P**2)
     4 -(8.*B+24.)*B**3/(9.*B*(B+1.)**3)+4./3.*D**3*R**3*B**1.5
     5 *ATAN(SQRT(1./(D*(B*R+P))))-(3.-B)*P**3*D**3/3./B
     6 *(ATANH(SQRT(B/D/(B*R+P)))-ACOSH(SQRT((B*R+P)/(P*(1.+B))))))
C
      XBW=MBW*8./(PI*D*KBW)
C
      IF(R .LT. 1.)GO TO 1030
C
C ...  R .GE. 1
C
      KBW=KBW+16.*SQRT(B)*D/(PI*(1.+B))*((1.+B*R)*SQRT((R-1.)*(B*R+1.))
     1 -(1.+B)/SQRT(B)*ATANH(SQRT((B*R-B)/(B*R+1.)))
     2 -B*(1.+B)*R**2*ATAN(SQRT((R-1.)/(B*R+1.))))
      MBW=MBW+(4.*D**3*R**2/(3.*(1.+B)*SQRT(B))
     1 *(B**2+B/2./R-(B+3.)/4./R**2)*SQRT((R-1.)*(B*R+1.))
     2 -4./3.*D**3*R**3*B**1.5*ATAN(SQRT((R-1.)/(B*R+1.)))
     3 +(3.-B)*D**3/3./B*ATANH(SQRT((B*R-B)/(B*R+1.))))
C
      XBW=MBW*8./(PI*D*KBW)
C
      GO TO 1030
C
C***  SUPERSONIC LEADING EDGE
C
C ... R .LT. 1
C
 1000 KBW=8.*D/(PI*SQRT(B**2-1.))*((-B/(1.+B))*(B*R+P)**2
     1 *ACOS((R+B*P)/(B*R+P))+B/D**2*SQRT(B**2-1.)/(1.+B)*
     2 (SQRT(1.+2.*P*D)-1.)-B**2/(D**2*(1.+B))*ACOS(1./B)
     3 +B*R**2*SQRT(B**2-1.)*ACOS(P/R))
      MBW=(SQRT(1.+2.*D*P)*((2.*B+5.)/3./(1.+B)**2
     1 +D*P/(3.*(B+1.))-D**2*P**2/B)-(2.*B+5.)/(3.*(1.+B)**2)
     2 +1./SQRT(B**2-1.)*(D**3*R**3*(1.-2.*B)-1./(1.+B)**2-
     3 3.*P*D**3*R**2)*ACOS((R+B*P)/(B*R+P))-1./SQRT(B**2-1.)*
     4 (1.-1./(1.+B)**2)*ACOS(1./B)+2.*D**3*R**3*ACOS(P/R)
     5 +P**2*D**3/B*SQRT(R**2-P**2))
C
      XBW=MBW*8.*B/(3.*PI*D*KBW)
C
      IF(R .LT. 1.)GO TO 1030
C
C ...  R .GE. 1
C
      KBW=KBW+8.*D/(PI*SQRT(B**2-1.))*((B*R+1.)**2*
     1 ACOS((R+B)/(B*R+1.))-SQRT(B**2-1.)*ACOSH(R)
     2 +B*R**2*SQRT(B**2-1.)*(ASIN(1./R)-PI/2.))
      MBW=MBW+(D**3*R**3/SQRT(B**2-1.)*
     1 (2.*B+3./R-1./B**2/R**3)*ACOS((B+R)/(B*R+1.))+2.*D**3*R**3
     2 *(ASIN(1./R)-PI/2.)-D**3/B*SQRT(R**2-1.)+D**3/B**2*ACOSH(R))
C
      XBW=MBW*8.*B/(3.*PI*D*KBW)
C
      GO TO 1030
C
C***  NEGATIVE AFTERBODY LENGTHS
C
 1010 CONTINUE
C
C ... CUT-OFF FOR BASE AHEAD OF MACH LINE ACROSS BODY
C
      KBW=0.
      XBW=0.
      IF(P .LE. -1./D)GO TO 1030
C
      IF(B .GE. 1.)GO TO 1020
C
C ... SUBSONIC LEADING EDGE
C
C ... R .LT. 1
C
      KBW=16.*B*R/(PI*(1.+B))*(SQRT(B)*(1.+B)*ATAN(SQRT(1./B))-B)
     1 *(1.+D*P)
      XBW=2./3.*(1.+D*P)
C
      IF(R .LT. 1.)GO TO 1030
C
C ...  R .GE. 1
C
      KBW=KBW+16.*SQRT(B)/(PI*(B+1.)*R)*((B*R+1.)*SQRT((R-1.)*(B*R+1.))
     1 -(B+1.)/SQRT(B)*ATANH(SQRT((B*R-B)/(B*R+1.)))
     2 -B*(1.+B)*R**2*ATAN(SQRT((R-1.)/(B*R+1.))))*(1.+D*P)
      XBW=XBW+32.*R/(3.*PI*KBW)*(-B**1.5*ATAN(SQRT((R-1.)/(B*R+1.)))
     1 +1./((1.+B)*R*SQRT(B))*(B**2+B/2./R-(B+3.)/4./R**2)
     2 *SQRT((R-1.)*(B*R+1.))+(3.-B)/4./B/R**3*ATANH(SQRT((B*R-B)/
     3 (B*R+1.))))*(1.+D*P)
C
      GO TO 1030
C
C***  SUPERSONIC LEADING EDGE
C
C ... R .LT. 1
C
 1020 KBW=8.*B*R/(PI*SQRT(B**2-1.))*(SQRT(B**2-1.)*PI/2.
     1 -B*ACOS(1./B))*(1.+D*P)
      XBW=2./3.*(1.+D*P)
C
      IF(R .LT. 1.)GO TO 1030
C
C ...  R .GE. 1
C
      KBW=KBW+8./(PI*SQRT(B**2-1.)*R)*((B*R+1.)**2*ACOS((R+B)/(B*R+1.))
     1 -SQRT(B**2-1.)*ACOSH(R)+B*R**2*SQRT(B**2-1.)*(ASIN(1./R)-PI/2.))
     2 *(1.+D*P)
      XBW=XBW+8.*B*R/(3.*PI*KBW)*(2.*(ASIN(1./R)-PI/2.)
     1 +1./SQRT(B**2-1.)*(2.*B+3./R-1./B**2*R**3)*ACOS((B+R)/(B*R+1.))
     2 -SQRT(R**2-1.)/B/R**3+ACOSH(R)/B**2/R**3)*(1.+D*P)
C
 1030 CONTINUE
C
      RETURN
      END
