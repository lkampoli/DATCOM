      SUBROUTINE HXZU(RLE, T, C1, SWP, XU, ZU, DZDX, XM, ZM)
C
C     CALCULATE THE INTERSECTION POINT OF THE ELLIPTICAL NOSE
C     CAP AND WEDGE SURFACE AND LOCATION OF POINT WHERE LOCAL
C      SLOPE IS 15 DEG.
C
C     INPUT VARIABLES
C         RLE - L.E. RADIUS
C           T - THICKNESS
C          C1 - CHORD OF WEDGE
C         SWP - SWEEP ANGLE, DEG.
C
C     OUTPUT VARIABLES
C          XU - X-LOCATION OF INTERSECTION
C          ZU - Z-LOCATION OF INTERSECTION
C
      COMMON /CONST/ PI,RAD,UNUSED,KAND
C
      CS = COS(SWP/RAD)
      CP = C1*CS/RLE
      TP = 0.5*T/RLE
      A  = (1.0-CP)**2+TP**2
      B  = 2.0*(CP*(1.0-CP)-TP**2)
      C  = CP**2
      XP = -B-SQRT(B**2-4.0*A*C)
      XP = XP/(2.0*A)
      ZP = SQRT(2.0*XP-XP**2)
      XU = XP*RLE/CS
      ZU = ZP*RLE
      DZDX = (T/2.0-ZU)/(C1-XU)
C
      TM = TAN(15.0/RAD)
      F  = CS*CS/(TM*TM+CS*CS)
      G  = SQRT(1.0-F)
      XM = RLE*(1.0-G)/CS
      ZM = RLE*CS*G/TM
C
      RETURN
      END
