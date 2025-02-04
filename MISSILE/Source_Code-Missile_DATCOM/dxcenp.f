      FUNCTION DXCENP(NPTS,X,R)
C
C**  FUNCTION TO COMPUTE THE PLANFORM AREA CENTROID
C
C***  INPUTS
C
C  NPTS -- NUMBER OF INPUT POINTS
C     X -- X STATIONS
C     R -- RADII AT EACH X STATION
C
C
      COMMON /CONST/ PI,RAD,UNUSED,KAND
C
      DIMENSION X(NPTS),R(NPTS)
C
      DXCENP=0.0
      SPTOT=0.0
      DO 1000 I=2,NPTS
         DX=X(I)-X(I-1)
         DELSP=DX*(R(I)+R(I-1))
         SPTOT=SPTOT+DELSP
         IF(R(I).GE.R(I-1))DXCP=X(I)-DX/3.*(2.*R(I-1)+R(I))/
     1     (R(I)+R(I-1))
         IF(R(I).LT.R(I-1))DXCP=X(I-1)+DX/3.*(2.*R(I)+R(I-1))/
     1     (R(I)+R(I-1))
         DXCENP=DXCENP+DXCP*DELSP
 1000 CONTINUE
      DXCENP=DXCENP/SPTOT
      RETURN
      END
