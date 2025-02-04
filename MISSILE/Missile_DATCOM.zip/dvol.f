      FUNCTION DVOL(NPTS,X,R)
C
C**  FUNCTION TO COMPUTE THE VOLUME
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
      DVOL=0.0
      DO 1000 I=2,NPTS
         DX=X(I)-X(I-1)
         DVOL=DVOL+PI/3.*DX*(R(I)**2+R(I-1)**2+R(I)*R(I-1))
 1000 CONTINUE
      RETURN
      END
