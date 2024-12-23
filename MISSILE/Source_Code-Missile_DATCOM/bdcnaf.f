      SUBROUTINE BDCNAF(DFWD,DAFT,CNAF)
C
C  COMPUTE THE INCREMENTAL NORMAL FORCE COEFFICIENT SLOPE
C  DUE TO A FLARE AT SUBSONIC/TRANSONIC SPEEDS
C
C  METHOD FROM AMC PAMPHLET AMCP 706-280, JULY 1968
C
C***  INPUTS
C
C  DFWD - DIAMETER OF THE FORWARD END OF THE FLARE
C  DAFT - DIAMETER OF THE BASE OF THE FLARE
C
***  OUTPUT
C
C  CNAF - FLARE NORMAL FORCE SLOPE INCREMENT, PER RADIAN
C         BASED OF FORWARD AREA OF FLARE
C
      COMMON /CONST/ PI,RAD,UNUSED,KAND
C
      DIMENSION DCDF(6),DCNA(6),IROUT(2)
      DATA DCDF / 0.,.2,.4,.6,.8,1./
      DATA DCNA / 2.,1.94,1.68,1.26,0.72,0./
C
      CALL LNTRP(DCDF,DCNA,6,DFWD/DAFT,DCNAF)
C
      CNAF=DCNAF*DAFT**2/DFWD**2
C
      RETURN
      END
