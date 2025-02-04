      SUBROUTINE CMINC(ICASE,CMS,ICON,DUMMY,PHI)
C      
C         SUBROUTINE TO CALCULATE CM INCREMENTALS AND TO COMPUTE
C      NEW CM VALUES WITH THOSE INCREMENTALS.  IF AERODYNAMIC ROLL
C      ANGLE IS ZERO, THERE ARE CORRECTION TERMS COMPUTED FOR THE 
C      POTENTIAL AND VISCOUS TERMS.  IF AERODYNAMIC ROLL ANGLE IS 
C      NOT ZERO, A TABLE OF MULTIPLIERS IS USED AS THE CORRECTION
C      FACTOR.
C      
C***  INPUTS
C      
C     ICASE = RUN CASE NUMBER
C     CMS   = ARRAY FROM SYNTHS CONTAINING CONFIGURATION CM VALUES
C     DUMMY = ARRAY THAT CONTAINS EXPERIMENTAL COEFFICIENTS AND WILL
C             CONTAIN UPDATED COEFFICIENTS CM = DUMMY 21-40
C     ICON  = CONFIGURATION TO BE INCREMETED
C             ICON = 1    BODY
C             ICON = 2    BODY + 1 FIN SET
C             ICON = 3    BODY + 2 FIN SETS
C             ICON = 4    BODY + 3 FIN SETS
C             ICON = 5    BODY + 4 FIN SETS
C     PHI   = BODY AXIS AERODYNAMIC ROLL ANGLE
C      
C***  OUTPUT
C      
C     CM = UPDATED CM VALUES EQUIVALENT TO DUMMY 21-40
C          NOTE: ON THE FIRST TIME THROUGH DUMMY COMES IN WITH THE 
C                EXPERIMENTAL VALUES AND LEAVES WITH INCREMENTED VALUES
C      
      COMMON /FLC   / NA,ALPHA(20),FLDUM(123)
      COMMON /INC   / DUM(22),C1,C2(20),DCM0,DIP(84),ZIP,ALP(20),NAP
      COMMON /CONST/ PI,RAD,UNUSED,KAND
      DIMENSION CM(20),CMEXP(20),CMTH(20),CMATH(20),Z(20),
     1          CMPTH(20),CMVTH(20),CMPEXP(20),DUMMY(120),CMS(20,5)
      REAL NA,NAP
C
C **  INITIALIZE WORKING ARRAYS
C      
      DO 1000 I=1,20
         CM(I)=UNUSED
         CMEXP(I)=UNUSED
         CMPTH(I)=UNUSED
         CMVTH(I)=UNUSED
         CMTH(I)=UNUSED
         CMPEXP(I)=UNUSED
 1000 CONTINUE
C      
      NAP1= NAP + .5
      NALPHA = NA + .5
C      
C       USE APPROPRIATE IOM COMMON BLOCK TO FILL THE WORKING ARRAYS
C      
      DO 1010 I=1,NALPHA
         IF(ICON .EQ. 1)CMTH(I)=CMS(I,5)
         IF(ICON .GT. 1)CMTH(I)=CMS(I,ICON-1)
 1010 CONTINUE
C
C     ZIP=0  FIRST TIME THROUGH FROM BETADR SO PHI IS AS INPUT
C     ZIP=1 SECOND TIME THROUGH FROM BETADR SO PHI HAS BEEN INCREASED BY 1
C
      IF(ZIP .EQ. 0. .AND. PHI .NE. 0.)GO TO 1030
C      
C    COMPUTE THEORETICAL CMA
C      
      CALL CALIB(4,NALPHA,ALPHA,CMEXP,NALPHA,ALPHA,CMTH,SLOPE)
      CMATH(1)=SLOPE*RAD
 1030 IF(ICASE .NE. 1 .OR. ZIP .EQ. 1)GO TO 1070
C
C     FOR FIRST CASE FIRST TIME TROUGH PUT EXPERIMENTAL VALUES FROM DUMMY
C        INTO WORKING ARRAY
C
      DO 1040 I=1,NALPHA
         CMEXP(I)= DUMMY(I+20)
 1040 CONTINUE
      IF(PHI .NE. 0.)GO TO 1110
C      
C       DETERMINE THE DIFFERENCE BETWEEN CM EXPERIMENTAL AND CM
C          THEORETICAL AT ALPHA = 0
C      
      CALL LNTRP(ALPHA,CMEXP,NALPHA,0.,CMEXP0)
      CALL LNTRP(ALPHA,CMTH,NALPHA,0.,CMTH0)
      DCM0= CMEXP0 - CMTH0
C
C     SUBTRACT DCM0 FROM THE EXPERIMENTAL CM TO BRING DOWN TO CM 
C          THEORETICAL CURVE (WILL BE ADDED IN AT END)
C
      DO 1060 I=1,NALPHA
         CMEXP(I) = CMEXP(I) - DCM0
 1060 CONTINUE
C      
C***  USE CALIB TO DETERMINE THE POTENTIAL INCREMENTAL C1, A SCALAR 
C          MULTIPLIER AT ALPHA=0 
C      
       CALL CALIB(-3,NALPHA,ALPHA,CMEXP,NALPHA,ALPHA,CMTH,Z)
       C1=Z(1)
C      
C      CALCULATE THE THEORETICAL POTENTIAL CM AND VISCOUS CM FROM TOTAL CM
C      
 1070 IF(PHI .NE. 0.)GO TO 1120
      DO 1080 I=1,NALPHA
         CMPTH(I)=.5*CMATH(1)*SIN(2.*ALPHA(I))*COS(ALPHA(I)/2.)
         CMVTH(I)=CMTH(I) - CMPTH(I)
 1080 CONTINUE
C      
C        USE THE INCREMENTAL FACTOR C1 TO DETERMINE THE CORRECTED
C          POTENTIAL CM WHICH IS THE EXPERIMENTAL CM POTENTIAL
C      
      DO 1090 I=1,NALPHA
         CMPEXP(I)=C1*CMPTH(I)
 1090 CONTINUE
C
C*    ONLY ON FIRST CASE AND FIRST TIME THROUGH IS CM VISCOUS NEEDED
C       TO DETERMINE VISCOUS CORRECTION TERMS
C
      IF (ICASE .NE. 1 .OR. ZIP .EQ. 1)GO TO 1140
C      
C***  DETERMINE THE VISCOUS INCREMENTAL BY TAKING THE RATIO OF
C       EXPERIMENTAL CM VISCOUS TO THEROTICAL CM VISCOUS
C      
      DO 1100 I=1,NALPHA
         IF(CMVTH(I) .EQ. 0.)C2(I)=1.
         IF(CMVTH(I) .EQ. 0.)GO TO 1100
         C2(I)=(CMEXP(I) - CMPEXP(I))/ CMVTH(I)
 1100 CONTINUE
      GO TO 1140
C
C***  HERE FOR AERODYNAMIC ROLL ANGLE NOT ZERO
C     USE CALIB TO DETERMINE A TABLE OF MULTIPLIERS
C
 1110 CALL CALIB(2,NALPHA,ALPHA,CMEXP,NALPHA,ALPHA,CMTH,C2)
C
C***  USE TABLE OF MULTIPLIERS DETERMINED AT FIRST CASE ALPHA'S TO 
C     INCREMENT CM AT ALPHA OF INTEREST
C
 1120 DO 1130 I=1,NALPHA
         CALL LNTRP(ALP,C2,NAP1,ALPHA(I),C2I)
         CM(I)=CMTH(I)*C2I
 1130 CONTINUE
      GO TO 1160
C      
C       COMPUTE THE INCREMENTED CM FOR AERODYNAMIC ROLL = 0
C      
 1140 DO 1150 I=1,NALPHA
         CALL LNTRP(ALP,C2,NAP1,ALPHA(I),C2I)
         CM(I)=CMPEXP(I) + C2I*CMVTH(I) + DCM0
 1150 CONTINUE
C
C**   PUT INCREMENTED CM INTO ARRAY DUMMY
C
 1160 DO 1170 I=1,NALPHA
         DUMMY(I+20)=CM(I)
 1170 CONTINUE
      RETURN
      END
