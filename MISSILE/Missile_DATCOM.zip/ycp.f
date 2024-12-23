      SUBROUTINE YCP(AR,TR,FM,CNF,YCPS) 
C***********************************************************************
C
C  THIS ROUTINE ESTIMATES THE FIN SPANWISE CENTER OF PRESSURE 
C
C   DESCRIPTION OF DATA BASE: 
C
C     1.  FIN DESIGNATIONS AND GEOMETRIES
C
C                  AR\TR \  0.0  \  0.5  \  1.0  \
C                 --------------------------------
C                   0.25 \   I       12      II
C                   0.50 \   31      32      33
C                   1.00 \  III      42      IV
C                   2.00 \   51      52      53
C                   4.00 \   V       62      VI
C
C            NUMBERS INDICATE FIN DESIGNATIONS AND THE ROMAN
C            NUMERALS INDICATE REGIONS OF INTERPOLATION.
C
C     2.  MACH NUMBER, ANGLE OF ATTACK AND FIN ORIENTATION ANGLE RANGES
C
C            MACH  = 0.6,0.8,0.9,1.2,1.5,2.0,2.5,3.0,3.5,4.5
C            ALPHA = 0,2,5,10,15,20,25,30*,35,40,45
C
C            * MAXIMUM ALPHA FOR: 
C                1) MACH LESS THAN 0.8
C                2) MACH LESS THAN 1.2 IF AR < 1.0
C
C  INPUT: 
C     AR     -  ASPECT RATIO OF WING ALONE
C     TR     -  FIN TAPER RATIO
C     FM     -  FREESTREAM MACH NUMBER
C     CNF    -  FIN NORMAL FORCE COEFFICIENT, REFERENCED TO THE
C               WING ALONE AREA
C
C  OUTPUT:  
C     YCPS   -  FIN SPANWISE CENTER-OF-PRESSURE LOCATION FOR NO FIN 
C               DEFLECTION MEASURED OUTBOARD FROM THE ROOT CHORD AND
C               IS GIVEN AS A FRACTION OF THE EXPOSED SEMISPAN.
C
C  SUBPROGRAMS USED:  YBAR
C
C  WRITTEN BY  NIELSEN ENGINEERING AND RESEARCH, 8-89 
C              (415) 968-9457 
C
C***********************************************************************
      DIMENSION AXY(11),FMACH(10),YS(11,10),IROUT(2)
      REAL YSN(2)
C
      DATA AXY/0.,0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6,1.8,2.0/
      DATA FMACH/.6,.8,.9,1.2,1.5,2.,2.5,3.,3.5,4.5/
C
C DETERMINE MACH NUMBER RANGE OF STABILITY DATA
      IM2=10
      DO 110 I=2,10
        IF (FM.LE.FMACH(I)) THEN
          IM2=I
          GOTO 10
        ENDIF
  110 CONTINUE
   10 IM1=IM2-1
      AM = (FM-FMACH(IM1))/(FMACH(IM2)-FMACH(IM1))
C
C DETERMINE FIN NORMAL FORCE RANGE OF STABILITY DATA
      JN2=11
      DO 120 J=2,11
        IF (CNF.LE.AXY(J)) THEN
          JN2=J
          GOTO 20
        ENDIF
  120 CONTINUE
   20 JN1=JN2-1
      AN = (CNF-AXY(JN1))/(AXY(JN2)-AXY(JN1))
C
C CALCULATE STABILITY YCP 
      K=0
      DO 210 I=IM1,IM2
        K=K+1
        DO 220 J=JN1,JN2
          YS(J,I) =YBAR(AXY(J),FMACH(I),AR,TR)
  220   CONTINUE
        YSN(K)  = YS(JN1,I) + AN*(YS(JN2,I)-YS(JN1,I))
  210 CONTINUE
      YCPS = YSN(1) + AM*(YSN(2)-YSN(1))
C
      RETURN
      END
