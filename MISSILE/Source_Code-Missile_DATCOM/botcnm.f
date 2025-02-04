      SUBROUTINE BOTCNM(ISHAPE,SREF,LREF,XCG,XO,XB,FMACH,ALPHA,
     1              BOTLEN,MISDIA,BASDIA,XSEPA0,BOTDCN,BOTDCM)
C
C     THIS SUBROUTINE CALCULATES THE INCREMENTAL NORMAL FORCE
C     AND PITCHING MOMENT COEFFICIENTS ON A BOATTAIL DUE TO 
C     FLOW SEPARATION.
C
C  WRITTEN BY A. JENN, MCDONNELL DOUGLAS
C
      REAL MISDIA,LREF
C
C     CHECK FOR A CYLINDRICAL BOATTAIL
      IF (ISHAPE .EQ. 1 .OR .BASDIA .GE. MISDIA) THEN
C
C        SET COEFFICIENTS TO ZERO
         BOTDCN = 0.0
         BOTDCM = 0.0
C
C        EXIT THE SUBROUTINE
         GOTO 100
C
      END IF
C
C     CALCULATE THE VALUE OF PI
      PI = ACOS(-1.0)
C
C     CALCULATE THE MISSILE AND BASE RADII
      RMIS = MISDIA / 2.0
      RBAS = BASDIA / 2.0
C
C     CALCULATE CROSS SECTIONAL AREA OF MISSILE
      CSA1 = PI * RMIS**2
C
C     CALCULATE BOATTAIL BASE AREA FOR UNSEPARATED CASE
      CSA2U = PI * RBAS**2
C
C     CALCULATE THE LENGTH OF THE BOATTAIL AT SEPARATION
      BOTLS = BOTLEN - XSEPA0
C
C     CALCULATE VOLUME OF CYLINDER WITH SAME FINENESS RATIO AS BOATTAIL
      VOL1U = CSA1 * BOTLEN
      VOL1S = CSA1 * BOTLS
C
C     CALCULATE REMAINING BOATTAIL GEOMETRIC PARAMETERS
      IF (ISHAPE .EQ. 2) THEN
C
C        CONICAL BOATTAIL
C
C        BOATTAIL DIAMETER AT SEPARATION
         DBSEP = BASDIA + (MISDIA - BASDIA) * XSEPA0 / BOTLEN
C
C        BOATTAIL VOLUME FOR UNSEPARATED CASE
         VOL2U = PI * BOTLEN / 3.0 * (RMIS**2 + RBAS**2 + RMIS * RBAS)
C
C        BOATTAIL VOLUME FOR SEPARATED CASE
         RSEP = DBSEP / 2.0
         VOL2S = PI * BOTLS / 3.0 * (RMIS**2 + RSEP**2 + RMIS * RSEP)
C
      ELSE
C
C        OGIVE BOATTAIL
C
C        RADIUS OF CURVATURE
         DR = RMIS - RBAS
         RCUR = (DR**2 + BOTLEN**2) / (2.0 * DR)
C
C        BOATTAIL DIAMETER AT SEPARATION
         P = SQRT(RCUR**2 - BOTLEN**2)
         DBSEP = BASDIA + 2.0 * (SQRT(RCUR**2 - BOTLS**2) - P)
C
C        BOATTAIL VOLUME FOR UNSEPARATED CASE
         PMRB   = P - RBAS                           
         VOL2U = PI * BOTLEN * (PMRB**2 + RCUR**2 - BOTLEN**2 / 3.0 -
     1           PMRB * (P + RCUR**2 / BOTLEN * ASIN(BOTLEN / RCUR)))
C
C        BOATTAIL VOLUME FOR SEPARATED CASE
         P = SQRT(RCUR**2 - BOTLS**2)
         VOL2S = PI * BOTLS * (PMRB**2 + RCUR**2 - BOTLS**2 / 3.0 -
     1           PMRB * (P + RCUR**2 / BOTLS * ASIN(BOTLS / RCUR)))
C
      END IF         
C
C     CALCULATE BOATTAIL BASE AREA FOR SEPARATED CASE
      CSA2S = PI * DBSEP**2 / 4.0
C
C     CALCULATE NORMAL FORCE CURVE SLOPE FOR UNSEPARATED BOATTAIL
      CALL BDCNAB(FMACH, BASDIA, MISDIA, CNABU)
C
C     CONVERT TO NORMAL FORCE COEFFICIENT
      CNBU = CNABU * ALPHA * PI / 180.0
      CNBU=CNBU*PI*MISDIA**2/4./SREF
C
C     CALCULATE NORMAL FORCE CURVE SLOPE FOR SEPARATED BOATTAIL
      CALL BDCNAB(FMACH, DBSEP, MISDIA, CNABS)
C
C     CONVERT TO NORMAL FORCE COEFFICIENT
      CNBS = CNABS * ALPHA * PI / 180.0
      CNBS=CNBS*PI*MISDIA**2/4./SREF
C
C     CALCULATE NORMAL FORCE COEFFICIENT INCREMENT
      BOTDCN = CNBS - CNBU
C
C     CALCULATE BOATTAIL CENTER OF PRESSURE FOR UNSEPARATED CASE.
C     CENTER OF PRESSURE IS REFERENCED TO LONGITUDINAL C.G. LOCATION
      DVOLU = VOL1U  - VOL2U
      DCSAU = CSA1   - CSA2U
      XCPU  = (XB+BOTLEN-(XCG-XO)-DVOLU/DCSAU)/LREF
C
C     CALCULATE BOATTAIL CENTER OF PRESSURE FOR SEPARATED CASE
      IF (XSEPA0 .LT. BOTLEN) THEN
C
C        NOMINAL BOATTAIL SEPARATION
         DVOLS = VOL1S - VOL2S
         DCSAS = CSA1  - CSA2S
         XCPS  = (XB+BOTLS-(XCG-XO)-DVOLS/DCSAS)/LREF
C
      ELSE
C
C        ENTIRE BOATTAIL IS SEPARATED
         XCPS = 0.0
C
      END IF
C
C     CALCULATE PITCHING MOMENT FOR UNSEPARATED BOATTAIL
      CMBU = -CNBU * XCPU
C
C     CALCULATE PITCHING MOMENT FOR SEPARATED BOATTAIL
      CMBS = -CNBS * XCPS
C
C     CALCULATE PITCHING MOMENT COEFFICIENT INCREMENT
      BOTDCM = CMBS - CMBU 
C
  100 CONTINUE
      RETURN
      END
