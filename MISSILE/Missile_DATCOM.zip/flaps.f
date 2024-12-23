      SUBROUTINE FLAPS(XMACH,CFOC,TC,DELTA,CHORDS,XLAMLE,
     1           XLAMTE,YU,SSPAN,CNALPA,CAZERO,SREF,DELFF,CLD)
C      
C   THIS IS A SUBROUTINE TO DETERMINE THE EQUIVALENT ALL MOVING FIN DELTA
C   PER FLAP DEFLECTION.  THE APPROACH IS BASED ON THE DATCOM HANDBOOK
C   FOR PLAIN TRAILING EDGE, FULL SPAN FLAPS AT SUBSONIC AND SUPERSONIC
C   SPEEDS.  THE EQUIVALENT ALL MOVING FIN DEFLECTION IS THEN INCLUDED IN THE 
C   EQUIVALENT ANGLE OF ATTACK FORMULATION CURRENTLY IN MISSILE DATCOM.
C
C  WRITTEN BY K. BURNS, MCDONNELL DOUGLAS
C  MODIFIED BY W. BLAKE, WL/FIGC
C      
C *** INPUTS
C  	XMACH  - MACH NUMBER
C       CFOC   - (FLAP CHORD/FIN CHORD)station i
C       TC     - EFFECTIVE THICKNESS TO CHORD RATIO
C  	SSPAN  - (SEMI-SPAN)station i
C       DELTA  - $DEFLCT FLAP DEFLECTION, DEGREES
C       CHORDS - FIN (CHORD)station i
C       XLAMLE - EFFECTIVE LEADING EDGE SWEEP, DEGREES
C       XLAMTE - EFFECTIVE TRAILING EDGE SWEEP, DEGREES
C       CNALPA - NORMAL FORCE CURVE SLOPE, PER DEG (SINGLE PANEL)
C       CAZERO - PANEL ZERO ALPHA AXIAL FORCE
C       SREF   - MISSILE REFERENCE AREA
C      
C *** OUTPUT
C  	DELFF - EFFECTIVE ALL MOVING FIN DEFLECTION ANGLE WHICH CORRESPONDS
C   	        INPUT FLAP DEFLECTION, DEGREES
C       CLDEL - FLAP EFFECTIVENESS DERIVATIVE, CL-DELTA
C      
      COMMON /CONST/ PIE,RAD,UNUSED,KAND
C      
C * DATCOM FIGURE 6.1.4.1-14
C      
      DIMENSION X11418(10),X21418(10),Y61418(100)
C      
C * DATCOM FIGURE 6.1.4.1-15
C      
      DIMENSION X11419(3),X21419(12),Y61419(36)
C
C * DATCOM FIGURE 6.1.1.1-40 CFOC=.25
C
      DIMENSION X11140(14),Y11140(14)
C      
C * MISC ARRAY DIMENSIONS
C      
      DIMENSION CFOC(10)   ,ALPHAD(10) ,ALDAVG(9),
     1          ADCADS(9)  ,DKB(10)    ,CHORDS(10),
     2          YU(50)     ,SSPAN(10)  ,LAMDA(10) ,RKB(10)
C      
C * REALS
C      
      REAL LAMDA ,LAMDAF, MU
C      
C * LOGICALS
C      
      LOGICAL LTRANS
C      
C * DATCOM FIGURE 6.1.4.1-14A
C      
      DATA X21418/
     1 0.0,    0.25,   0.50,   1.0,    2.0,    3.0,    4.0,    5.0,
     2 6.0,    8.0/
      DATA X11418/
     1 -1.0,   -.9,    -.8,    -.7,    -.6,    -.5,    -.4,    -.3,
     2 -.2,    -.1/
      DATA Y61418/10*1.,
     1 1.11,   1.075,  1.06,   1.049,  1.03,   1.019,  1.011,  1.01,
     2 1.009,  1.007,
     3 1.2,    1.15,   1.125,  1.095,  1.06,   1.04,   1.03,   1.02,
     4 1.019,  1.015,
     5 1.365,  1.245,  1.20,   1.145,  1.095,  1.065,  1.05,   1.04,
     6 1.032,  1.025,
     7 1.5,    1.35,   1.285,  1.205,  1.132,  1.095,  1.072,  1.06,
     8 1.05,   1.039,
     9 1.7,    1.5,    1.4,    1.29,   1.185,  1.135,  1.10,   1.085,
     A 1.071,  1.055,
     B 2.0,    1.63,   1.53,   1.39,   1.25,   1.185,  1.145,  1.118,
     C 1.1,    1.079,
     D 2.4,    1.90,   1.70,   1.52,   1.35,   1.26,   1.20,   1.165,
     E 1.142,  1.11,
     F 2.73,   2.43,   2.10,   1.73,   1.49,   1.375,  1.30,   1.24,
     G 1.2,    1.16,
     H 3.08,   2.85,   2.55,   2.18,   1.79,   1.58,   1.46,   1.385,
     I 1.325,  1.245/
C      
C * DATCOM FIGURE 6.1.4.1-15
C      
      DATA X21419/
     1 0.0,    0.1,    0.2,    0.3,    0.4,    0.5,    0.6,    0.7,
     2 0.8,    0.85,   0.9,    1.0/
      DATA X11419/
     1 0.0,    0.5,    1.0/
      DATA Y61419/
     1 0.0,    0.160,  0.305,  0.440,  0.560,  0.670,  0.772,  0.860,
     2 .930,   .96,    .981,   1.0,
     3 0.0,    0.140,  0.270,  0.400,  0.515,  0.630,  0.735,  0.830,
     4 0.912,  0.947,  0.972,  1.0,
     5 0.,     .125,   .255,   .37,    .49,    .6,     .705,   .8,
     6 0.885,  0.921,  0.955,  1.0/
C
C * DATCOM FIGURE 6.1.1.1-40 CFOC=.25
C
      DATA X11140/
     1 0.,    10.,   12.,   14.,   16.,   18.,   20.,
     2 23.,   27.,   30.,   35.,   40.,   50.,   60./
      DATA Y11140/
     1 1.000, 1.000, 0.994, 0.989, 0.965, 0.935, 0.850,
     2 0.740, 0.677, 0.644, 0.600, 0.569, 0.518, 0.480/
C      
C *** TAKING CARE OF BUSINESS
C      
      DELFF = 0.0
      LTRANS= .FALSE.
      XLE   = XLAMLE/RAD
      TANPHE = (YU(45) - YU(49))/2./0.09
      ICNTR = 0
C      
C     DETERMINE THE NUMBER OF SPAN STATIONS INPUT
C      
      DO 1000 I=1,10
        IF(SSPAN(I) .NE. UNUSED)NSTA = I
 1000 CONTINUE
      BOVER2 = SSPAN(NSTA) - SSPAN(1)
C      
C     DETERMINE THE NUMBER OF CONTROL SURFACE SEGMENTS(NSEG),
C     THE STARTING AND ENDING SPAN STATION INDEX FOR THE FLAP
C     (IBEGIN AND IEND)
C      
      NSEG   = 0
      IBEGIN = 0
      IEND   = 0
      DO 1010 I=1,NSTA
        IF(CFOC(I) .GT. UNUSED)NSEG = NSEG + 1
        IF(CFOC(I) .GT. UNUSED .AND. IBEGIN .EQ. 0)IBEGIN=I
        IF(CFOC(I) .GT. UNUSED .AND. IBEGIN .NE. 0)IEND = I
        LAMDA(I) = CHORDS(I)/CHORDS(1)
 1010 CONTINUE
      NSEG = NSEG - 1
C
C     DETERMINE SINGLE PANEL PLANFORM AREA
C
      SFIN = 0.0
      DO 1015 ISTA=2,NSTA
        SFIN = SFIN + 0.5*(CHORDS(ISTA) + CHORDS(ISTA-1))
     1         *(SSPAN(ISTA) - SSPAN(ISTA-1))
 1015 CONTINUE
      AW = 2.0*BOVER2**2/SFIN
C      
C     CALCULATE FLAP AREA
C      
      IBP1 = IBEGIN +1
      SF = 0.0
      DO 1020 ISTA=IBP1,IEND
        CROOT = CHORDS(ISTA-1)*CFOC(ISTA-1)
        CTIP  = CHORDS(ISTA)*CFOC(ISTA)
        DSPAN = SSPAN(ISTA) - SSPAN(ISTA-1)
        SF = SF + 0.5*(CROOT + CTIP)*DSPAN
 1020 CONTINUE
C      
C *** TRANSONIC FLAG
C     TRANSONIC DEFINED TO BE .GT. MACH 0.8 BUT .LT. MACH 1.4
C      
      IF(XMACH .GT. 0.8 .AND. XMACH .LT. 1.4)THEN
         LTRANS=.TRUE.
C      
C   STORE TRANSONIC MACH NUMBER
C      
         XTRANS = XMACH
      ENDIF
C      
C   TRANSONIC "LOOP" - DETERMINE SUBSONIC AND SUPERSONIC VALUE THAT DEFINE
C   TRANSONIC CUBIC SPLINE
C      
 1030 IF(LTRANS)THEN
         ICNTR=ICNTR+1
C      
C   CALCULATE EFFECTIVE FULL FIN DEFLECTION AT MACH 0.79, 0.80, 1.40, 1.41
C      
         IF(ICNTR.EQ.1)XMACH=0.79
         IF(ICNTR.EQ.2)THEN
            X1    = XMACH
            Y1    = DELFF
            DELFF = 0.0
            XMACH = 0.8
         ENDIF
         IF(ICNTR.EQ.3)THEN
            X2    = XMACH
            Y2    = DELFF
            DELFF = 0.0
            XMACH = 1.4
         ENDIF
         IF(ICNTR.EQ.4)THEN
            X3    = XMACH
            Y3    = DELFF
            DELFF = 0.0
            XMACH = 1.41
         ENDIF
      ENDIF
C      
C *** SUBSONIC CALCULATIONS
C      
      IF(XMACH.LE.0.8)THEN
C      
C        CALCULATE ALPHA PER DELTA
C  NOTE: THE SPAN FACTOR ETA MUST BE MEASURED RELATIVE TO SSPAN(1)=0
C        HOWEVER IF A BODY IS PRESENT SSPAN(1)=BODY WIDTH.  SUBTRACT
C        SSPAN(1) FROM SPAN DIMENSION
C      
         ETA = (SSPAN(IBEGIN) - SSPAN(1))/BOVER2
         CALL MVLOOK(1,3,12,36,1.0,X11419,X21419,Y61419,
     1                ETA,LAMDA(IBEGIN),1.0,RKB(1))
C
C  DATCOM FIGURES 6.1.4.1-14 (INSET ONLY) AND 6.1.1.1-39B
C
         CALL FLAP2(CFOC(IBEGIN),TANPHE,ALPHAD(1))
C      
C        FOR EACH SPAN STATION DEFINED DETERMINE THE 2-D ALPHA PER DELTA
C        AND USE AVERAGE AT EACH END OF A FLAP SEGMENT AS ALPHA PER DELTA
C        FOR THAT SEGMENT. INDICES IN THE FOLLOWING LOOP INDICATE:
C        ISTA = FIN SPAN STATION INDEX, FOR THE FIRST FLAP CHORD ISTA=IBEGIN
C        ISEG = THE FLAP SEGMENT INDEX
C        K    = FLAP CHORD INDEX
C        N    = PREVIOUS FLAP CHORD STATION
C      
         ISTA = IBEGIN
         ISEG = 0
         NSEGP1 = NSEG + 1
         CDF = 0.0
         DO 1040 K=2,NSEGP1
           ISTA = ISTA + 1
           ISEG = ISEG + 1
           N    = K - 1
           IF(CHORDS(ISTA) .EQ. 0.0)THEN
             CFC = 0.0
           ELSE
             CFC = CFOC(ISTA)
           ENDIF
C
C  DATCOM FIGURES 6.1.4.1-14 (INSET ONLY) AND 6.1.1.1-39B
C
           CALL FLAP2(CFC,TANPHE,ALPHAD(K))
C
           ALDAVG(ISEG) = 0.50*(ALPHAD(K)+ALPHAD(N))
           ETA          = (SSPAN(ISTA) - SSPAN(1))/BOVER2
C      
C          DATCOM FIGURE 6.1.4.1-15 (KB)
C      
           CALL MVLOOK(1,3,12,36,1.0,X11419,X21419,Y61419,
     1                ETA,LAMDA(ISTA),1.0,RKB(K))
C      
           DKB(ISEG)=RKB(K)-RKB(N)
 1040    CONTINUE
C      
C        DATCOM FIGURE 6.1.4.1-14
C        TABLE LOOKUP FOR ALPHA_DELTA_CL/ALPHA_DELTA_Cl, ADCADS(N)
C        ADCADS(N) = THREE DIMENSIONAL FLAP EFFECTIVENESS PARAMETER TO
C                    THE TWO DIMENSIONAL FLAP EFFECTIVENESS PARAMETER
C                    RATIO FOR EACH SEGMENT OF THE FLAP
C        ADR       = THE WEIGHTED AVERAGE OF EFFECTIVE DEFLECTION/FLAP
C                    DEFLECTION ANGLE
C      
         ADR = 0.0
         DO 1050 ISEG=1,NSEG
            CALL MVLOOK(1,10,10,100,1.,X11418,X21418,Y61418,
     1                  AW,ALDAVG(ISEG),1.0,ADCADS(ISEG))
            ADR = ADR + ALDAVG(ISEG)*ADCADS(ISEG)*DKB(ISEG)
 1050    CONTINUE
C      
C        CALCULATE DELTA_ALPHA_TOTAL, DELFF
C        DELFF = TOTAL EQUIVALENT FULL FIN DEFLECTION REQUIRED TO 
C                GENERATE THE SAME ADDITIONAL LIFT GENERATED BY THE 
C                DEFLECTION OF THE INDIVIDUAL FLAP SEGMENTS
C      
         DELFF = -DELTA*ADR
         CLD=-ADR*(CNALPA*RAD-CAZERO)*SREF/(2.*SFIN)
      ENDIF
C
C   CORRECT DELTA FOR NON-LINEAR EFFECTS OF LARGE FLAP DEFLECTION
C
      ADEL=ABS(DELTA)
C     CALL MVLOOK(1,1,14,14,1.0,1.0,X11140,11140,ADEL,1.,1.,DELR)
      CALL LNTRP(X11140,Y11140,14,ADEL,DELR)
      DELFF=DELFF*DELR
C      
C *** END SUBSONIC CALCULATIONS
C      
C***********************************************************************
C      
C   *** BEGIN SUPERSONIC CALCULATIONS (REFERENCE NACA TR 1041) ***
C      
C***********************************************************************
C      
      IF(XMACH.GE.1.4)THEN
         SRI   = 0.0
         SRIA  = 0.0
         SRIB  = 0.0
         SRII  = 0.0
         SRIII = 0.0
         SRO   = 0.0
         BETA=SQRT(XMACH*XMACH-1.)
C      
C        CALCULATE HINGE LINE SWEEP ANGLE
C      
         XLAMHL = SSPAN(IEND)*TAN(XLE)+CHORDS(IEND)*(1.-CFOC(IEND)) -
     1       (SSPAN(IBEGIN)*TAN(XLE)+CHORDS(IBEGIN)*(1.-CFOC(IBEGIN)))
         XLAMHL=XLAMHL/(SSPAN(IEND) - SSPAN(IBEGIN))
         XLAMHL=ATAN(XLAMHL)
C      
C        LEADING, TRAILING AND HINGE LINE SWEEP ANGLES MUST BE SUPERSONIC.
C        THEREFORE DETERMINE THE SWEEP REQUIRED FOR A SONIC LEADING EDGE SWEEP.
C        IF THE SWEEP IS GREATER THAN THE SWEEP OF THE MACH LINE, SET THE
C        SWEEP TO THE MACH LINE SWEEP MINUS 1 DEGREE.
C        NOTE: MACH LINE SWEEP, MU=ATAN(1/BETA), IS MEASURED FROM CENTERLINE
C        AND LEADING EDGE SWEEP IS MEASURED FROM Y-AXIS
C      
         MU = ATAN(1/BETA)
         SWMAX = PIE/2. - MU
C
C  REV 1/96  INCORRECT TEST REMOVED
C            ONE DEGREE OFFSETS ADDED TO MODIFIED T.E. AND H.L. SWEEP
C
C        IF(XLE .GT. SWMAX)XLE = SWMAX
         XTE = XLAMTE/RAD
         IF(XTE .GE. SWMAX)XTE = SWMAX - PIE/180.
         IF(XLAMHL .GE. SWMAX)XLAMHL = SWMAX - PIE/180.
C      
C        CALCULATE "RAY" A, THE RAY EXTENDS FROM THE ORIGIN OF THE SUPERSONIC
C        FLAP COORDINATE SYSTEM
C      
         A=TAN(XLAMHL)/BETA
C      
C        CALCULATE "RAY" D, THE RAY EXTENDS FROM THE ORIGIN OF THE SUPERSONIC
C        FLAP COORDINATE SYSTEM
C      
         D=TAN(XTE)/BETA
C      
C        CALCULATE FLAP TAPER RATIO (USES FLAP AREA AND ROOT CHORD TO DETERMINE
C        AN EFFECTIVE FLAP TAPER RATIO SINCE TRAILING EDGE MAY BE CRANKED)
C      
         CFROOT = CHORDS(IBEGIN)*CFOC(IBEGIN)
         SPANF  = SSPAN(IEND) - SSPAN(IBEGIN)
         LAMDAF = 2.*SF/CFROOT/SPANF - 1.
C      
C        CALCULATE THICKNESS CORRECTION FACTOR
C        REFERENCE DATCOM EQUATION 6.1.4.1-f
C      
         TANPHE = (YU(45) - YU(49))/2./0.09
         PHITE = 2.*ATAN(TANPHE)
         C1=2./SQRT(XMACH*XMACH-1.)
         C2=(1.4+1.)*XMACH**4-4.*(XMACH*XMACH-1.)
         C2=C2/(2.*(XMACH*XMACH-1.)**2)
         CORR=1. - C2/C1*PHITE
C      
C        CALCULATE TWO-DIMENSIONAL PRESSURE COEFFICIENT
C      
         CPO=2.*DELTA/(RAD*BETA*SQRT(ABS(1.-A*A)))
C	     
C *** HERE FOR FLAPS WITH TAPER RATIO .LT. 1 AND .GT. 0
C      
         IF(LAMDAF.LT.1.0 .AND. LAMDAF.GT.0.0)THEN
C      
C           CALCULATE AREA RATIO FOR REGION I
C
            SRI=2.*(A-D)/((1.-LAMDAF**2)*(1-D**2))
C      
C           CALCULATE AREA RATIO FOR REGION IA
C      
C           NOTE: FOR ALL AREA RATIO CALCULATIONS IN REGION IA AND IB,
C                 SET THE AREA=0 IF THE IT IS NEGATIVE.  A NEGATIVE AREA 
C                 RATIO INDICATES THAT THE REGION DOES NOT EXIST ON THIS 
C                 FLAP GEOMETRY.  ALSO WHEN AREA RATIO IS ZERO, BYPASS 
C                 PRESSURE RATIO CALCULATIONS FOR THAT REGION
C      
            SRIA=(LAMDAF*(1.-D)-(1.-A))/((1.-LAMDAF**2)*(1.-D))
            IF(SRIA .LT. UNUSED)SRIA=0.0
C      
C           CALCULATE AREA RATIO FOR REGION IB
C      
            SRIB=(LAMDAF*(1.-D)-(1.-A))/((1.+LAMDAF)*(A-D))
            IF(SRIB .LT. UNUSED)SRIB=0.0
C      
C           CALCULATE AREA RATIO FOR REGION II
C      
            SRII=2.*LAMDAF**2*(A-D)/((1.-LAMDAF**2)*(1.-D**2))
C      
C           CALCULATE AREA RATIO FOR REGION III
C      
            SRIII=(LAMDAF**2*(A-D))/((1.-LAMDAF**2)*(1.+D))
C      
C           CALCULATE AREA RATIO FOR TWO-DIMENSIONAL REGION
C      
            SRO=(1.-A)/(1.-D)-LAMDAF**2*(1.+A)/(1.+D)
            SRO=SRO/(1.-LAMDAF**2)
C      
C           CALCULATE AVERAGE PRESSURE RATIO IN REGION I
C      
            PI=SQRT((1-A**2)*(1.-D**2))-(1.-A)*(1+D)
            PI=PI/(2.*(A-D))
C      
C           CALCULATE AVERAGE PRESSURE RATIO IN REGION IA
C      
            IF(SRIA .GT. 0.0)THEN
              T1=(1.-D)/(LAMDAF*(1.-D)-(1.-A))
              T2=LAMDAF/PIE
              T3=((1.-A**2)-LAMDAF*(1.-A*D))/(LAMDAF*(A-D))
              T4=1./PIE*SQRT((1.-A**2)/(1.-D**2))
              T5=((1.-A*D)-LAMDAF*(1.-D**2))/(A-D)
              PIA=T1*(T2*ACOS(T3)-T4*ACOS(T5))
            ELSE
              PIA=0.0
            ENDIF
C      
C           CALCULATE AVERAGE PRESSURE RATIO IN REGION IB
C      
            IF(SRIB .GT. 0.0)THEN
               T1=1./(LAMDAF*(1.-D)-(1.-A))
               T2=T2*(A-D)
               T3=T3
               T4=(1.-LAMDAF)*SQRT(1.-A**2)/PIE
               IF((A-D).LT.0.)THEN
                  T5=(A-LAMDAF*D)+SQRT(2.*LAMDAF*(1.-A*D)-LAMDAF**2*
     1               (1.-D**2)-(1.-A*2.))
                  T5=T5/(1.-LAMDAF)
               ELSE
                  T5=(A-LAMDAF*D)-SQRT(2.*LAMDAF*(1.-A*D)-LAMDAF**2*
     1               (1.-D**2)-(1.-A*2.))
                  T5=T5/(1.-LAMDAF)
               ENDIF
               T6=LOG(ABS(T5))
               PIB=T1*(T2*ACOS(T3)+T4*T6)
            ELSE
               PIB=0.0
            ENDIF
C      
C           CALCULATE AVERAGE PRESSURE RATIO IN REGION II
C      
            PII=(SQRT((1.-A**2)*(1.-D**2)) - (1.+A)*(1.-D))/
     1          (2.*(D-A))
C      
C           CALCULATE AVERAGE PRESSURE RATIO IN REGION III
C      
            PIII=((1.+A)-SQRT((1.+A)*(1.+D)))/(A-D)
C      
C *** HERE IF FLAP IS UNTAPERED
C      
         ELSEIF(LAMDAF.GE.1.)THEN
C      
C           CALCULATE BETA*(ASPECT RATIO) OF UNTAPERED FLAP
C      
            AF=((SSPAN(IEND) - SSPAN(IBEGIN))**2)/SF*BETA
C      
C           CALCULATE AREA RATIO FOR REGION I
C      
            SRI=1./(AF*(1.-A*A))
C      
C           CALCULATE AREA RATIO FOR REGION IA
C      
            SRIA=(1.-AF*(1.-A))/(2.*AF*(1.-A))
            IF(SRIA .LT. UNUSED)SRIA = 0.0
C      
C           CALCULATE AREA RATIO FOR REGION IB
C      
            SRIB=(1.-AF*(1.-A))/2.
            IF(SRIB .LT. UNUSED)SRIB = 0.0
C      
C           CALCULATE AREA RATIO FOR REGION II
C      
            SRII=1./(AF*(1.-A**2))
C      
C           CALCULATE AREA RATIO FOR REGION III
C      
            SRIII=1./(2.*AF*(1.+A))
C      
C           CALCULATE AREA RATIO FOR TWO-DIMENSIONAL REGION
C      
            SRO=(AF*(1.-A*A)-1)/(AF*(1.-A*A))
C      
C           CALCULATE AVERAGE PRESSURE RATIO IN REGION I
C      
            PI=0.5
C      
C           CALCULATE AVERAGE PRESSURE RATIO IN REGION IA
C      
            IF(SRIA .GT. 0.0)THEN
              T1=1./((1.+A)*(1.-(1.-A)*AF))
              T2=SQRT((1.-A*A)*(1.+2.*A*AF-(1.-A*A)*AF*AF))/PIE
              T3=((1.-A*A)*AF-A)/PIE
              T4=(1.-A*A)*AF-A
              PIA=T1*(T2-T3*ACOS(T4))
            ELSE
              PIA=0.0
            ENDIF
C      
C           CALCULATE AVERAGE PRESSURE RATIO IN REGION IB
C      
            IF(SRIB .GT. 0.0)THEN
              T1=1./(1.-(1.-A)*AF)
              T2=AF*SQRT(1.-A*A)/PIE
              T3=1.+A*AF-SQRT(1.+2.*A*AF-(1.-A*A)*AF*AF)
              PIB=T1*(ACOS((1.-A*A)*AF-A)/PIE+T2*LOG(ABS(T3)))
            ELSE
              PIB=0.0
            ENDIF
C      
C          CALCULATE AVERAGE PRESSURE RATIO IN REGION II
C      
            PII=0.5
C      
C          CALCULATE AVERAGE PRESSURE RATIO IN REGION III
C      
            PIII=0.5
C      
C *** HERE FOR FLAP TAPER RATIO ZERO
C      
         ELSEIF(LAMDAF.EQ.0.)THEN
C      
C           CALCULATE AREA RATIO FOR TWO-DIMENSIONAL REGION
C      
            SRO=(1.-A)/(1.-D)-LAMDAF**2*(1.+A)/(1.+D)
            SRO=SRO/(1.-LAMDAF**2)
C      
C           CALCULATE AREA RATIO FOR REGION I
C      
            SRI=2.*(A-D)/((1.-LAMDAF**2)*(1-D**2))
C      
C           NOTE THAT THE AREA RATIO IS ZERO FOR REGION II
C           NO CONTRIBUTION FROM REGION II
C      
            SRII=0.0
C      
C           CALCULATE AVERAGE PRESSURE RATIO IN REGION I
C      
            PI=SQRT((1-A**2)*(1.-D**2))-(1.-A)*(1+D)
            PI=PI/(2.*(A-D))
C      
            PIA=0.
            PIB=0.
            PIII=0.
C      
         ENDIF
C      
C *** END TAPER LOGIC
C      
C *** CALCULATE THE LIFT COEFFICIENT PER DEGREE OF ELEVATOR DEFLECTION
C     IF IEND=NSTA THE CONTROL GOES TO THE FIN TIP
C     OTHERWISE USE METHOD FOR FLAP INBOARD OF FIN TIP
C      
         IF(IEND .EQ. NSTA)THEN
           PART = SRO + PI*SRI + PIA*SRIA - PIB*SRIB + PIII*SRIII
         ELSE
           PART = SRO + PI*SRI + PII*SRII
         ENDIF
         CLDEL = 2.*CPO*PART/DELTA
C      
C        CORRECT FOR FLAP THICKNESS, REFERENCE AREAS
C      
         CLDEL=CLDEL*CORR*SF/SREF
C      
C *** CALCULATE THE EFFECTIVE FULL FIN DEFLECTION ANGLE
C     CNALPA = NORMAL FORCE CURVE SLOPE (INPUT PER DEG.,SINGLE PANEL)
C     CAZERO = ZERO LIFT AXIAL FORCE
C     CL_ALPHA, LIFT CURVE SLOPE = CNALPA - CAZERO
C      
         DELFF=CLDEL*DELTA/(CNALPA*RAD-CAZERO)
         CLD=CLDEL
C      
         DELFF=DELFF*RAD
C      
C *** END SUPERSONIC CALCULATIONS
C      
      ENDIF
C      
C *** TRANSONIC LOOP
C      
      IF(LTRANS .AND. ICNTR.LT.4)GOTO 1030
C      
C   TRANSONIC CUBIC SLPINE - ESTIMATE THE EFFECTIVE FULL FIN DEFLECTION
C   THE SPECIFIED TRANSONIC MACH NUMBER
C      
      IF(LTRANS)THEN
          X4=XMACH
          Y4=DELFF
C      
C   ESTIMATE DERIVATIVES AT ENDPOINTS
C   BACKWARDS FINITE DIFFERENCE LENGTH IS 0.1, DELTA X
C      
          DY1=(Y2-Y1)/0.1
C      
C  FORWARDS FINITE DIFFERENCE LENGTH IS 0.1, DELTA X
C      
          DY2=(Y4-Y3)/0.1
C      
C   CALL SUBROUTINE CUBIC TO FAIR BETWEEN MACH 0.8 AND MACH 1.4 USING A
C   CUBIC SPLINE
C      
          CALL CUBIC(XTRANS,X2,X3,Y2,Y3,DY1,DY2,DELFF)
C      
          XMACH=XTRANS
      ENDIF
C      
      RETURN
      END
