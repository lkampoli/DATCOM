      SUBROUTINE CP3DW(XP, YP, BB, XM, CP, FAIL)
C      
C     THIS SUBROUTINE CALCULATES THE PRESSURE COEFFICIENT ON
C     A 3-DIMENSIONAL FIN AT POINT XP, YP.  THE METHOD IS
C     DESCRIBED IN NWL TR-3018, PAGES 10 THRU 16.  THE INDUCED
C     VELOCITY (D(PHI)/DX) FROM EACH SOURCE OR SINK LINE (SOSL)
C     IS CALCULATED AT THE POINT AND SUMMED.  THE PRESSURE
C     COEFFICIENT IS THEN CALCULATED FROM THE POTENTIAL VELOCITY.
C      
C     INPUT VARIABLES
C         XP - X-LOCATION OF POINT
C         YP - Y-LOCATION OF POINT
C         BB - SPAN OF FIN
C         XM - MACH NUMBER
C      
C     INPUT VARIABLES FROM COMMON
C         XO   - X-LOCATION AT ROOT OF SOSL
C         XT   - X-LOCATION AT TIP OF SOSL
C         TGA  - TANGENT OF SOSL SWEEP
C         ETA  - TGA/SQRT(XM**2-1)
C         DZDX - SLOPE FWD OF SOSL AT YP
C         NS   - NUMBER OF SOSL
C      
C     OUTPUT
C         CP - PRESSURE COEFFICIENT
C      
      LOGICAL FAIL
      COMMON /CONST/ PI,RAD,UNUSED,KAND
      COMMON /CAFD/  XO(60), XT(60), DZDXO(60), DZDXT(60), TGA(60),
     1               ETA(60), DZDX(60), NS, CPO
C      
C     STATEMENT FUNCTION FOR ARCOSH(X)
C      
      ARCOSH(X) = ALOG(X+SQRT(X*X-1.0))
C      
      BETA = SQRT(XM*XM-1.0)
      B2 = BB/2.0
      PHEX = 0.0
C      
C     LOOP ON SOSL, J = SOSL INDEX
C      
      DO 1080 J=1,NS
C       ...
C       ... CALCULATE SIG AT ROOT AND TIP (NSWC TR 3018,EQ 7A)
C       ...
        XXP = XP-XO(J)
        SIGR = 1.0
        IF(XXP .GE. 1.E-6) SIGR = TGA(J)*YP/XXP
        XXP = XP-XT(J)
        SIGT = 1.0
        IF(XXP .GE. 1.E-6) SIGT = TGA(J)*(B2-YP)/XXP
        XMA = XO(J)+BETA*YP
        XGA = XO(J)+YP*TGA(J)
        XMT = XT(J)+BETA*(B2-YP)
C       ...
C       ... SUBSONIC SOSL
C       ...
        IF(XMA .GE. XGA) GO TO 1030
C         ...
C         ... REGION 1 - MACH LINE TO SOSL
C         ...
          IF(XP .LT. XMA) GO TO 1000
          IF(XP .GE. (XGA-0.0001)) GO TO 1000
          IF(SIGR .LT. 1.00001) SIGR = 1.00001
          IF(ETA(J)**2 .LT. 1.)FAIL=.TRUE.
          IF(FAIL)GO TO 1090
            A = PI*BETA*SQRT(ETA(J)**2-1.0)
            B = SQRT((ETA(J)**2-1.0)/(SIGR**2-1.0))
            PHEX = PHEX-2.0*(DZDX(J+1)-DZDX(J))*ARCOSH(B)/A
 1000     CONTINUE
C         ...
C         ... REGION 2 - DOWNSTREAM OF SOSL
C         ...
          IF(XP .LT. (XGA-0.0001)) GO TO 1010
            IF(SIGR .GT. 0.99999) SIGR = 0.99999
            IF(ETA(J)**2 .LT. 1.)FAIL=.TRUE.
            IF(FAIL)GO TO 1090
            A = PI*BETA*SQRT(ETA(J)**2-1.0)
            B = SQRT((ETA(J)**2-SIGR**2)/(1.0-SIGR**2))
            PHEX = PHEX-2.0*(DZDX(J+1)-DZDX(J))*ARCOSH(B)/A
 1010     CONTINUE
C         ...
C         ... REGION 3 - DOWNSTREAM OF MACH LINE FROM TIP
C         ...
          IF(XP .LT. XMT) GO TO 1020
          IF(ETA(J)**2 .LT. 1.)FAIL=.TRUE.
          IF(FAIL)GO TO 1090
            A = PI*BETA*SQRT(ETA(J)**2-1.0)
            B = (ETA(J)**2+SIGT)/(ETA(J)*(SIGT+1.0))
            IF(B .LT. 1.00001) B = 1.00001
            PHEX = PHEX+(DZDX(J+1)-DZDX(J))*ARCOSH(B)/A
 1020     CONTINUE
 1030   CONTINUE
C       ...
C       ... SUPERSONIC SOSL
C       ...
        IF(XMA .LT. XGA) GO TO 1070
C         ...
C         ... REGION 1 - SOSL TO MACH LINE
C         ...
          IF(XP .LT. (XGA-0.0001)) GO TO 1040
          IF(XP .GE. XMA) GO TO 1040
          IF(ETA(J)**2 .GT. 1.)FAIL=.TRUE.
          IF(FAIL)GO TO 1090
            A = BETA*SQRT(1.0-ETA(J)**2)
            PHEX = PHEX-(DZDX(J+1)-DZDX(J))/A
 1040     CONTINUE
C         ...
C         ... REGION 2 - DOWNSTREAM OF MACH LINE
C         ...
          IF(XP .LT. XMA) GO TO 1050
            IF(SIGR .GT. 0.99999) SIGR = 0.99999
            IF(ETA(J)**2 .GT. 1.)FAIL=.TRUE.
            IF(FAIL)GO TO 1090
            A = PI*BETA*SQRT(1.0-ETA(J)**2)
            IF(ETA(J)**2 .LT. SIGR**2)FAIL=.TRUE.
            IF(FAIL)GO TO 1090
            B = SQRT((ETA(J)**2-SIGR**2)/(1.0-SIGR**2))
            PHEX = PHEX-(DZDX(J+1)-DZDX(J))*(PI-2.0*ASIN(B))/A
 1050     CONTINUE
C         ...
C         ... REGION 3 - DOWNSTREAM OF MACH LINE FROM TIP
C         ...
          IF(XP .LT. XMT) GO TO 1060
          IF(ETA(J)**2 .GT. 1.)FAIL=.TRUE.
          IF(FAIL)GO TO 1090
            A = PI*BETA*SQRT(1.0-ETA(J)**2)
            B = (ETA(J)**2+SIGT)/(ETA(J)*(1.0+SIGT))
            IF(B .GE. 0.99999) B = 0.99999
            PHEX = PHEX+(DZDX(J+1)-DZDX(J))*ACOS(B)/A
 1060     CONTINUE
 1070   CONTINUE
 1080 CONTINUE
C     ...
C     ... CALCULATE CP
C     ...
      CP = -2.0*PHEX
      IF(CP .LT. -CPO) CP = -CPO
      IF(CP .GT. CPO)  CP = CPO
C      
 1090 CONTINUE
C      
      RETURN
      END
