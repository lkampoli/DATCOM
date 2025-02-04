      SUBROUTINE KWBNEW(MACH,ALPHA,RADF,SSPAN,KWBSBT,FACTOR)
C
C***  MODIFIED AUGUST 8, 1996
C
C***  SUBROUTINE TO DETERMINE THE RATIO K-W(B)/K-W(B)SBT
C***  DUE TO ANGLE OF ATTACK
C
C***  THIS METHOD IS AN EMPIRICAL CORRELATION OF TEST DATA
C***  DERIVED FROM THE MDA/NASA FIN-BODY WIND TUNNEL TEST DATA.
C
C***  INPUT
C
C  MACH   - MACH NUMBER
C  ALPHA  - ANGLE OF ATTACK, DEG.
C  RADF   - BODY RADIUS AT FIN STATION
C  KWBSBT - SLENDER BODY THEORY K-W(B)
C  SSPAN  - FIN SEMI-SPAN (EXCLUDES BODY)
C
C***  OUTPUT
C
C  FACTOR - RATIO OF K-W(B) TO THAT FROM THEORY (K-W(B)SBT)
C
       DIMENSION MACHTAB(10),AOASBT(10),RSTAB(5),DELTAB(5)
       REAL MACH,MACHTAB,KWBSBT,KWB
C
C***   PREVIOUS ALPHA CORRECTION FACTORS ************************
C
C       DATA ALPTAB / 0.,2.,4.,6.,8.,10.,12.,14.,16.,18.,20.,
C     1              22.,24.,90.,156.,158.,160.,162.,164.,166.,
C     2              168.,170.,172.,174.,176.,178.,180./
C
C       DATA KWBTAB/  1.0,1.0,1.0,0.985,0.940,0.875,0.825,
C     1              0.790,0.770,0.770,0.770,0.770,0.770,
C     2              0.770,0.770,0.770,0.770,0.770,0.770,
C     3              0.790,0.825,0.875,0.950,0.985,1.0,1.0,1.0/
C      DATA KWBTAB / 27*1.0 /
C
C*****************************************************************
C
      DATA MACHTAB / 0.0,0.6,0.9,1.2,1.6,2.0,2.3,2.96,3.95,5.0 /
      DATA AOASBT / 11.169,11.169,11.285,9.848,8.083,8.825,8.285,
     &              7.681,7.889,7.889 /
      DATA RSTAB / 0.0,0.25,0.3333,0.5,1.0 /
      DATA DELTAB / 0.0,0.094,0.103,0.099,0.0 /
C      
      RS=RADF/(SSPAN+RADF)
C
      IF ((RS.GT.0.0).AND.(RS.LT.1.0)) THEN
C
C***    FIND AOA WHERE KWB=KWB(SBT)
        CALL LNTRP(MACHTAB,AOASBT,10,MACH,ASBT)
C***    FIND KWB(0)-KWB(SBT)
        CALL LNTRP(RSTAB,DELTAB,5,RS,DELTA)
C***    CALCULATE COEFFICIENT A
        A=DELTA+KWBSBT-1.0
C***    CALCULATE COEFFICIENT B
        B=(1.0/ASBT**1.5)*LOG(DELTA/(KWBSBT-1.0)+1.0)
C
C***    CALCULATE KWB(ALPHA)
        KWB=A*EXP(-B*ABS(ALPHA)**1.5)+1.0
C
C***    CALCULATE RATIO OF KWB/KWBSBT
        FACTOR=KWB/KWBSBT
C
      ELSE
C
        FACTOR=1.0
C
      ENDIF   
C
      RETURN
      END
