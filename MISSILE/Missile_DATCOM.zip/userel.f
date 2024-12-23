      SUBROUTINE USEREL(REQ)
C
C***  SUBROUTINE TO COMPUTE GEOMETRY CHARACTERISTICS FOR USER
C***  DEFINED SHAPES OF ELLIPTIC CROSS SECTION
C
      COMMON /CONST/ PI,RAD,UNUSED,KAND
C
      COMMON /ABODIN/ NX,XO,X(50),WIDTH(50),TNOSE,LNOSE,WNOSE,BNOSE,
     1                TRUNC,LCENTR,WCENTR,TAFT,LAFT,WAFT,
     2                POWER,DISCON(20),ELLIP(50),HEIGHT(50),ENOSE,
     3                ECENTR,EAFT,ADUM(645)
      COMMON /XRBLNT/ XTEMP(50),RTEMP(50),NXTEMP,VNTEMP,SWTEMP,ITYPE
      REAL NXTEMP
      REAL NX,LNOSE,LCENTR,LAFT,LTOTAL
C
      COMMON /GEOBOD/ SPNOSE,SWNOSE,VOLNOS,XCVNOS,XCPNOS,FRNOSE,
     1  THEOLN,THEOFN,SPCENT,SWCENT,VOLCEN,XCVCEN,XCPCEN,FRCENT,
     2  THEOLC,THEOFC,SPAFT ,SWAFT ,VOLAFT,XCVAFT,XCPAFT,FRAFT ,
     3  THEOLA,THEOFA,SPLAN ,SWET  ,VOL   ,XCENTV,XCENTP,FR    ,
     4                DBASE ,SBASE ,DMAX  ,SMAX  ,LTOTAL,
     5  BPNOSE,BCPNOS,BPCENT,BCPCEN,BPAFT,BCPAFT,BPLAN,
     6  BCENTP,ECSPN,ECSPC,ECSPA,ECSPT
C
      COMMON /THERY/ LSOSE,PRESUR,LHYBRD,LHYPER
C
      LOGICAL LSOSE,TRUNC
C
      DIMENSION REQ(50),DUM(8),DUMMY(20)
C
      LSOSE=.TRUE.
C
      NNX=NX+0.5
C
      DO 1000 I=1,NNX
         X(I)=X(I)-XO
         REQ(I)=SQRT(WIDTH(I)*HEIGHT(I))
 1000 CONTINUE
C
      LTOTAL=X(NNX)
C
      SPLAN=DSPLAN(NNX,X,WIDTH)
      BPLAN=DSPLAN(NNX,X,HEIGHT)
      ECSPT=DSPLAN(NNX,X,REQ)
      SWET=DSWETE(NNX,X,WIDTH,HEIGHT)
      VOL=DVOLE(NNX,X,WIDTH,HEIGHT)
      XCENTV=DXCNVE(NNX,X,WIDTH,HEIGHT)
      XCENTP=DXCENP(NNX,X,WIDTH)
      BCENTP=DXCENP(NNX,X,HEIGHT)
C
C  CHECK IF BODY LENGTHS ARE DEFINED
C
      INOSE=NNX
      ICENTR=NNX
      IF(LNOSE .EQ. UNUSED)GO TO 1020
      INOSE=1
      ICENTR=NNX
      DO 1010 I=2,NNX
         IF(LNOSE .EQ. X(I))INOSE=I-1
         IF(LCENTR .EQ. X(I))ICENTR=I-1
 1010 CONTINUE
C
      IF(LNOSE .GT. UNUSED)GO TO 1040
C
C  DEFINE NOSE LENGTH
C
 1020 INOSE=1
      DO 1030 I=2,NNX
         IF(REQ(I) .LE. REQ(I-1))GO TO 1040
         INOSE=I
 1030 CONTINUE
C
 1040 CONTINUE
C
      IF(INOSE .LE. 1)GO TO 1050
      LNOSE=X(INOSE)
      WNOSE=2.*WIDTH(INOSE)
      HNOSE=2.*HEIGHT(INOSE)
C
      IF(INOSE .EQ. NNX)GO TO 1090
C
C  DEFINE CENTER-BODY
C
 1050 INP1=INOSE+1
      ICENTR=INOSE
      IF(LCENTR .GT. UNUSED)GO TO 1070
      ICENTR=NNX
      IF(ABS(REQ(NNX)-REQ(INP1))/REQ(INP1) .LE. 0.01)GO TO 1070
      ICENTR=INOSE
      DO 1060 I=INP1,NNX
         IF(ABS(REQ(I)-REQ(I-1))/REQ(I) .GT. 0.01)GO TO 1070
         ICENTR=I
 1060 CONTINUE
C
 1070 CONTINUE
C
      IF(ICENTR .EQ. INOSE)GO TO 1080
      LCENTR=X(ICENTR)-X(INOSE)
      WCENTR=2.*WIDTH(ICENTR)
      HCENTR=2.*HEIGHT(ICENTR)
C
      IF(ICENTR .EQ. NNX)GO TO 1090
C
C  DEFINE AFT BODY
C
 1080 LAFT=X(NNX)-LNOSE-LCENTR
      WAFT=2.*WIDTH(NNX)
      HAFT=2.*HEIGHT(NNX)
C
 1090 IF(LNOSE .LE. UNUSED)GO TO 1100
      IF (INOSE.LE.1) GO TO 1100
C
C***  DEFINE THE GEOMETRY FOR AN EQUIVALENT CIRCULAR BODY
C***  WITH A SHARP NOSE TO BE USED IN THE TRANSONIC
C***  WAVE DRAG CALCULATION. THIS DATA IS STORED IN
C***  XRBLNT COMMON BLOCK.
      IF (BNOSE.GT.UNUSED) THEN
        NXTEMP=UNUSED
        CALL CONIC(DUM,NXTEMP,XTEMP,RTEMP,UNUSED,.FALSE.,
     &             LNOSE,REQ(INOSE),DUMMY)
        VNTEMP=DUM(3)
        DO 1095 I=INOSE+1,NNX
          NXTEMP=NXTEMP+1.0
          NTMP=NXTEMP+0.5
          XTEMP(NTMP)=X(I)
          RTEMP(NTMP)=REQ(I)
 1095   CONTINUE
C
C***    TOTAL WETTED AREA FOR SHARP NOSE
        SWTEMP=DSWET(NTMP,XTEMP,RTEMP)
      ENDIF
C
      SPNOSE=DSPLAN(INOSE,X,WIDTH)
      BPNOSE=DSPLAN(INOSE,X,HEIGHT)
      ECSPN=DSPLAN(INOSE,X,REQ)
      SWNOSE=DSWETE(INOSE,X,WIDTH,HEIGHT)
      VOLNOS=DVOLE(INOSE,X,WIDTH,HEIGHT)
      XCVNOS=DXCNVE(INOSE,X,WIDTH,HEIGHT)
      XCPNOS=DXCENP(INOSE,X,WIDTH)
      BCPNOS=DXCENP(INOSE,X,HEIGHT)
      THEOLN=LNOSE
C
 1100 IF(LCENTR .LE. UNUSED)GO TO 1110
C
      SPCENT=DSPLAN(ICENTR,X,WIDTH)-SPNOSE
      BPCENT=DSPLAN(ICENTR,X,HEIGHT)-BPNOSE
      ECSPC=DSPLAN(ICENTR,X,REQ)-ECSPN
      SWCENT=DSWETE(ICENTR,X,WIDTH,HEIGHT)-SWNOSE
      VOLCEN=DVOLE(ICENTR,X,WIDTH,HEIGHT)-VOLNOS
      XCVCEN=DXCNVE(ICENTR,X,WIDTH,HEIGHT)-XCVNOS
      XCPCEN=DXCENP(ICENTR,X,WIDTH)-XCPNOS
      BCPCEN=DXCENP(ICENTR,X,HEIGHT)-BCPNOS
      THEOLC=LCENTR
C
 1110 IF(LAFT .LE. UNUSED)GO TO 1120
C
      SPAFT=DSPLAN(NNX,X,WIDTH)-SPNOSE-SPCENT
      BPAFT=DSPLAN(NNX,X,HEIGHT)-BPNOSE-BPCENT
      ECSPA=DSPLAN(NNX,X,REQ)-ECSPN-ECSPC
      SWAFT=DSWETE(NNX,X,WIDTH,HEIGHT)-SWNOSE-SWCENT
      VOLAFT=DVOLE(NNX,X,WIDTH,HEIGHT)-VOLNOS-VOLCEN
      XCVAFT=DXCNVE(NNX,X,WIDTH,HEIGHT)-XCVNOS-XCVCEN
      XCPAFT=DXCENP(NNX,X,WIDTH)-XCPNOS-XCPCEN
      BCPAFT=DXCENP(NNX,X,HEIGHT)-BCPNOS-BCPCEN
      THEOLA=LAFT
C
 1120 CONTINUE
C
      DO 1130 I=1,NNX
         X(I)=X(I)+XO
 1130 CONTINUE
C
      DO 1140 I=1,20
         II=I
         IF(DISCON(I) .LE. UNUSED)GO TO 1150
 1140 CONTINUE
C
 1150 IF(II .GT. 1 .AND. DISCON(II-1) .NE. NX)DISCON(II)=NX
      IF(II .EQ. 1)DISCON(II)=NX
C
      IF(LNOSE .GT. UNUSED .AND. TNOSE .EQ. UNUSED)TNOSE=1.
      IF(LNOSE .GT. UNUSED .AND. TNOSE .GT. 4.)TNOSE=0.
      IF(TAFT .EQ. UNUSED .AND. LAFT .GT. UNUSED)TAFT=0.
      IF(TAFT .GT. 1. .AND. LAFT .GT. UNUSED)TAFT=0.
C
      RETURN
      END
