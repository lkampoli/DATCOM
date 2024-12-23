      SUBROUTINE FGEOM
C      
C***  SUBROUTINE EXECUTIVE FOR FIN GEOMETRY
C      
      COMMON /CONST/  PI,RAD,UNUSED,KAND
      COMMON /DESIG/  NACA(80,4)
      COMMON /GEOFS1/ GFIN1(188)
      COMMON /GEOFS2/ GFIN2(188)
      COMMON /GEOFS3/ GFIN3(188)
      COMMON /GEOFS4/ GFIN4(188)
      COMMON /FSET1/  FDATA1(399)
      COMMON /FSET2/  FDATA2(399)
      COMMON /FSET3/  FDATA3(399)
      COMMON /FSET4/  FDATA4(399)
      COMMON /F1WORK/ F1WK(290)
      COMMON /F2WORK/ F2WK(290)
      COMMON /F3WORK/ F3WK(290)
      COMMON /F4WORK/ F4WK(290)
      COMMON /LOGIC/ LDMPCS,LDAMP,LBUILD,LNACA,LDERDG,
     1               LDERRD,LPART,LNAME,LPLOT,
     2               LFLT,LREFQ,LAXIS,LFIN1,LFIN2,LFIN3,LFIN4,
     3               LDEFL,LTRIM,LDIMIN,LDIMFT,LDIMCM,LDIMM,
     4               LELLB,LINLET,LEXPR,LICRMT,LSPIN,LARBOD
      LOGICAL        LDMPCS,LDAMP,LBUILD,LNACA,LDERDG,
     1               LDERRD,LPART,LNAME,LPLOT,
     2               LFLT,LREFQ,LAXIS,LFIN1,LFIN2,LFIN3,LFIN4,
     3               LDEFL,LTRIM,LDIMIN,LDIMFT,LDIMCM,LDIMM
      CHARACTER*4 NACA
      LOGICAL LLER1,LLER2,LLER3,LLER4
C      
      LLER1=.FALSE.
      LLER2=.FALSE.
      LLER3=.FALSE.
      LLER4=.FALSE.
C      
C***  FIN SET NUMBER 1
C      
      IF(.NOT. LFIN1)GO TO 1060
C      
C   IF USER HAS INPUT LEADING EDGE RADII SET LLER1 TO TRUE
C      
      IF(FDATA1(364) .NE. UNUSED)LLER1=.TRUE.
C      
C  PLANFORM GEOMETRY CONSTANTS
C      
      CALL F1GEOM
C      
C  DEFINE AIRFOIL SECTION IF NECESSARY
C      
      CALL FOIL(1,FDATA1(1),GFIN1(178),FDATA1(2),FDATA1(12),
     1          FDATA1(22),FDATA1(62),FDATA1(364),F1WK(1),LNACA,
     2          NACA(1,1))
C      
C  SET UP SECTION IF NOT USER DEFINED COORDINATES
C      
      IF(FDATA1(1) .LT. 3.)CALL AIRFOL(1,NACA(1,1),FDATA1(113),
     1      FDATA1(263),FDATA1(313),FDATA1(213),FDATA1(163),
     2      GFIN1(184),F1WK(1),F1WK(2),GFIN1(185),F1WK(3),
     3      F1WK(268),F1WK(269),F1WK(270))
      IF (LLER1) THEN
C      
C***  AVERAGE LEADING EDGE RADIUS 
C          NOTE - THE METHOD DESCRIBED HERE IS USED FOR EACH FIN SET
C      
C        THIS LOOP DETERMINES HOW MANY SEMI SPANS ARE INPUT (AND THERFORE
C           HOW MANY LEADING EDGE RADII)
C
      DO 1020 I=1,10
         IF(FDATA1(I+1) .EQ. UNUSED)GO TO 1030
         J=I
 1020 CONTINUE
C
C     USE A LINEAR INTERPOLATION TO DETERMINE THE AVERAGE(EFFECTIVE) LEADING
C      EDGE RADIUS THIS IS DONE AT THE AVERAGE SEMI SPAN 
C
 1030 CALL LNTRP(FDATA1(2),FDATA1(364),J,GFIN1(178),F1WK(1))
      ENDIF
      IF(FDATA1(1) .EQ. 3.) THEN
C      
C  SET UP SECTION IF USER DEFINED COORDINATES
C      
            CALL USEFOL(1,FDATA1(113),
     1      FDATA1(263),FDATA1(313),FDATA1(213),FDATA1(163),
     2      F1WK(1),F1WK(2),GFIN1(185),F1WK(3))
C
      ENDIF 
      GFIN1(176)=F1WK(2)
      DO 1050 I=1,10
         GFIN1(I+150)=F1WK(2)
         FDATA1(I+363)=F1WK(1)
 1050 CONTINUE
C      
C***  FIN SET NUMBER 2
C      
 1060 IF(.NOT. LFIN2)GO TO 1120
C      
C  IF USER HAS INPUT LEADING EDGE RADII SET LLER2 TO TRUE
C      
      IF(FDATA2(364) .NE. UNUSED)LLER2=.TRUE.
C      
C  PLANFORM GEOMETRY CONSTANTS
C      
      CALL F2GEOM
C      
C  DEFINE AIRFOIL SECTION IF NECESSARY
C      
      CALL FOIL(2,FDATA2(1),GFIN2(178),FDATA2(2),FDATA2(12),
     1          FDATA2(22),FDATA2(62),FDATA2(364),F2WK(1),LNACA,
     2          NACA(1,2))
C      
C  SET UP SECTION IF NOT USER DEFINED COORDINATES
C      
      IF(FDATA2(1) .LT. 3.)CALL AIRFOL(2,NACA(1,2),FDATA2(113),
     1      FDATA2(263),FDATA2(313),FDATA2(213),FDATA2(163),
     2      GFIN2(184),F2WK(1),F2WK(2),GFIN2(185),F2WK(3),
     3      F2WK(268),F2WK(269),F2WK(270))
      IF(LLER2) THEN
C      
C  AVERAGE LEADING EDGE RADIUS
C      
      DO 1080 I=1,10
         IF(FDATA2(I+1) .EQ. UNUSED)GO TO 1090
         J=I
 1080 CONTINUE
 1090 CALL LNTRP(FDATA2(2),FDATA2(364),J,GFIN2(178),F2WK(1))
      ENDIF
      IF(FDATA2(1) .EQ. 3.) THEN
C      
C  SET UP SECTION IF USER DEFINED COORDINATES
C      
            CALL USEFOL(2,FDATA2(113),
     1      FDATA2(263),FDATA2(313),FDATA2(213),FDATA2(163),
     2      F2WK(1),F2WK(2),GFIN2(185),F2WK(3))
C      
      ENDIF
      GFIN2(176)=F2WK(2)
      DO 1110 I=1,10
         GFIN2(I+150)=F2WK(2)
         FDATA2(I+363)=F2WK(1)
 1110 CONTINUE
C      
C***  FIN SET NUMBER 3
C      
 1120 IF(.NOT. LFIN3)GO TO 1180
C      
C  IF USER HAS INPUT LEADING EDGE RADII SET LLER3 TO TRUE
C      
      IF(FDATA3(364) .NE. UNUSED)LLER3=.TRUE.
C      
C  PLANFORM GEOMETRY CONSTANTS
C      
      CALL F3GEOM
C      
C  DEFINE AIRFOIL SECTION IF NECESSARY
C      
      CALL FOIL(3,FDATA3(1),GFIN3(178),FDATA3(2),FDATA3(12),
     1          FDATA3(22),FDATA3(62),FDATA3(364),F3WK(1),LNACA,
     2          NACA(1,3))
C      
C  SET UP SECTION IF NOT USER DEFINED COORDINATES
C      
      IF(FDATA3(1) .LT. 3.)CALL AIRFOL(3,NACA(1,3),FDATA3(113),
     1      FDATA3(263),FDATA3(313),FDATA3(213),FDATA3(163),
     2      GFIN3(184),F3WK(1),F3WK(2),GFIN3(185),F3WK(3),
     3      F3WK(268),F3WK(269),F3WK(270))
      IF(LLER3) THEN
C      
C  AVERAGE LEADING EDGE RADIUS
C      
      DO 1140 I=1,10
         IF(FDATA3(I+1) .EQ. UNUSED)GO TO 1150
         J=I
 1140 CONTINUE
 1150 CALL LNTRP(FDATA3(2),FDATA3(364),J,GFIN3(178),F3WK(1))
      ENDIF
      IF(FDATA3(1) .EQ. 3.) THEN
C      
C  SET UP SECTION IF USER DEFINED COORDINATES
C      
            CALL USEFOL(3,FDATA3(113),
     1      FDATA3(263),FDATA3(313),FDATA3(213),FDATA3(163),
     2      F3WK(1),F3WK(2),GFIN3(185),F3WK(3))
C      
      ENDIF
      GFIN3(176)=F3WK(2)
      DO 1170 I=1,10
         GFIN3(I+150)=F3WK(2)
         FDATA3(I+363)=F3WK(1)
 1170 CONTINUE
C      
C***  FIN SET NUMBER 4
C      
 1180 IF(.NOT. LFIN4)GO TO 1240
C      
C  IF USER HAS INPUT LEADING EDGE RADII SET LLER4 TO TRUE
C      
      IF(FDATA4(364) .NE. UNUSED)LLER4=.TRUE.
C      
C  PLANFORM GEOMETRY CONSTANTS
C      
      CALL F4GEOM
C      
C  DEFINE AIRFOIL SECTION IF NECESSARY
C      
      CALL FOIL(4,FDATA4(1),GFIN4(178),FDATA4(2),FDATA4(12),
     1          FDATA4(22),FDATA4(62),FDATA4(364),F4WK(1),LNACA,
     2          NACA(1,4))
C      
C  SET UP SECTION IF NOT USER DEFINED COORDINATES
C      
      IF(FDATA4(1) .LT. 3.)CALL AIRFOL(4,NACA(1,4),FDATA4(113),
     1      FDATA4(263),FDATA4(313),FDATA4(213),FDATA4(163),
     2      GFIN4(184),F4WK(1),F4WK(2),GFIN4(185),F4WK(3),
     3      F4WK(268),F4WK(269),F4WK(270))
      IF(LLER4) THEN
C      
C  AVERAGE LEADING EDGE RADIUS
C      
 1190 DO 1200 I=1,10
         IF(FDATA4(I+1) .EQ. UNUSED)GO TO 1210
         J=I
 1200 CONTINUE
 1210 CALL LNTRP(FDATA4(2),FDATA4(364),J,GFIN4(178),F4WK(1))
      ENDIF
      IF(FDATA4(1) .EQ. 3.) THEN
C      
C  SET UP SECTION IF USER DEFINED COORDINATES
C      
            CALL USEFOL(4,FDATA4(113),
     1      FDATA4(263),FDATA4(313),FDATA4(213),FDATA4(163),
     2      F4WK(1),F4WK(2),GFIN4(185),F4WK(3))
C      
      ENDIF
      GFIN4(176)=F4WK(2)
      DO 1230 I=1,10
         GFIN4(I+150)=F4WK(2)
         FDATA4(I+363)=F4WK(1)
 1230 CONTINUE
C      
 1240 CONTINUE
C      
C***  COMPUTE X/C AT MAX THICKNESS FOR EACH FIN SET      
C      
      TOC1=FDATA1(213)
      GFIN1(188)=FDATA1(113)
      TOC2=FDATA2(213)
      GFIN2(188)=FDATA2(113)
      TOC3=FDATA3(213)
      GFIN3(188)=FDATA3(113)
      TOC4=FDATA4(213)
      GFIN4(188)=FDATA4(113)
      DO 1260 I=2,50
         IF(FDATA1(I+212).GT.TOC1) THEN
           GFIN1(188)=FDATA1(I+112)
           TOC1=FDATA1(I+212)
         ENDIF
         IF(FDATA2(I+212).GT.TOC2) THEN
           GFIN2(188)=FDATA2(I+112)
           TOC2=FDATA2(I+212)
         ENDIF
         IF(FDATA3(I+212).GT.TOC3) THEN
           GFIN3(188)=FDATA3(I+112)
           TOC3=FDATA3(I+212)
         ENDIF
         IF(FDATA4(I+212).GT.TOC4) THEN
           GFIN4(188)=FDATA4(I+112)
           TOC4=FDATA4(I+212)
         ENDIF
 1260 CONTINUE
C      
C***  COMPUTE SEMI-WEDGE ANGLE AT LEADING EDGE
C      
      GFIN1(186)=ATAN(FDATA1(214)/FDATA1(114))*RAD
      GFIN2(186)=ATAN(FDATA2(214)/FDATA2(114))*RAD
      GFIN3(186)=ATAN(FDATA3(214)/FDATA3(114))*RAD
      GFIN4(186)=ATAN(FDATA4(214)/FDATA4(114))*RAD
C      
      RETURN
      END
