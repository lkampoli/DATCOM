      SUBROUTINE BODY(IM)
C
C***  TOP LEVEL EXECUTIVE FOR BODY ALONE AERODYNAMICS
C
C***  IM IS THE CURRENT MACH NUMBER INDEX
C
      COMMON /LOGIC/ LDMPCS,LDAMP,LBUILD,LNACA,LDERDG,
     1               LDERRD,LPART,LNAME,LPLOT,
     2               LFLT,LREFQ,LAXIS,LFIN1,LFIN2,LFIN3,LFIN4,
     3               LDEFL,LTRIM,LDIMIN,LDIMFT,LDIMCM,LDIMM,LELLB,
     4               LINLET,LEXPR,LICRMT,LSPIN,LARBOD
      LOGICAL        LDMPCS,LDAMP,LBUILD,LNACA,LDERDG,
     1               LDERRD,LPART,LNAME,LPLOT,
     2               LFLT,LREFQ,LAXIS,LFIN1,LFIN2,LFIN3,LFIN4,
     3               LDEFL,LTRIM,LDIMIN,LDIMFT,LDIMCM,LDIMM,LELLB,
     4               LINLET,LEXPR,LICRMT,LSPIN,LARBOD
C
C***  AXISYMMETRIC BODIES
C
      IF(LAXIS .AND. .NOT. LELLB)CALL BODYA(IM)
C
C***  ELLIPTICAL BODIES
C
      IF(LELLB)CALL BODYE(IM)
C
      RETURN
      END
