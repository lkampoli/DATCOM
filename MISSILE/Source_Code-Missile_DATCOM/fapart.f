      SUBROUTINE FAPART(IM,NSET,NFINS,CAF,CAPR,CAWT,CAWS,CALE,
     1 CATE,CAO,CANL,CATOT)
C
C***  WRITE PARTIAL OUTPUT FOR FIN AXIAL FORCE
C
C   INPUTS
C
C     IM - CURRENT MACH NUMBER INDEX
C   NSET - FIN SET NUMBER
C  NFINS - NUMBER OF FINS IN THE FIN SET
C    CAF - SKIN FRICTION INCREMENT (1 FIN)
C   CAPR - SUBSONIC PRESSURE DRAG INCREMENT (1 FIN)
C   CAWT - TRANSONIC WAVE DRAG INCREMENT (1 FIN)
C   CAWS - SUPERSONIC WAVE DRAG INCREMENT (1 FIN)
C   CALE - LEADING EDGE DRAG INCREMENT (1 FIN)
C   CATE - TRAILING EDGE DRAG INCREMENT (1 FIN)
C    CAO - TOTAL ZERO-LIFT DRAG COEFFICIENT (1 FIN)
C   CANL - NON-LINEAR VARIATION OF CA (1 FIN)
C  CATOT - TOTAL FIN SET AXIAL FORCE COEFFICIENT (N FINS)
C
      COMMON /FLC/  NALPHA,ALPHA(20),FLDUM(123)
C
      REAL NALPHA
C
      LOGICAL NDUM,NMF,NAF
C
      DIMENSION CANL(20),CATOT(20)
C
      CALL HEADR9
      WRITE(6,1010)NSET
      CALL PRFLC9(IM,NDUM,NMF,NAF)
      WRITE(6,1020)
      WRITE(6,1030)CAF
      WRITE(6,1040)CAPR
      WRITE(6,1050)CAWT
      WRITE(6,1060)CAWS
      WRITE(6,1070)CALE
      WRITE(6,1080)CATE
      WRITE(6,1090)CAO
      WRITE(6,1100)
      WRITE(6,1110)NFINS
      NALP=NALPHA+0.5
      DO 1000 I=1,NALP
         WRITE(6,1120)ALPHA(I),CANL(I),CATOT(I)
 1000 CONTINUE
C
 1010 FORMAT(24X,'FIN SET',I2, ' CA PARTIAL OUTPUT',/)
 1020 FORMAT(4X,'SINGLE FIN PANEL ZERO-LIFT AXIAL FORCE COMPONENTS',/)
 1030 FORMAT(4X,'SKIN FRICTION     ',F9.4)
 1040 FORMAT(4X,'SUBSONIC PRESSURE ',F9.4)
 1050 FORMAT(4X,'TRANSONIC WAVE    ',F9.4)
 1060 FORMAT(4X,'SUPERSONIC WAVE   ',F9.4)
 1070 FORMAT(4X,'LEADING EDGE      ',F9.4)
 1080 FORMAT(4X,'TRAILING EDGE     ',F9.4)
 1090 FORMAT(4X,'TOTAL CAO         ',F9.4,/)
 1100 FORMAT(4X,'FIN AXIAL FORCE DUE TO ANGLE OF ATTACK',/)
 1110 FORMAT(4X,'ALPHA',6X,'CA DUE TO LIFT (SINGLE PANEL)',3X,
     1 'CA-TOTAL (',I1,' FINS)',/)
 1120 FORMAT(4X,F7.2,11X,F9.4,19X,F9.4)
      RETURN
      END
