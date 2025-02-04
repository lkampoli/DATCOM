      SUBROUTINE SYNPAR(IM,NA,ALP,NSETS,NFIN,XCPWA,YCPFIN,AFACT)
C
C***  PRINT CONFIGURATION SYNTHESIS PARTIAL OUTPUT DATA
C
      COMMON /PAERO/  AKBW(4),AKWB(4),AKKBW(4),AKKWB(4),XCPBW(4),
     1                XCPWB(4),DAQ(8,20,4),WAQ(8,20,4),
     2                CNW(8,20,4),CLW(8,20,4),CNWB(20,4),CMWB(20,4),
     3                CAWB(20,4),CYWB(20,4),CSNWB(20,4),CSLWB(20,4),
     4                CNBW(20,4),CMBW(20,4),CABW(20,4),CYBW(20,4),
     5                CSNBW(20,4),CSLBW(20,4),CN(20,4),CM(20,4),
     6                CA(20,4),CY(20,4),CSN(20,4),CSL(20,4)
C
      LOGICAL NDUM,NMF,NAF
      DIMENSION NFIN(4),ALP(NA),XCPWA(4),YCPFIN(20,4),AFACT(20,4)
C***
C***  FINS IN PRESENCE OF THE BODY AERODYNAMICS
C***
      DO 1020 ISET=1,NSETS
         CALL HEADR9
         WRITE(6,1060)
         CALL PRFLC9(IM,NDUM,NMF,NAF)
         NFINS=NFIN(ISET)
         WRITE(6,1070)ISET
         WRITE(6,1080)
         DO 1010 I=1,NA
            IFIN=1
            WRITE(6,1100) ALP(I),CNWB(I,ISET),CMWB(I,ISET),CAWB(I,ISET),
     1         CYWB(I,ISET),CSNWB(I,ISET),CSLWB(I,ISET)
 1010    CONTINUE
 1020 CONTINUE
C
C***  INDIVIDUAL PANEL ALPHA-EQUIVALENT AND NORMAL FORCE VALUES
C
      DO 1040 ISET=1,NSETS
         CALL HEADR9
         WRITE(6,1060)
         CALL PRFLC9(IM,NDUM,NMF,NAF)
         WRITE(6,1075)ISET
         WRITE(6,1090)
         DO 1045 II=1,NA
            DO 1000 IFIN=1,NFINS
               WRITE(6,1110) ALP(II),IFIN,WAQ(IFIN,II,ISET),
     1                       CNW(IFIN,II,ISET)
 1000       CONTINUE
      WRITE(6,*)
 1045    CONTINUE
 1040 CONTINUE
C***
C***  CARRYOVER INTERFERENCE FACTORS AND PANEL CENTERS OF PRESSSURE
C***
      DO 1050 ISET=1,NSETS
      CALL HEADR9
      WRITE(6,1060)
      CALL PRFLC9(IM,NDUM,NMF,NAF)
      WRITE(6,1190) ISET
      WRITE(6,1200)
         DO 1055 I=1,NA
            AKWBA=AKWB(ISET)*AFACT(I,ISET)
            WRITE(6,1210) ALP(I),AKWBA,AKBW(ISET),AKKWB(ISET),
     1      AKKBW(ISET),XCPWB(ISET),XCPBW(ISET),YCPFIN(I,ISET)
 1055    CONTINUE
 1050 CONTINUE
      WRITE(6,1300)
C***
C***  FORMATS
C***
 1060 FORMAT(17X,'AERODYNAMIC FORCE AND MOMENT SYNTHESIS',/)
 1070 FORMAT(10X,'---------------FIN SET ',I1,
     1 ' IN PRESENCE OF THE BODY----------------',/)
 1075 FORMAT(10X,'---------------FIN SET ',I1,
     1 ' PANEL CHARACTERISTICS------------------',/)
 1080 FORMAT(6X,'ALPHA',7X,'CN',9X,'CM',9X,'CA',9X,'CY',
     1 9X,'CLN',8X,'CLL',/)
 1090 FORMAT(6X,'ALPHA',6X,'PANEL',4X,'AEQ (PANEL AXIS SYS.)',
     1       4X,'PANEL CN',/)
 1100 FORMAT(4X,F7.2,6F11.4)
 1110 FORMAT(4X,F7.2,7X,I2,9X,F11.4,6X,F11.4)
 1190 FORMAT(4X,9X,'CARRYOVER INTERFERENCE FACTORS - FIN SET',I2,/)
 1200 FORMAT(6X,'ALPHA',3X,'K-W(B)',3X,'K-B(W)',2X,'KK-W(B)',2X,
     1 'KK-B(W)',2X,'XCP-W(B)',1X,'XCP-B(W)',1X,'Y-CP/(B/2)',/)
 1210 FORMAT(4X,F7.2,7F9.4)
 1300 FORMAT(/,4X,'NOTE - XCP-W(B) USED FOR STABILITY ONLY',
     1       ' DIFFERENT VALUES USED FOR HINGE MOMENTS') 
      RETURN
      END
