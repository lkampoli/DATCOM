      SUBROUTINE LIPCOR(CAINL,MACH)
C
C *** TEMPORARY SUBROUTINE TO CORRECT DIVERTER MODEL BASED ON
C *** NASA MEMO 1-10-59E, SEPT 1958.
C *** THIS CORRECTS A SPECIFIC INLET FOR OFF DESIGN, COWL-LIP
C *** DRAG  (THIS IS NOT GENERALIZED YET)
C
      DIMENSION DCAL(4),TMACH(4)
      REAL MACH
      DATA DCAL/.02286,.02487,.02712,.02893/
      DATA TMACH/2.5,2.95,3.5,3.95/
C
      DO 1000 I=1,4
      IF (ABS(MACH-TMACH(I)).GT..1) GO TO 1000
      IM=I
      GO TO 1010
 1000 CONTINUE
 1010 CONTINUE
C     CAINL=CAINL-DCAL(IM)
C
C *** C WITH ABOVE LINE NULLS OUT CORRECTION
C *** ON DESIGN OPERATION IS ASSUMED
C
      RETURN
      END
