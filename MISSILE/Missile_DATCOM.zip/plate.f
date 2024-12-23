      SUBROUTINE PLATE(WH,CDPL)
C ****  2-D DRAG ON FLATE PLATE AS A FUNCTION OF W/H FIG.3.28  ****
C **              ( DATA FOR FLAT PLATE SUBROUTINE)              **
      REAL XARRAY(11),YARRAY(11)
      DATA (YARRAY(I), I=1,11) /1.98,1.79,1.6,1.46,1.36,1.31,1.24,
     * 1.2,1.18,1.18,1.18/
      DATA (XARRAY(I), I=1,11) /0.0,.02,.04,.06,.08,.1,.15,.2,.3,
     * .5,1.0/
      DATA NDIM,NAVAL,NXARR,NXVAL,IEXTRP /1,11,11,11,9/
      X=WH
      CALL TABLOK(NDIM,NAVAL,NXARR,NXVAL,XARRAY,YARRAY,X,Y,IEXTRP,
     * IERR)
      CDPL=Y
      RETURN
      END
