      SUBROUTINE TABLOK(NDIM,NAVAL,NXARR,NXVAL,XARRAY,YARRAY,
     1                  X,Y,IEXTRP,IERR)
C
C  WRITTEN BY A. JENN, MCDONNELL DOUGLAS
C
C     THIS ROUTINE PERFORMS A TABLE LOOKUP ON A N-DIMENSIONAL TABLE.
C     THE VALUE OF N IS LIMITED ONLY BY THE PARAMETER NDIMM.  NDIMM
C     MUST BE AT LEAST AS LARGE AS THE NUMBER OF DIMENSIONS USED FOR
C     THE ARRAY OF DEPENDENT VALUES.  ALL INTERPOLATIONS ARE LINEAR.
C
C     INPUTS:
C
C     NDIM   - NUMBER OF INDEPENDENT VARIABLES
C
C     NAVAL  - 1-D ARRAY OF THE DIMENSIONED SIZE OF THE DEPENDENT
C              VARIABLE ARRAY, YARRAY, IN THE CALLING ROUTINE,
C              NAVAL(1...NDIM).  NAVAL WILL BE A SCALAR IF NDIM=1.
C
C     NXARR  - FIRST DIMENSION IN THE XARRAY FROM THE CALLING ROUTINE
C
C     NXVAL  - 1-D ARRAY OF NUMBER OF VALUES FOR EACH OF THE
C              INDEPENDENT VARIABLES, NXVAL(1...NDIM).  NXVAL WILL
C              BE A SCALAR IF NDIM=1.
C
C     XARRAY - 2-D ARRAY OF THE VALUES OF THE INDEPENDENT VARIABLES
C              CORRESPONDING TO THE VALUES IN THE DEPENDENT VALUE 
C              ARRAY, XARRAY(1...MAX(NXVAL),1...NDIM).  XARRAY WILL
C              BE A 1-D ARRAY IF NDIM=1.
C
C     YARRAY - NDIM-D ARRAY OF THE DEPENDENT VARIABLE VALUES,
C              YARRAY(1...NXVAL(1),1...NXVAL(2),...,1...NXVAL(NDIM)).
C
C     X      - 1-D ARRAY OF DEPENDENT VALUES AT WHICH A VALUE IS
C              TO BE LOOKED UP OUT OF THE TABLE, X(1...NDIM).  X WILL
C              BE A SCALAR IF NDIM=1.
C
C     IEXTRP - 1-D ARRAY OF EXTRAPOLATION OPTIONS, IEXTRP(1...NDIM),
C              CORRESPONDING TO EACH INDEPENDENT VARIABLE. EXTRAPOLATION 
C              OF AN INDEPENDENT VARIABLE IS ALLOWED BASED ON THE VALUE
C              OF IEXTRP FOR THAT INDEPENDENT VARIABLE.
C
C              IEXTRP(I) = 0 ==> LOW AND HIGH END EXTRAPOLATION ALLOWED
C              IEXTRP(I) = 1 ==> LOW END EXTRAPOLATION ALLOWED ONLY
C              IEXTRP(I) = 2 ==> HIGH END EXTRAPOLATION ALLOWED ONLY
C              IEXTRP(I) = ANYTHING ELSE ==> NO EXTRAPOLATION ALLOWED
C
C              IF EXTRAPOLATION IS NOT ALLOWED AND THE INDEPENDENT 
C              VARIABLE IS OUT OF RANGE, THE INDEPENDENT VARIABLE IS 
C              CLIPPED TO THE APPROPRIATE EDGE OF THE INDEPENDENT 
C              VARIABLE ARRAY, AND THE ERROR FLAG, IERR, IS SET.
C                
C
C     OUTPUTS:
C
C     Y      - VALUE OF DEPENDENT VARIABLE BASED ON X(1...NDIM)
C
C     IERR   - ERROR FLAG TO INDICATE IF INDEPENDENT VARIABLE CLIPPING 
C              HAS OCCURRED.  IERR=0 FOR NO CLIPPING OR IERR=IDIM WHERE 
C              IDIM IS THE INDEX OF THE FIRST INDEPENDENT VARIABLE WHICH 
C              REQUIRED CLIPPING.  IF THE NUMBER OF INDEPENDENT 
C              VARIABLES IS TOO LARGE FOR THE CURRENT ARRAY DIMENSIONS, 
C              IERR=-1 WILL BE RETURNED.  IF THIS OCCURS, THIS 
C              SUBROUTINE MUST BE RECOMPILED WITH THE VALUE OF NDIMM AT 
C              LEAST AS LARGE AS THE NUMBER OF INDEPENDENT VARIABLES.
C
C     PARAMETER FOR THE MAXIMUM NUMBER OF INDEPENDENT VARIABLES.  THIS
C     PARAMETER MUST BE AT LEAST AS LARGE AS THE NUMBER OF INDEPENDENT
C     VARIABLES USED IN THE PROBLEM.  IF NDIMM IS TOO SMALL, ITS VALUE
C     MUST SIMPLY BE INCREASED TO THE APPROPRIATE SIZE.
      PARAMETER (NDIMM = 10)
C
C     PARAMETER FOR THE MAXIMUM SIZE OF THE WORK ARRAY
      PARAMETER (NWORKM = 2**NDIMM-1)
C
      INTEGER NXVAL(1), NAVAL(1), IEXTRP(1), IXSAV(NDIMM), INCR(NDIMM), 
     1        IBIT(NDIMM)
C
      REAL XARRAY(1), YARRAY(1), X(1), WORK(NWORKM), XL(NDIMM)
C
      DIMENSION XM(NDIMM),X1M(NDIMM),XNM(NDIMM)
C
C     CHECK IF ARRAY DIMENSIONS ARE LARGE ENOUGH TO HANDLE PROBLEM
      IF (NDIM .GT. NDIMM) THEN
C
C        ARRAY DIMENSIONS ARE TOO SMALL
         IERR = -1
         GOTO 180
C
      ELSE
C
C        INITIALIZE THE ERROR FLAG
         IERR = 0
C
      END IF
      IM=0
C
C     PLACE THE X ARRAY IN LOCAL STORAGE IN CASE VALUES NEED TO
C     BE CLIPPED DUE TO EXTRAPOLATION
      DO 90 IDIM = 1, NDIM
         XL(IDIM) = X(IDIM)
   90 CONTINUE
C
C     LOOP TO FIND THE INTERVAL IN THE INDEPENDENT VARIABLE ARRAY,
C     XARRAY, IN WHICH EACH INDEPENDENT VARIABLE FALLS
      DO 110 IDIM = 1, NDIM
C
C        LOOP FOR EACH INDEPENDENT VARIABLE
         DO 100 IXVAL = 1, NXVAL(IDIM)-1
C
C           CHECK IF THE INDEPENDENT VARIABLE VALUE FALLS IN THIS INTERVAL
            ILO = IXVAL + (IDIM - 1) * NXARR
            IHI = ILO + 1
            IF (XL(IDIM) .GE. XARRAY(ILO) .AND.
     1         XL(IDIM) .LE. XARRAY(IHI)) THEN
C
C              STORE THE INTERVAL FOR THIS INDEPENDENT VARIABLE
               IXSAV(IDIM) = IXVAL
               GOTO 110
C
            END IF
C
  100    CONTINUE
C
C        HERE IF INDEPENDENT VARIABLE IS OUT OF RANGE.  CHECK IF 
C        THE VALUE IS TOO LARGE OR TOO SMALL
         ILO = 1 + (IDIM - 1) * NXARR
         IF (XL(IDIM) .LT. XARRAY(ILO)) THEN
C                                 
C           STORE THE FIRST INTERVAL
            IXSAV(IDIM) = 1
C
C           CHECK IF EXTRAPOLATION IS PROHIBITED
            IF (IEXTRP(IDIM) .EQ. 0 .OR. IEXTRP(IDIM) .EQ. 1) THEN
              IM=IM+1
              XM(IM)=XL(IDIM)
              X1M(IM)=XARRAY(ILO)
              XNM(IM)=XARRAY(IHI)
            ELSE
C
C              CLIP THE X VARIABLE
               XL(IDIM) = XARRAY(ILO)
C
C              SET THE ERROR FLAG TO INDICATE CLIPPING OCCURRED
               IERR = IDIM
C
            END IF
C
         ELSE
C
C           STORE THE LAST INTERVAL
            IXSAV(IDIM) = NXVAL(IDIM) - 1
C
C           CHECK IF EXTRAPOLATION IS PROHIBITED
            IF (IEXTRP(IDIM) .EQ. 0 .OR. IEXTRP(IDIM) .EQ. 2) THEN
              IM=IM+1
              XM(IM)=XL(IDIM)
              X1M(IM)=XARRAY(ILO)
              XNM(IM)=XARRAY(IHI)
            ELSE
C
C              CLIP THE X VARIABLE
               IHI = NXVAL(IDIM) + (IDIM - 1) * NXARR
               XL(IDIM) = XARRAY(IHI)
C
C              SET THE ERROR FLAG TO INDICATE CLIPPING OCCURRED
               IERR = IDIM
C
            END IF
C
         END IF
C
  110 CONTINUE
C
C     DETERMINE THE INCREMENTAL VALUES NEED TO DETERMINE POINTERS
C     FOR THE DEPENDENT VALUE ARRAY
      INCR(1) = 1
      DO 120 IDIM = 2, NDIM
         INCR(IDIM) = INCR(IDIM-1) * NAVAL(IDIM-1)
  120 CONTINUE
C
C     DEFINE NUMBER OF INTERPOLATIONS REQUIRED ON FIRST PASS
      NFIRST = 2**(NDIM-1) - 1
C
C     LOOP TO LOAD THE WORK ARRAY
      DO 150 IFIRST = NFIRST, 0, -1
C
C        REVERSE THE LOOP INDEX
         IREV = NFIRST - IFIRST
C
C        DETERMINE THE COMBINATIONS OF ZEROS AND ONES REQUIRED TO
C        DETERMINE THE DEPENDENT ARRAY POINTER
         DO 130 IDIM = NDIM, 2, -1
C
C           DETERMINE THE POWER OF TWO
            IMULT = 2**(IDIM-2)
C
C           SET THE BIT IF IVAL IS LARGER THAN IMULT
            IBIT(IDIM) = IREV / IMULT
C
C           CALCULATE THE REMAINDER
            IREV = IREV - IMULT * IBIT(IDIM)
C
  130    CONTINUE
C
C        INITIALIZE SUMMING VARIABLE TO DETERMINE POINTER
         ISPOT = IXSAV(1)
C
C        LOOP TO DETERMINE DEPENDENT ARRAY POINTER
         DO 140 IDIM = 2, NDIM
C
C           ADD THE INCREMENT DUE TO EACH INDEPENDENT VARIABLE TO
C           THE POINTER VALUE
            ISPOT = ISPOT + (IXSAV(IDIM)-IBIT(IDIM)) * INCR(IDIM)
C
  140    CONTINUE
C
C        DETERMINE STORAGE LOCATIONS IN THE WORK ARRAY
         ISPOT1 = ISPOT  + 1
         IWORK  = IFIRST * 2 + 1
         IWORK1 = IWORK  + 1
C
C        LOAD THE DEPENDENT VALUES INTO THE WORK ARRAY
         WORK(IWORK)  = YARRAY(ISPOT)
         WORK(IWORK1) = YARRAY(ISPOT1)
C
  150 CONTINUE
C
C     BEGIN LOOP TO REDUCE WORK ARRAY TO A SINGLE VALUE THROUGH A
C     SERIES OF LINEAR INTERPOLATIONS
      DO 170 IDIM = NDIM, 1, -1
C
C        DETERMINE THE INDICES OF THE INDEPENDENT VARIABLE BOUNDS FOR 
C        THE INTERPOLATION
         IREV = NDIM - IDIM + 1
         IX1  = IXSAV(IREV) + (IREV - 1) * NXARR
         IX2  = IX1 + 1
C
C        DETERMINE THE INDEPENDENT VARIABLE BOUNDS FOR THE INTERPOLATION
         X1 = XARRAY(IX1)
         X2 = XARRAY(IX2)
         DELX = X2 - X1
         XINC = XL(IREV) - X1 
C
C        DETERMINE THE NUMBER OF INTERPOLATIONS REQUIRED FOR THIS LEVEL
         NINTRP = 2**(IDIM-1)
C
C        LOOP FOR EACH INTERPOLATION
         DO 160 IINTRP = 1, NINTRP
C
C           DETERMINE THE INDICES OF THE DEPENDENT VARIABLE BOUNDS IN
C           THE WORK ARRAY FOR THIS INTERPOLATION
            IY1 = (IINTRP - 1) * 2 + 1
            IY2 = IY1 + 1
C
C           DETERMINE THE DEPENDENT VARIABLE BOUNDS FOR THE INTERPOLATION
            Y1 = WORK(IY1)
            Y2 = WORK(IY2)
C
C           PERFORM THE LINEAR INTERPOLATION AND PLACE THE RESULT BACK
C           IN THE WORK ARRAY
            DELY = Y2 - Y1
            WORK(IINTRP) = Y1 + DELY / DELX * XINC
C
  160    CONTINUE
C
  170 CONTINUE
C
C     ASSIGN THE FINAL RESULT TO THE RETURN VARIABLE
      Y = WORK(1)
C
C     WRITE EXTRAPOLATION MESSAGES
c     DO 175 IM=1,NDIM
c       CALL MESSG(XM(IM),Y,X1M(IM),XNM(IM),0.0,0.0,1)
c 175 CONTINUE
C
  180 CONTINUE
      RETURN
      END
