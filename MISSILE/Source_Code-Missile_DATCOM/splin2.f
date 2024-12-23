      SUBROUTINE SPLIN2(M,X,Y,K)
C
C***  SUBROUTINE TO FIT A NATURAL SPLINE TO THE INPUT ARRAY
C***  THE SPLINE COEFFICIENTS ARE RETURNED
C
C***  INPUTS
C
C     M - NUMBER OF POINTS TO SPLINE
C     X - X ARRAY OF DATA TO SPLINE
C     Y - Y ARRAY OF DATA TO SPLINE
C
C***  OUTPUT
C
C     K - ARRAY OF SPLINE COEFFICIENTS
C                          
C***  THE FOLLOWING STATEMENTS ARE SET FOR UP TO 100 INPUT POINTS
C***  TWO ADDITIONAL ARE REQUIRED IN EACH ARRAY
C
      PARAMETER (NMAX=100)
      DIMENSION X(NMAX),Y(NMAX),A(NMAX,NMAX),K(NMAX)
C
      REAL K                                     
C
      DO 1010 I=1,M
         DO 1000 J=1,M
            A(I,J)=0.  
 1000    CONTINUE
 1010 CONTINUE
C
      A(1,1)=2.  
      A(1,2)=1.  
      A(1,M+1)=3.*(Y(2)-Y(1))/(X(2)-X(1))
      A(M,M-1)=1.      
      A(M,M)=2.  
      A(M,M+1)=3.*(Y(M)-Y(M-1))/(X(M)-X(M-1))
C
      MM2=M-2
      DO 1020 I=1,MM2
         A(I+1,I)=X(I+2)-X(I+1)
         A(I+1,I+2)=X(I+1)-X(I)
         A(I+1,I+1)=2.*(A(I+1,I)+A(I+1,I+2))
         D1=(Y(I+1)-Y(I))/A(I+1,I+2)
         D2=(Y(I+2)-Y(I+1))/A(I+1,I)
         A(I+1,M+1)=3.*(D1*A(I+1,I)+D2*A(I+1,I+2))
 1020 CONTINUE
C
      NROW=M-1
      NCOL=M
C
      DO 1030 J=1,NCOL
         A(1,J+1)=A(1,J+1)/A(1,1)
 1030 CONTINUE
C
      DO 1080 L=1,NROW
         DO 1050 I=L,NROW
            SUM=0.  
            DO 1040 KK=1,L
               SUM=SUM+A(I+1,KK)*A(KK,L+1)
 1040       CONTINUE
            A(I+1,L+1)=A(I+1,L+1)-SUM
 1050    CONTINUE
C
         LP1=L+1
         DO 1070 J=LP1,NCOL
            SUM=0.  
            DO 1060 KK=1,L
               SUM=SUM+A(L+1,KK)*A(KK,J+1)
 1060       CONTINUE
            A(L+1,J+1)=(A(L+1,J+1)-SUM)/A(L+1,L+1)
 1070    CONTINUE
 1080 CONTINUE
C
      K(NROW+1)=A(NROW+1,NCOL+1)
      DO 1100 MM=1,NROW
         I=NROW-MM
         SUM=0.  
         IP1=I+1
         DO 1090 J=IP1,NROW
            SUM=SUM+A(I+1,J+1)*K(J+1)
 1090    CONTINUE
         K(I+1)=A(I+1,NCOL+1)-SUM
 1100 CONTINUE
C
      RETURN
      END
