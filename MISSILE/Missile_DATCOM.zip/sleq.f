      SUBROUTINE SLEQ(A,X,B,N,M)
C
C****  THIS ROUTINE SOLVES N SIMULTANEOUS EQUATIONS WITH CONSTANTS
C****  MATRIX A(I,J) IS USED AS I=ROW AND J=COLUMN
C
      DIMENSION A(N,M),X(N),B(N)
      NP1=N+1
      IND=0
      NM1=N-1
      DO 1000 K=1,N
      A(K,NP1)=B(K)
 1000 CONTINUE
 1010 DO 1040 K=1,N
      KP1=K+1
C
C  CHECK FOR A ZERO ON THE DIAGONAL
C
      IF(A(K,K).EQ.0.) GO TO 1060
      DO 1020 J=KP1,NP1
      A(K,J)=A(K,J)/A(K,K)
 1020 CONTINUE
      A(K,K)=1.
      DO 1045 I=1,N
      IF(I.EQ.K) GO TO 1045
      DO 1030 J=KP1,NP1
      A(I,J)=A(I,J)-A(I,K)*A(K,J)
 1030 CONTINUE
      A(I,K)=0.0
 1045 CONTINUE
 1040 CONTINUE
C
C  GET THE RESULTS AND STORE IN X
C
      DO 1050 L=1,N
      X(L)=A(L,NP1)
 1050 CONTINUE
      GO TO 1090
C
C  HERE FOR ZERO ON THE DIAGONAL
C  INTERCHANGE THE ROWS AND TRY NEW SOLUTION
C
 1060 IND=IND+1
      IF(IND.GT.N)GO TO 1090
      DO 1080 I=1,M
      TEMP=A(N,I)
      DO 1070 J=1,NM1
      A(N-J+1,I)=A(N-J,I)
 1070 CONTINUE
      A(1,I)=TEMP
 1080 CONTINUE
      GO TO 1010
 1090 CONTINUE
      RETURN
      END
