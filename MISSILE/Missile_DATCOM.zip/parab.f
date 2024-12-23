      SUBROUTINE PARAB(X,Y,Z,N,SL1,SLN)
C
C  SUBROUTINE TO FIT A SET OF INPUT DATA USING A PARABOLA
C  THE LEFT HAND AND RIGHT HAND SLOPES ARE USED IN THE FITTING
C
C  INPUTS
C
C      X -- INPUT X COORDINATES
C      Y -- INPUT Y COORDINATES
C   NPTS -- NUMBER OF POINTS TO FIT
C    SL1 -- BEGINNING SLOPE
C    SLN -- END SLOPE
C
C  OUTPUT
C
C      Z -- VALUES OF Z
C
C  ROUTINES CALLED -- NONE
C
      DIMENSION X(50),Y(50),Z(50),Q(50)
      IF(N.EQ.2)GO TO 1020
      GM=(2.+(X(3)-X(2))/(X(2)-X(1)))/(X(3)-X(1))
      DE=(X(2)-X(1))/(X(3)-X(2))/(X(3)-X(1))
      P=2./(X(2)-X(1))+GM
      Q(1)=-DE/P
      Z(1)=(SL1/2.+Y(1)*2./(X(2)-X(1))+Y(2)*(GM+DE))/P
      NM=N-1
      DO 1000 I=2,NM
         AL=(X(I+1)-X(I))/(X(I)-X(I-1))/(X(I+1)-X(I-1))
         BE=(2.+(X(I)-X(I-1))/(X(I+1)-X(I)))/(X(I+1)-X(I-1))
      IF(I.EQ.NM)GO TO 1000
         GM=(2.+(X(I+2)-X(I+1))/(X(I+1)-X(I)))/(X(I+2)-X(I))
         DE=(X(I+1)-X(I))/(X(I+2)-X(I+1))/(X(I+2)-X(I))
         P=AL*Q(I-1)+BE+GM
         Q(I)=-DE/P
         Z(I)=(Y(I)*(AL+BE)+Y(I+1)*(GM+DE)-AL*Z(I-1))/P
 1000 CONTINUE
      P=AL*Q(N-2)+BE+2./(X(N)-X(N-1))
      Z(N-1)=(Y(N-1)*(AL+BE)+Y(N)*2./(X(N)-X(N-1))-SLN/2.-AL*Z(N-2))/P
      NZ=N-2
      DO 1010 I=1,NZ
         IR=NM-I
         Z(IR)=Q(IR)*Z(IR+1)+Z(IR)
 1010 CONTINUE
      GO TO 1030
C
C   FOR N=2
C
 1020 Z(1)=(Y(2)+Y(1))/2.+(SL1-SLN)*(X(2)-X(1))/8.
 1030 CONTINUE
      RETURN
      END
