      SUBROUTINE INTERP(TX,TY,X,Y,N,J)
      DIMENSION TX(100),TY(100)
      IF((N-J+2).EQ.1)GO TO 1010
      I=J-3
 1000 I=I+1
      IF(TX(I).LT.X) GO TO 1000
      IF(I.LE.J) I=J
      IF(I.GT.(N-2)) I=N-2
      CALL INTER5(X,TX(I-2),TX(I-1),TX(I),TX(I+1),TX(I+2),TY(I-2),TY(I-1
     1),TY(I),TY(I+1),TY(I+2),Y)
      GO TO 1020
 1010 Y=TY(J-2)+(X-TX(J-2))*(TY(N)-TY(J-2))/(TX(N)-TX(J-2))
 1020 CONTINUE
      RETURN
      END
