SUBROUTINE rolling_var(x, n, w, narm, varx)

  IMPLICIT NONE
  INTEGER:: w, n, ni, i
  LOGICAL:: narm
  REAL*8:: sumx2, mean
  REAL*8, DIMENSION(n):: x, varx
  INTEGER, DIMENSION(n):: nan
  nan = 1

  ! If NA values should be ignored, first loop through all values, set
  ! each value of 'nan' to 0 if it is NA and 1 otherwise, then change 
  ! all NA values in 'x' to 0.
  IF (narm) THEN
    DO i = 1, n
      IF (ISNAN(x(i))) THEN
        nan(i) = 0
        x(i) = 0
      END IF
    END DO
  ENDIF
  

  DO i = 1, w - 1
    ni = SUM(nan(1:i))
    IF (ni > 1) THEN
      mean = SUM(x(1:i)) / ni
      sumx2 = SUM(x(1:i)**2)
      varx(i) = (sumx2 - ni * mean**2) / (ni - 1)
    ENDIF
  END DO
  DO i = w, n
    ni = SUM(nan((i - w + 1):i))
    IF (ni > 1) THEN
      mean = SUM(x((i - w + 1):i)) / ni
      sumx2 = SUM(x((i - w + 1):i)**2)
      varx(i) = (sumx2 - ni * mean**2) / (ni - 1)
    ENDIF
  END DO

  
END SUBROUTINE rolling_var
