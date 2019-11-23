SUBROUTINE rolling_mean(x, n, w, narm, meanx)

  IMPLICIT NONE
  INTEGER:: w, n, ni, i
  LOGICAL:: narm
  REAL*8, DIMENSION(n):: x, meanx
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
      ENDIF
    END DO
  ENDIF

  ! Calculate arithmetic mean for indices 1 through w-1.
  DO i = 1, w - 1
    ni = SUM(nan(1:i))
    IF (ni > 0) THEN
      meanx(i) = SUM(x(1:i)) / ni
    ENDIF
  END DO
  ! Calculate arithmetic mean for indices w through n.
  DO i = w, n
    ni = SUM(nan((i - w + 1):i))
    IF (ni > 0) THEN
      meanx(i) = SUM(x((i - w + 1):i)) / ni
    ENDIF
  END DO

END SUBROUTINE rolling_mean 
