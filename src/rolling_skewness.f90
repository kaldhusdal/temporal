SUBROUTINE rolling_skewness(x, n, w, narm, skewx)
  ! G1 in https://en.wikipedia.org/wiki/Skewness

  IMPLICIT NONE
  INTEGER:: w, n, ni, i, j
  LOGICAL:: narm
  REAL*8:: mean, sumx2, varx, a, m, inum
  REAL*8, DIMENSION(n):: x, skewx
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
    m = DBLE(ni)
    IF (ni > 2) THEN
      !a = (m * (m + 1)) / ((m - 1) * (m - 2) * (m - 3))
      a = SQRT(m * (m - 1)) / (m * (m - 2))
      mean = SUM(x((i - w + 1):i)) / ni
      sumx2 = SUM(x((i - w + 1):i)**2)
      !varx = (sumx2 - ni * mean**2) / (ni - 1)
      varx = (sumx2 - ni * mean**2) / ni
      skewx(i) = 0
      DO j = 1, i
        skewx(i) = skewx(i) + (x(j) - mean)**3
      END DO
      skewx(i) = a * skewx(i) / varx**(3 / 2)
    ENDIF
  END DO

  DO i = w, n
    ni = SUM(nan((i - w + 1):i))
    m = DBLE(ni)
    IF (ni > 2) THEN
      !a = (m * (m + 1)) / ((m - 1) * (m - 2) * (m - 3))
      a = SQRT(m * (m - 1)) / (m * (m - 2))
      mean = SUM(x((i - w + 1):i)) / ni
      sumx2 = SUM(x((i - w + 1):i)**2)
      !varx = (sumx2 - ni * mean**2) / (ni - 1)
      varx = (sumx2 - ni * mean**2) / ni
      skewx(i) = 0
      DO j = i - w + 1, i
        skewx(i) = skewx(i) + (x(j) - mean)**3
      END DO
      skewx(i) = a * skewx(i) / varx**(3 / 2)
    ENDIF
  END DO


END SUBROUTINE rolling_skewness
