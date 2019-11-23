SUBROUTINE rolling_kurtosis(x, n, w, narm, kurtx)
  ! Excess sample kurtosis
  ! https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-whats-with-the-different-formulas-for-kurtosis/

  IMPLICIT NONE
  INTEGER:: w, n, ni, i, j
  LOGICAL:: narm
  REAL*8:: mean, sumx2, varx, a, b, m
  REAL*8, DIMENSION(n):: x, kurtx
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
    IF (ni > 3) THEN
      a = (m * (m + 1)) / ((m - 1) * (m - 2) * (m - 3))
      b = -3 * (m - 1)**2 / ((m - 2) * (m - 3))
      mean = SUM(x(1:i)) / ni
      sumx2 = SUM(x(1:i)**2)
      varx = (sumx2 - ni * mean**2) / (ni - 1)
      kurtx(i) = 0
      DO j = 1, i
        kurtx(i) = kurtx(i) + (x(j) - mean)**4
      END DO
      kurtx(i) = a * kurtx(i) / varx**2 + b
    ENDIF
  END DO

  DO i = w, n
    ni = SUM(nan((i - w + 1):i))
    m = DBLE(ni)
    IF (ni > 3) THEN
      a = (m * (m + 1)) / ((m - 1) * (m - 2) * (m - 3))
      b = -3 * (m - 1)**2 / ((m - 2) * (m - 3))
      mean = SUM(x((i - w + 1):i)) / ni
      sumx2 = SUM(x((i - w + 1):i)**2)
      varx = (sumx2 - ni * mean**2) / (ni - 1)
      kurtx(i) = 0
      DO j = i - w + 1, i
        kurtx(i) = kurtx(i) + (x(j) - mean)**4
      END DO
      kurtx(i) = a * kurtx(i) / varx**2 + b
    ENDIF
  END DO


END SUBROUTINE rolling_kurtosis
