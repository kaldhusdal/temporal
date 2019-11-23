SUBROUTINE rolling_sum(x, n, w, windowtype, narm, sumx)

  ! x: The raw data
  ! n: The cardinality of x
  ! w: Window size
  ! windowtype: "lag", "lead" or "mid"
  ! narm: Whether NA values should be ignored or not
  ! sumx: Return vector with the rolling sums
  IMPLICIT NONE
  INTEGER:: n, w, windowtype
  LOGICAL:: narm
  REAL*8, DIMENSION(n):: x, sumx


  INTEGER:: i, whalf
  INTEGER, DIMENSION(n):: nan
  nan = 1

  ! If NA values should be ignored, first loop through all values, set
  ! each value of 'nan' to 0 if it is NA and 1 otherwise, then change 
  ! all NA values in 'x' to 0. (This procedure assumes SUM(NA) is 0.)
  IF (narm) THEN
    DO i = 1, n
      IF (ISNAN(x(i))) THEN
        nan(i) = 0
        x(i) = 0
      END IF
    END DO
  ENDIF

  ! Type "lag"
  IF (windowtype == 1) THEN
    DO i = w, n
      sumx(i) = SUM(x((i - w + 1):i))
    END DO
  ENDIF

  ! Type "mid"
  IF (windowtype == 2) THEN
    whalf = (w - 1) / 2
    DO i = whalf + 1, n - whalf
      sumx(i) = SUM(x((i - whalf):(i + whalf)))
    END DO
  ENDIF

  ! Type "lead"
  IF (windowtype == 3) THEN
    DO i = 1, n - w + 1
      sumx(i) = SUM(x(i:(i + w - 1)))
    END DO
  ENDIF

END SUBROUTINE rolling_sum 
