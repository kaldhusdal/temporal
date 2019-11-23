SUBROUTINE successions(X, N, G, C)

  ! X: Group observations, e.g. group name, number etc. (it is assumed that the
  !    observations are correctly sorted)
  ! N: The number of observations, i.e. the cardinality of x.
  ! G: The unique X value per succession
  ! C: The counts of each succession
  IMPLICIT NONE
  INTEGER:: N
  INTEGER, DIMENSION(N):: X, G, C

  ! i: Looping index
  ! s: Succession counter
  ! current: Placeholder for the current value when looping through X
  ! previous: Placeholder for the previous value when looping through X
  INTEGER:: i, s, current, previous

!  ! Fill index vektor.
!  DO i = 1, N
!    A(i) = i
!  END DO

  s = 1
  G(s) = X(1)
  C(1) = 1
  DO i = 2, N
    current = X(i)
    previous = X(i - 1)

    IF (current == previous) THEN
      ! Increment counter as current observation is identical to previous
      C(s) = C(s) + 1
    ELSE
      s = s + 1
      G(s) = X(i)
      C(s) = 1
    END IF
    
  END DO


END SUBROUTINE successions


!based on either index or diff(index),
!algorithm that groups per x into sucessive groups,
!returns number of groups per x,
!amount in each group



!set.seed(29)
!df <- data.frame(i = 1:100,
!                 x = sample(LETTERS[1:3], 100, replace = TRUE),
!                 y = rpois(100, 5))


