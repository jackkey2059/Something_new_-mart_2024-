module Solution
  implicit none
contains
  integer pure function nextHigher(n) result (res)
    integer, intent(in) :: n
    !> ----
    integer :: tmp, pow2
    real :: test
    
    pow2 = 0
    res = n
    do while(res /= 1)
      tmp = mod(res, 2)
      res = res/2
      if (tmp == 1 .and. mod(res, 2) == 0) exit
      pow2 = pow2 + 1
    end do
    
    test = 2.**(log(n+2.)/log(2.))
    if (test == n+2.) then
      res = 1
      pow2 = pow2 - 1
    else
      res = 0
    end if
    
    res = res + n + 2**pow2    
  end function nextHigher
end module Solution