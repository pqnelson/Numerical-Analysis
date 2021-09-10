module root
  use ISO_FORTRAN_ENV
  use IEEE_ARITHMETIC, only: ieee_is_negative
  implicit none
  private
  public bisection, fixed_point, newton

  interface
     real(real64) function func(x)
       use ISO_FORTRAN_ENV, only: real64
       implicit none
       real(real64), intent(in) :: x
     end function func
  end interface
contains
  function bisection(f, l, r, max_iter, tol)
    implicit none
    integer, intent(in) :: max_iter
    real(real64), intent(in) :: l, r, tol
    procedure(func) :: f
    logical :: is_left_neg
    integer :: j
    real(real64) :: bisection, midpoint, y, left, right
    
    y = f(l)
    is_left_neg = ieee_is_negative(y)
    
    left = l
    right = r
    do j=1, max_iter
       midpoint = 0.5*(left + right)
       y = f(midpoint)
       if (abs(y) < tol) then
          exit
       else if (ieee_is_negative(y) .eqv. is_left_neg) then
          left = midpoint
       else
          right = midpoint
       end if
    end do
    bisection = midpoint
  end function bisection

  function fixed_point(f, initial_guess, tol, max_iter, result)
    implicit none
    procedure(func) :: f
    real(real64), intent(in) :: initial_guess, tol
    integer, intent(in) :: max_iter
    real(real64), intent(inout) :: result
    integer :: j
    logical :: fixed_point
    real(real64) :: y, y_prev

    y_prev = f(initial_guess)
    do j=1,max_iter
       y = f(y_prev)
       if (abs(y - y_prev) < tol) then
          result = y
          fixed_point = .true.
          return
       end if
       y_prev = y
  end do
  fixed_point = .false.
 end function fixed_point

 function newton(f, df, initial_guess, tol, max_iter, result)
    implicit none
    procedure(func) :: f, df
    real(real64), intent(in) :: initial_guess, tol
    integer, intent(in) :: max_iter
    real(real64), intent(inout) :: result
    
    integer :: j
    logical :: newton
    real(real64) :: p, p_prev

    p_prev = initial_guess
    do j=1,max_iter
       p = p_prev - f(p_prev)/df(p_prev)
       if (abs(p - p_prev) < tol) then
          result = p
          newton = .true.
          return
       end if
       p_prev = p
   end do
   newton = .false.
 end function newton
end module root