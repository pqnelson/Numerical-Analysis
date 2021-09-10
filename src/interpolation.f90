module interpolation
  use ISO_FORTRAN_ENV
  use IEEE_ARITHMETIC, only: ieee_is_negative
  implicit none
  private
  public newton

  interface
     real(real64) function func(x)
       use ISO_FORTRAN_ENV, only: real64
       implicit none
       real(real64), intent(in) :: x
     end function func
  end interface
contains
  ! function lagrange(xs, ys, points)
  !   implicit none
  !   real(real64), intent(in) :: xs(:), ys(:), points(:)
  !   integer :: n = size(points)
  !   real(real64), dimension(n) :: la
  !   ! assert size(xs) = size(ys)
  ! end function lagrange

  function newton(xs, ys, n)
    implicit none
    real(real64), intent(in) :: xs(:), ys(:)
    integer, intent(in) :: n
    real(real64) :: newton(1:n)
    real(real64) :: coefs(1:n,1:n)
    integer :: i, j
    coefs(1:n,1) = ys(1:n)
    newton(1) = ys(1)
    ! coefs(j,i) = f[x(i..i+j)]
    do i=2,n ! ranging over columns
       do j=1,1+n-i ! ranging over rows
          ! coefs(i,j) = (coefs(i-1,j+1) - coefs(i-1,j))/(xs(i+j-1) - xs(j))
          coefs(j,i) = (coefs(j+1,i-1) - coefs(j,i-1))/(xs(i+j-1) - xs(j))
       end do
       newton(i) = coefs(1,i)
    end do
  end function newton

  subroutine exp5_newton()
    implicit none
    real(real64) :: xs(1:5), ys(1:5), coefs(1:5), expected(1:5)
    integer :: i
    expected = (/ 0.36787944117144233_real64, 0.47730243708238218_real64, &
         0.30963624349235097_real64, 0.13391174488878152_real64, &
         4.3435698652960625E-002_real64 /)
    xs = (/ (0.5_real64*i, i=-2,2) /)
    ys = exp(xs)
    coefs = newton(xs,ys,size(ys))
    write (*,*) "COMPUTED coefficients:"
    write (*,*) coefs
    write (*,*) "EXPECTED:"
    write (*,*) expected
    ! call horner_eval(coefs, xs(2:5), x), for example
  end subroutine exp5_newton
end module