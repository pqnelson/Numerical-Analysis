! Cross-compiling instructions:
! arm-linux-gnueabihf-gfortran -g -c -S -fverbose-asm -O0 naive.f90 -o naive-arm.s
! parc64-linux-gnu-gfortran -g -c -S -fverbose-asm -O0 naive.f90 -o naive-sparc64.s
! riscv64-linux-gnu-gfortran-9 -g -c -S -fverbose-asm -O0 naive.f90 -o naive-riscv.s
! gfortran -march=native -masm=intel -S -fverbose-asm -O0 -c naive.f90 -o naive.s
! gfortran -g -c -S -fverbose-asm -O0 naive.f90 -o naive-x64.s

! sudo apt-get install gfortran-riscv64-linux-gnu
! sudo apt-get install gfortran-arm-linux-gnueabihf gfortran-aarch64-linux-gnu
! sudo apt-get install gfortran-sparc64-linux-gnu
! sudo apt-get install gfortran-mips-linux-gnu
! sudo apt-get install gfortran-powerpc64-linux-gnu gfortran-alpha-linux-gnu
module polynomial
  use ISO_FORTRAN_ENV
  implicit none
contains
  function horner_base(p, b, x0, n)
    use ISO_FORTRAN_ENV
    implicit none
    integer, intent(in) :: n
    real(real64), intent(in) :: x0, p(0:n), b(0:n-1)
    real(real64) :: horner_base, r
    integer :: j

    r = p(n)
    do j=1,n
       r = r*(x0 - b(n-j)) + p(n-j)
    end do
    horner_base = r
  end function horner_base

  function horner(p, x0, n)
    use ISO_FORTRAN_ENV
    implicit none
    integer, intent(in) :: n
    real(real64), intent(in) :: x0, p(0:n)
    real(real64) :: r, horner
    integer :: j

    r = p(n)
    do j=1,n
       r = r*x0 + p(n-j)
    end do
    horner = r
  end function horner

  function naive(p, x0, n)
    use ISO_FORTRAN_ENV
    implicit none
    integer, intent(in) :: n
    real(real64), intent(in) :: x0, p(0:n)
    real(real64) :: r, x, naive
    integer :: j
    r = p(0)
    x = 1.0_real64
    do j=1,n
       x = x*x0
       r = r + x*p(j)
    end do
    naive = r
  end function naive

end program test
