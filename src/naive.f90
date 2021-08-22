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
program test
  use ISO_FORTRAN_ENV
  implicit none
  if (.not.coherence_test()) then
     write(*,*) "ERROR: coherence test failed!"
  end if

  call wilkinson_test ()
contains
  ! a simple polynomial should agree with both methods
  function coherence_test ()
    use ISO_FORTRAN_ENV
    implicit none
    real(real64) :: x
    real(real64), dimension(0:3) :: y
    logical :: coherence_test
    y = (/2.0_real64,3.0_real64,4.0_real64,5.0_real64/)
    x = -3.0_real64
    write(*,*) "y(x) naive  = ", naive(y,x,4)
    write(*,*) "y(x) horner = ", horner(y,x,4)
    coherence_test = abs(naive(y,x,4) - horner(y,x,4)) < 1.0d-16
  end function coherence_test

  subroutine wilkinson_test ()
    use ISO_FORTRAN_ENV
    implicit none
    real(real64), dimension(0:19) :: base
    real(real64), dimension(0:20) :: wilkinson, zeros
    integer :: i,j

    zeros = (/ (j*0.0_real64, j=0,20) /)
    zeros(20) = 1
    base = (/ (j*1.0_real64, j=1,20) /)
    wilkinson(0) = 2432902008176640000.0_real64
    wilkinson(1) = -8752948036761600000.0_real64
    wilkinson(2) = 13803759753640704000.0_real64
    wilkinson(3) = -12870931245150988800.0_real64
    wilkinson(4) = 8037811822645051776.0_real64
    wilkinson(5) = -3599979517947607200.0_real64
    wilkinson(6) = 1206647803780373360.0_real64
    wilkinson(7) = -311333643161390640.0_real64
    wilkinson(8) = 63030812099294896.0_real64
    wilkinson(9) = -10142299865511450.0_real64
    wilkinson(10) = 1307535010540395.0_real64
    wilkinson(11) = -135585182899530.0_real64
    wilkinson(12) = 11310276995381.0_real64
    wilkinson(13) = -756111184500.0_real64
    wilkinson(14) = 40171771630.0_real64
    wilkinson(15) = -1672280820.0_real64
    wilkinson(16) = 53327946.0_real64
    wilkinson(17) = -1256850.0_real64
    wilkinson(18) = 20615.0_real64
    wilkinson(19) = -210.0_real64
    wilkinson(20) = 1.0_real64
    write(*,*) "\begin{longtable}{r|l|l}"
    write(*,*) "$x$ & Naive Evaluation & Horner Evaluation & Horner with Base \\\hline"
    do i=1,20
       write(*,*) "$", i, "$ & $", naive(wilkinson,1.0_real64*i,20),"$ & $",horner(wilkinson,1.0_real64*i,20), &
            "$ & $", horner_base(zeros, base, 1.0_real64*i,20), "$ \\"
    end do
    write(*,*) "\hline"
    write(*,*) "$x$ & Naive Evaluation & Horner Evaluation & Horner with Base\\\hline"
    write(*,*) "\end{longtable}"
    write(*,*) "$", 10.001_real64, "$ & $", naive(wilkinson, 10.001_real64, 20), "$ & $",horner(wilkinson,10.001_real64,20),&
    "$ & $",horner_base(zeros,base,10.001_real64,20), "$ \\"
    call binary_print(horner_base(zeros,base,10.001_real64,20))
  end subroutine wilkinson_test

  function get_elt(p, j, n)
    use ISO_FORTRAN_ENV
    implicit none
    integer, intent(in) :: n, j
    real(real64), intent(in) :: p(0:n)
    real(real64) :: get_elt
    get_elt = p(j)
  end function get_elt

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

  subroutine binary_print(x)
    use ISO_FORTRAN_ENV
    implicit none
    real(real64) :: x
    character(len=64) :: bin

    write(bin, '(B64.64)') x

    print *, 'Sign Exponent     Mantissa '
    print *, bin(1:1), '    ', bin(2:12), '  ', bin(13:64)
  end subroutine binary_print

end program test
