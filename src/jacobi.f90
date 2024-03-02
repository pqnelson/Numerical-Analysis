module jacobi
  use ISO_FORTRAN_ENV
  implicit none

contains
  
  function vec_infty_norm_diff(n, x, y)
    use ISO_FORTRAN_ENV
    implicit none
    integer, intent(in) :: n
    real(real64), intent(in) :: x(1:n), y(1:n)
    real(real64) :: val, diff, vec_infty_norm_diff
    integer :: i
    val = abs(x(1) - y(1))
    do i=2,n
       diff = abs(x(i) - y(i))
       if (diff > val) then
          val = diff
       end if
    end do
    vec_infty_norm_diff = val
  end function vec_infty_norm_diff

  ! TODO: I should really have a routine which will check if A has
  ! any zero on its diagonal and, if so, pivot. This particular
  ! subroutine would be called ASSUMING that A has no zero on the diagonal.
  subroutine jacobi_iterate(n, A, b, init_guess, tol, max_iter, ans)
    use ISO_FORTRAN_ENV
    implicit none
    integer, intent(in) :: n
    real(real64), dimension(n,n), intent(in) :: A
    real(real64), dimension(n), intent(in) :: b, init_guess
    real(real64), intent(in) :: tol
    integer, intent(in) :: max_iter
    real(real64), dimension(n), intent(inout) :: ans
    integer :: i, j, k
    real(real64), dimension(n) :: x, prev_iter
    prev_iter = init_guess
    do k=1,max_iter
       do concurrent (i=1:size(x))
          x(i) = b(i)
          do j=1,n
             if (i.ne.j) then
                x(i) = x(i) - A(i,j)*prev_iter(j)
             end if
          end do
          x(i) = x(i)/A(i,i)
       end do
       
       if (vec_infty_norm_diff(n, x, prev_iter) < tol) then
          ans = x
          exit
       end if
       prev_iter = x
    end do
    ans = x
  end subroutine jacobi_iterate

  subroutine gauss_seidel(n, A, b, init_guess, tol, max_iter, ans)
    use ISO_FORTRAN_ENV
    implicit none
    integer :: n
    real(real64), dimension(n,n), intent(in) :: A
    real(real64), dimension(n), intent(in) :: b, init_guess
    real(real64), intent(in) :: tol
    integer, intent(in) :: max_iter
    real(real64), dimension(n), intent(inout) :: ans
    integer :: i, j, k
    real(real64), dimension(n) :: x, prev_iter
    prev_iter = init_guess
    do k=1,max_iter
       x = prev_iter
       do i=1,n
          x(i) = b(i)
          do j=1,n
             if (i.ne.j) then
                x(i) = x(i) - A(i,j)*x(j)
             end if
          end do
          x(i) = x(i)/A(i,i)
       end do
       
       if (vec_infty_norm_diff(n, x, prev_iter) < tol) then
          ans = x
          exit
       end if
       prev_iter = x
    end do
    ans = x
  end subroutine gauss_seidel
end module jacobi

program test_jacobi
  use ISO_FORTRAN_ENV
  use jacobi
  implicit none

  if (burden_faires_ex_7_3_1_jacobi()) then
     print *, "[+] Jacobi iteration success on Burden-Faires 7.3.1"
  else
     print *, "[-] Jacobi iteration failed on Burden-Faires 7.3.1"
  end if
  
  if (burden_faires_ex_7_3_1_gauss_seidel()) then
     print *, "[+] Gauss-Seidel iteration success on Burden-Faires 7.3.1"
  else
     print *, "[-] Gauss-Seidel iteration failed on Burden-Faires 7.3.1"
  end if

contains
  ! Test functions
  function burden_faires_ex_7_3_1_jacobi() result(success)
    use ISO_FORTRAN_ENV
    implicit none
    real(real64), dimension(4,4) :: A
    real(real64), dimension(4) :: b, init_guess, x, expected
    real(real64) :: tol
    logical :: success
    integer :: i
    A = reshape((/ 10, -1, 2, 0, -1, 11, -1, 3, 2, -1, 10, -1, 0, 3, -1, 8 /), (/4,4/))
    b = (/6,25,-11,15/)
    init_guess = (/ 0, 0, 0, 0 /)
    tol = 1e-4
    call jacobi_iterate(4, A, b, init_guess, tol, 10, x)
    tol = 1.828118e-3 ! The actual difference is smaller than this
    expected = (/ 1.001, 1.998, -0.998, 0.9998 /)
    success = .TRUE.
    do i=1,4
       success = success .AND. (abs(x(i) - expected(i)) < tol)
    end do
  end function burden_faires_ex_7_3_1_jacobi

  function burden_faires_ex_7_3_1_gauss_seidel() result(success)
    use ISO_FORTRAN_ENV
    implicit none
    real(real64), dimension(4,4) :: A
    real(real64), dimension(4) :: b, init_guess, x, expected
    real(real64) :: tol
    logical :: success
    integer :: i
    
    A = reshape((/ 10, -1, 2, 0, -1, 11, -1, 3, 2, -1, 10, -1, 0, 3, -1, 8 /), (/4,4/))
    b = (/6,25,-11,15/)
    init_guess = (/ 0, 0, 0, 0 /)
    tol = 1e-7
    call gauss_seidel(4, A, b, init_guess, tol, 10, x)

    expected = (/ 1.00, 2.0, -1.0, 1.0 /)
    success = .TRUE.
    do i=1,4
       success = success .AND. (abs(x(i) - expected(i)) < tol)
    end do
  end function burden_faires_ex_7_3_1_gauss_seidel
  
end program test_jacobi
