module test_utils
    use constants, only: r_kind
    real(r_kind), parameter :: epsilon = 1e-6
    contains
    logical function eq_r(r1,r2, tol)
        real(r_kind), intent(in) :: r1, r2
        real(r_kind), optional :: tol
        real(r_kind) :: tolerance
        if(.not. present(tol)) then
            tolerance = epsilon
        else
            tolerance = tol
        endif

        if(abs(r1-r2) < tolerance) then
            eq_r = .true.
        else
            eq_r = .false.
        endif
    end function

end module