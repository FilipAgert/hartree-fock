module test_geom
    use constants
    use test_utils
    use geom
    implicit none
    private
    public :: run_tests_geom

    contains

    subroutine run_tests_geom()
        implicit none
        integer :: n_passed = 0, n_failed = 0

        print *, "Running fitting tests..."
        call test_cg(n_passed, n_failed)
        call test_fac(n_passed, n_failed)
        print *, "fitting results: ", n_passed, " passed,", n_failed, " failed"
    end subroutine

    subroutine test_cg(n_passed, n_failed)
        integer :: n_passed, n_failed

        real(r_kind) :: j1, j2, j, m1, m2
        real(r_kind) :: expected, actual
        logical :: pass
        expected = 1
        j1 = 0.0_r_kind
        j2 = 1.0_r_kind
        j = 1.0_r_kind
        m1 = 0.0_r_kind
        m2 = 0.0_r_kind
        actual =  cg(j1,m1, j2,m2,j)
        pass = eq_r(expected, actual)
        if(.not. pass) then
            n_failed = n_failed + 1
            write(*,*) "test_cg failed: expected:", expected, " actual: ", actual
        else
            n_passed = n_passed + 1

        endif

        j1 = 1.0_r_kind
        j2 = 1.0_r_kind
        j = 2.0_r_kind
        m1 = 0.0_r_kind
        m2 = 0.0_r_kind
        expected = sqrt(2.0/3.0)
        actual = cg(j1,m1, j2,m2,j)
        pass = eq_r(expected, actual)
        if(.not. pass) then
            n_failed = n_failed + 1
            write(*,*) "test_cg failed: expected:", expected, " actual: ", actual
        else
            n_passed = n_passed + 1

        endif


    end subroutine


    subroutine test_fac(n_passed, n_failed)
        integer :: n_passed, n_failed
        integer :: n
        integer :: expected, actual
        logical :: pass
        expected = 1
        actual = fac(0)

        pass = actual==expected

        if(.not. pass) then
            n_failed = n_failed + 1
            write(*,*) "test_fac failed: expected:", expected, " actual: ", actual
        else
            n_passed = n_passed + 1

        endif

        expected = 1
        actual = fac(1)

        pass = actual==expected

        if(.not. pass) then
            n_failed = n_failed + 1
            write(*,*) "test_fac failed: expected:", expected, " actual: ", actual
        else
            n_passed = n_passed + 1

        endif

        expected = 2
        actual = fac(2)

        pass = actual==expected

        if(.not. pass) then
            n_failed = n_failed + 1
            write(*,*) "test_fac failed: expected:", expected, " actual: ", actual
        else
            n_passed = n_passed + 1

        endif


        expected = 6
        actual = fac(3)

        pass = actual==expected

        if(.not. pass) then
            n_failed = n_failed + 1
            write(*,*) "test_fac failed: expected:", expected, " actual: ", actual
        else
            n_passed = n_passed + 1

        endif

        expected = 39916800
        actual = fac(11)

        pass = actual==expected

        if(.not. pass) then
            n_failed = n_failed + 1
            write(*,*) "test_fac failed: expected:", expected, " actual: ", actual
        else
            n_passed = n_passed + 1

        endif


    end subroutine
        

end module test_geom