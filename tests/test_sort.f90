module test_sort
    use sort
    implicit none
    private
    public :: run_tests_sort

    contains

    subroutine run_tests_sort()
        implicit none
        integer :: n_passed = 0, n_failed = 0

        print *, "Running sort tests..."
        call test_sortarr(n_passed, n_failed)
        call test_sort2arr(n_passed,n_failed)
        call test_sort2arr2(n_passed,n_failed)
        print *, "sort results: ", n_passed, " passed,", n_failed, " failed"
    end subroutine

    subroutine test_sortarr(n_passed, n_failed)
        integer :: n_passed, n_failed

        integer :: expected, actual
        integer :: arr(4)
        logical :: pass
        arr = [2 ,3, 1, 4]
        expected = 1
        call sort_insert(arr)
        
        actual = arr(1)
        pass = actual == expected

        if(.not. pass) then
            n_failed = n_failed + 1
            write(*,*) "test_sort failed: expected at (1):", expected, " actual: ", arr
        else
            n_passed = n_passed + 1

        endif


    end subroutine

    subroutine test_sort2arr(n_passed, n_failed)
        integer :: n_passed, n_failed

        integer :: expected, actual
        integer :: arr(4), arr2(4)
        logical :: pass
        arr = [2 ,3, 1, 4]
        arr2 = [5, 1, 2, 5]

        call sort_by(arr, arr2)
        expected = 5
        actual = arr2(2)
        pass = actual == expected

        if(.not. pass) then
            n_failed = n_failed + 1
            write(*,*) "test_sort failed: expected at (2):", expected, " actual: ", arr2
        else
            n_passed = n_passed + 1

        endif


    end subroutine

    subroutine test_sort2arr2(n_passed, n_failed)
        integer :: n_passed, n_failed

        integer :: expected, actual
        integer :: arr(4), arr2(4)
        logical :: pass
        arr = [2,1,1,5]
        arr2 = [10,4,2,2]

        call sort_1_2(arr, arr2)
        expected = 2
        actual = arr2(1)
        pass = actual == expected

        if(.not. pass) then
            n_failed = n_failed + 1
            write(*,*) "test_sort failed: expected at (1):", expected, " actual: ", arr2
        else
            n_passed = n_passed + 1

        endif

        expected = 4
        actual = arr2(2)
        pass = actual == expected

        if(.not. pass) then
            n_failed = n_failed + 1
            write(*,*) "test_sort failed: expected at (2):", expected, " actual: ", arr2
        else
            n_passed = n_passed + 1

        endif


    end subroutine

        

end module test_sort