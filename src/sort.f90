module sort
    use constants, only: r_kind
    implicit none
    private
    public :: sort_insert, sort_by, sort_1_2

    interface sort_insert
        module procedure sort_insert_real, sort_insert_int
    end interface

    interface sort_by
        module procedure sort_by_insert_real, sort_by_insert_int
    end interface


    interface sort_1_2 !!Sort elements first by first array, then if equality, by second array
        module procedure sort_1_then_2_int
    end interface
    contains

    subroutine sort_insert_real(arr)
        real(r_kind), intent(inout) :: arr(:)
        integer :: ii, jj
        real(r_kind) :: key
        do ii = 2, size(arr)
            key = arr(ii)
            jj = ii-1

            do while (jj >= 1)
                if (arr(jj) <= key) exit
                arr(jj+1) = arr(jj)
                jj = jj - 1
            end do
            

            arr(jj+1) = key
        end do
    end subroutine


    subroutine sort_insert_int(arr)
        integer, intent(inout) :: arr(:)
        integer :: ii, jj
        integer :: key
        do ii = 2, size(arr)
            key = arr(ii)
            jj = ii-1

            do while (jj >= 1)
                if (arr(jj) <= key) exit
                arr(jj+1) = arr(jj)
                jj = jj - 1
            end do
            arr(jj+1) = key
        end do
    end subroutine

    subroutine sort_by_insert_real(arr1,arr2) !!sort arr1 and arr2 by arr1
        real(r_kind), intent(inout) :: arr1(:), arr2(:)
        integer :: ii, jj
        real(r_kind) :: key, k2
        do ii = 2, size(arr1)
            key = arr1(ii)
            k2 = arr2(ii)
            jj = ii-1

            do while (jj >= 1)
                if (arr1(jj) <= key) exit
                arr1(jj+1) = arr1(jj)
                arr2(jj+1) = arr2(jj)
                jj = jj-1

            end do
            arr1(jj+1) = key
            arr2(jj+1) = k2
        end do
    end subroutine

    subroutine sort_by_insert_int(arr1,arr2) !!sort arr1 and arr2 by arr1
        integer, intent(inout) :: arr1(:), arr2(:)
        integer :: ii, jj
        integer :: key, k2
        do ii = 2, size(arr1)
            key = arr1(ii)
            k2 = arr2(ii)
            jj = ii-1


            do while (jj >= 1)
                if (arr1(jj) <= key) exit
                arr1(jj+1) = arr1(jj)
                arr2(jj+1) = arr2(jj)
                jj = jj-1

            end do
            arr1(jj+1) = key
            arr2(jj+1) = k2
        end do
    end subroutine

    subroutine sort_1_then_2_int(arr1,arr2) !!sort arr1 and arr2 by first arr1, then by arr2 for equal elements
        integer, intent(inout) :: arr1(:), arr2(:)
        integer :: idx, val, stopidx
        call sort_by(arr1, arr2)

        do idx = 1, size(arr1) - 1
            val = arr1(idx)
            stopidx = idx+1
            do while(arr1(stopidx) == val .and. stopidx .le. size(arr1)) !Get elements with same value as n1
                stopidx = stopidx + 1
            end do
            stopidx = stopidx - 1

            !For elements with equality, sort again, only this time by arr2
            call sort_by(arr2(idx:stopidx), arr1(idx:stopidx))

        end do
    end subroutine
end module