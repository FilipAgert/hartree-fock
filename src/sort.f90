module sort
    use constants, only: r_kind
    implicit none
    private
    public :: sort_insert

    interface sort_insert
        module procedure sort_insert_real
    end interface
    contains

    subroutine sort_insert_real(arr)
        real(r_kind), intent(inout) :: arr(:)
        integer :: ii, jj
        real(r_kind) :: key
        do ii = 2, size(arr)
            key = arr(ii)
            jj = ii-1

            do while(jj .ge. 1 .and. arr(jj) .gt. key)
                arr(jj+1) = arr(jj)
                jj = jj-1

            end do
            arr(jj+1) = key
        end do

    end subroutine
end module