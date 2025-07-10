program main
    use constants
    use ho
    use geom
    use pot
    use integrate
    implicit none
    
    type(ho_state), allocatable :: states(:)
    real(r_kind), allocatable :: V(:,:,:,:)
    integer :: sz, ii, jj
    character(len=50) :: wr
    sz = get_num_states_upto(N_max)
    allocate(states(sz), V(sz,sz,sz,sz))
    states = get_states_upto(N_max)
    call precompute()
    V = V_mat(1.0_r_kind, states)

    write(wr, '(A,I5,A)')'(' , sz, 'E15.3)'
    write(*,*) sz
    do ii = 1,sz
        write(*,wr) V(6,ii,4,:)
    end do
    write(*,*) "Max:", maxval(V)

    



    
    contains

    subroutine precompute()

        call precompute_gauss()
        call precompute_ho()
    end subroutine
end program