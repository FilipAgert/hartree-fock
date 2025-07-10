program main
    use constants
    use ho
    use geom
    use pot
    use hf
    use integrate
    implicit none
    
    type(ho_state), allocatable :: states(:)
    real(r_kind), allocatable :: V(:,:,:,:), T(:,:)
    integer :: sz, ii, jj
    character(len=50) :: wr
    sz = get_num_states_upto(N_max)
    allocate(states(sz), V(sz,sz,sz,sz), T(sz,sz))
    states = get_states_upto(N_max)
    call precompute()
    V = V_mat(40.0_r_kind, states)
    T = 0
    do ii = 1,sz
        T(ii,ii) = E_ho(states(ii)%ml + 2* states(ii)%n,1.0_r_kind)
    end do

    write(wr, '(A,I5,A)')'(' , sz, 'E15.3)'
    write(*,*) sz
    do ii = 1,sz
        write(*,wr) V(1,ii,1,:)
    end do
    write(*,*) "Max:", maxval(V)

    

    call main_loop(T, V)

    
    contains

    subroutine precompute()

        call precompute_gauss()
        call precompute_ho()
    end subroutine
end program