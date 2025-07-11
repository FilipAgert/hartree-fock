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
    integer :: A = num_part
    real(r_kind) :: hbo
    sz = get_num_states_upto(N_max)
    allocate(states(sz), V(sz,sz,sz,sz), T(sz,sz))
    states = get_states_upto(N_max)
    call precompute()
    write(*,*) "Number of states in diagonalisation: ", sz
    !write(*,*) "Maximum matrix element:", maxval(V)
    write(*,*) "size dim 2:", size(lnl,2)
    !write(*,*) lnl(1,3,1)
    hbo = A**(-1.0/3.0) * 41.0_r_kind
    V = V_mat(40.0_r_kind, states, mass_n, hbo)
    T = 0

    do ii = 1,sz
        T(ii,ii) = E_ho(states(ii)%ml + 2* states(ii)%n,hbo)
    end do


    ! do ii = 1,sz
    !     write(*,wr) V(1,ii,1,:)
    ! end do

    

    call main_loop(T, V)

    
    contains

    subroutine precompute()

        call precompute_gauss()
        call precompute_ho()
    end subroutine
end program