program main
    use constants
    use ho
    implicit none
    
    type(ho_state), allocatable :: states(:)
    allocate(states(get_num_states_upto(N_max)))
    states = get_states_upto(N_max)
    write(*,*) "r_kind", r_kind

end program