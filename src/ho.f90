module ho

    use constants
    use integrate, only: lag_x
    use geom
    implicit none
    private
    public :: compute_laguerre, get_num_states_upto, get_states_upto, ho_state, modw, lnl
    real(r_kind), dimension(nquad, 0:N_max, 0:N_max) :: Lnl !!Generalized laguerre polynomials in radial solution of 3D H.O. Solution

    type :: ho_state
        integer :: n,l,ml, ms
    end type

    contains
    subroutine precompute()
        integer :: rr
        real(r_kind) :: eta, x(nquad)
        do rr = 1,nquad
            eta = lag_x(rr)
            x(rr) = eta*0.5_r_kind
        end do

        call compute_laguerre(x)
    end subroutine

    subroutine compute_laguerre(x)
        real(r_kind), dimension(nquad), intent(in) :: x !!For which x coordinates to evaluate gen. lag. poly at
        integer :: N, l, q
        Lnl = huge(r_kind)
        do N = 0, N_max
            do l = N, 0, -2
                do q = 1,nquad
                    Lnl(q,N,l) = lna(x(q), N, l+0.5_r_kind)
                end do
            end do
        end do
    end subroutine

    pure real(r_kind) elemental function lna(x,n,a) result(res) !gen laguerre polynomial iterative version
        real(r_kind), intent(in) :: x
        integer, intent(in) :: n
        real(r_kind), intent(in) :: a
        real(r_kind), dimension(0:n) :: lnas
        integer :: nn
        lnas(0) = 1.0_r_kind

        if(n > 0) then
            lnas(1) = 1.0_r_kind + real(a,r_kind) - x

            do nn = 2, n
                lnas(nn) = (real(2*nn-1+a,r_kind)-x)*lnas(nn-1) - (real(nn-1+a,r_kind))*lnas(nn-2)
                lnas(nn) = lnas(nn)/real(nn,r_kind)
            end do
        endif
        res = lnas(n)
    end function

    real(r_kind) function lnamod(eta,n,l) result(res) !modified laguerre polynomial (radial part in eta)
        real(r_kind), intent(in) :: eta
        integer, intent(in) :: n
        integer, intent(in) :: l
        real(r_kind) :: const
        const = modw(n,l)
        res = const   * eta**((l+0.5_r_kind)/2.0_r_kind) *Lna(eta/2, n, l+0.5_r_kind)


    end function
    real(r_kind) function modw(n,l) result(res) !modified laguerre polynomial constant term
        integer, intent(in) :: n
        integer, intent(in) :: l
        real(r_kind) :: const
        const = 2.0_r_kind**((n-0.5_r_kind)*0.5_r_kind) * sqrt(real(fac(n),r_kind))/sqrt(real(ffac(2*n+2*l+1),r_kind))
        res = const   


    end function



    pure real(r_kind) function E_ho(N, hbaromega)
        integer, intent(in) :: N
        real(r_kind), intent(in) :: hbaromega
        E_ho = hbaromega * (1.5_r_kind + N)
    end function

    function get_states_upto(N) result(states)
        integer, intent(in) :: N
        type(ho_state) :: states(get_num_states_upto(N))
        integer :: ii, l, nn, idx, mm, lb, ms, mj

        idx = 0
        write(*,*)"N n  l   m"
        do ii = 0, N
            do l = ii, 0, -2
                nn = (ii -l) / 2
                if(time_reversal_symmetry) then
                    lb = 0
                else
                    lb = -l
                endif
                do mm = lb, l, 1
                    do ms = -1, 1, 2
                        idx = idx + 1
                        !write(*,'(4I3)')ii,nn, l, mm
                        states(idx) = ho_state(n=nn, l = l, ml = mm, ms = ms) 
                    end do

                end do
            end do
        end do
        if(idx /= size(states)) then
            write(*,*) "Error in get_states_upto: Wrong number of H.O states generated:", size(states)," but generated ", idx, " states."
            write(*,*)" n  l   m"
            do ii = 1, idx
                write(*,'(3I3)')states(ii)%n, states(ii)%l, states(ii)%ml
            end do
            stop
        endif
    end function

    pure integer function get_num_states_upto(N) result(num)
        integer, intent(in) :: N
        num = (N+1)*(N+2)*(N+3)/3 
        if(time_reversal_symmetry) num = num / 2
    end function




end module ho