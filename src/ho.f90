module ho

    use constants
    implicit none
    private
    public :: compute_laguerre
    real(r_kind), dimension(nquad, 0:N_max, 0:N_max) :: L_n_l !!Generalized laguerre polynomials


    contains

    subroutine compute_laguerre(x)
        real(r_kind), dimension(nquad), intent(in) :: x !!For which x coordinates to evlauate gen. lag. poly at
        integer :: N, l, q
        L_n_l = huge(r_kind)
        do N = 0, N_max
            do l = N, 0, -2
                do q = 1,nquad
                    L_n_l(q,N,l) = lna(x(q), N, l+0.5_r_kind)
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




end module ho