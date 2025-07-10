module integrate
    use quadrule
    use constants, only: nquad, r_kind


    implicit none

    real(r_kind), dimension(nquad) :: her_x, her_w, lag_x, lag_w, leg_x, leg_w
    contains

    subroutine precompute_gauss()
        !!64 point gaussian quadrature.
        call hermite_ek_compute(nquad,her_x,her_w)
        !!Gets weights and locations for where to evaluate 64 point integral. 
        !!Integral must be of form 
        !! -infty < x < infty  dx f(x) * exp(-x^2)
        !! where x = alpha_z*z => z = x/alpha_z


        call laguerre_ss_compute(nquad, lag_x, lag_w)
        !!Gets weights and locations for where to evaluate 64 point integral. 
        !!Integral must be of form 
        !! 0 < x < infty  dx f(x) * exp(-x)
        !! where x = alpha^2 * rho^2

        
        call legendre_dr_compute(nquad, leg_x, leg_w)

    end subroutine

end module integrate