module pot
    use constants
    use ho
    use integrate

    implicit none


    contains
    function delta_pot_mat_elem(s1, s2, s3, s4, V0)result(elem) ! <s1,s2|V|s3,s4>
        type(ho_state), intent(in) :: s1,s2,s3,s4
        real(r_kind), intent(in) :: V0
        real(r_kind) :: elem

    end function

    function radial_overlap_integral(n1,l1,n2,l2,n3,l3,n4,l4) result(val) !!Compute integral dr r^4 * R_1 R_2 R_3 R_4 from 0 to infty
        integer, intent(in) :: n1,l1,n2,l2,n3,l3, n4,l4 !!quantum numbers
        real(r_kind) :: val, eta, w1,w2,w3,w4
        integer :: rr
        w1 = modw(n1,l1)
        w2 = modw(n2,l2)
        w3 = modw(n3,l3)
        w4 = modw(n4,l4)
        
        val = 0
        do rr = 1, nquad !!radial overlap integral for delta potential
            eta = lag_x(rr)
            val = val + lag_w(rr) * eta**((l1+l2+l3+l4)*0.5_r_kind) * eta * lnl(rr,n1,l1) * lnl(rr,n2,l2) * lnl(rr,n3,l3) * lnl(rr,n4,l4) 

        end do

        val = val * w1 * w2 *w3 *w4
    end function


end module pot