module pot !!module to create the two body interaction matrix
    use constants
    use ho, only: ho_state, modw, lnl
    use integrate
    use geom
    use sort

    implicit none
    

    contains
    function delta_pot_mat_elem(s1, s2, s3, s4, V0, mass, hbaromega)result(elem) ! <s1,s2|V|s3,s4>
        type(ho_state), intent(in) :: s1,s2,s3,s4
        real(r_kind), intent(in) :: V0
        real(r_kind) :: elem, mass, hbaromega, nu
        real(r_kind) :: radial, ang

        nu = mass* hbaromega/(hbarc*hbarc)


        ang = angular_integral(s1%l, s1%ml, s2%l, s2%ml, s3%l, s3%ml, s4%l, s4%ml)
        
        if(ang .eq. 0.0_r_kind) then
            elem = 0
        else
            !write(*,*) "ang:", ang
            radial = radial_overlap_integral(s1%n, s1%l, s2%n, s2%l, s3%n, s3%l, s4%n, s4%l, nu) !!radial part
            elem = ang*radial * (-V0)
        endif
    end function

    function V_mat(V0, states, mass, hbaromega) !!create matrix
        real(r_kind), intent(in) :: V0, mass, hbaromega
        type(ho_state), intent(in) :: states(:)
        type(ho_state) :: s1,s2,s3,s4
        integer :: aa, bb, cc, dd
        real(r_kind) :: V_mat(size(states,1), size(states,1), size(states,1), size(states,1)), hg
        hg = huge(hg)
        V_mat= hg
        do aa = 1, size(states)
            s1 = states(aa)
            do bb = 1, size(states)
                s2 = states(bb)
                do cc = 1, size(states)
                    s3 = states(cc)
                    do dd = 1, size(states)
                        s4 = states(dd)

                        if(V_mat(cc,dd,aa,bb) < hg) then !transpose
                            V_mat(aa,bb,cc,dd) = V_mat(cc,dd,aa,bb)
                        elseif(V_mat(cc,dd,bb,aa) < hg) then !transpose
                            V_mat(aa,bb,cc,dd) = -V_mat(cc,dd,bb,aa)
    

                        elseif(V_mat(bb,aa,dd,cc) < hg) then !switch particles
                            V_mat(aa,bb,cc,dd) = V_mat(bb,aa,dd,cc)
                        elseif(V_mat(bb,aa,cc,dd) < hg) then !switch particles
                            V_mat(aa,bb,cc,dd) = -V_mat(bb,aa,cc,dd)

                        elseif(V_mat(dd,cc,bb,aa) < hg) then !switch particles and transpose
                            V_mat(aa,bb,cc,dd) = V_mat(dd,cc,bb,aa)
                        elseif(V_mat(dd,cc,aa,bb) < hg) then !switch particles and transpose
                            V_mat(aa,bb,cc,dd) = -V_mat(dd,cc,aa,bb)
                        elseif(V_mat(aa,bb,dd,cc) < hg) then !switch particles and transpose
                            V_mat(aa,bb,cc,dd) = -V_mat(aa,bb,dd,cc)
                        else
                            
                            V_mat(aa,bb,cc,dd) = delta_pot_mat_elem(s1,s2,s3,s4,v0, mass, hbaromega) - delta_pot_mat_elem(s1,s2,s4,s3,v0, mass, hbaromega)
                        endif

                    end do
                end do
            end do
        end do

    end function

    subroutine qnbr_to_idx(idx,n1,l1,n2,l2,n3,l3,n4,l4)
        integer, intent(in) :: n1,l1,n2,l2,n3,l3, n4,l4 !!quantum numbers
        integer, intent(out) :: idx
        !Order quantum numbers in decreasing order n1 .ge. n2 .ge. n3 .ge. n4
        integer, dimension(4) :: ns, ls
        integer(8) :: num_n_config, num_l_config, num_config, idx1, idx2
        ns = [n1,n2,n3,n4]
        ls = [l1,l2,l3,l4]
        call sort_by(ns,ls) !!sorts by ns and then ls
        num_n_config = choose(N_max/2 + 4, 4) !!How many combinations of n1,n2,n3,n4?
        num_l_config = (N_max+1)**4
        num_config = num_n_config*num_l_config

        idx1 = choose(n1+3,4) + choose(n2+2,3) + choose(n3+1,2) + choose(n4,1)

        idx2 = l1*((N_max+1)**3) + l2 * (N_max+1)**2 + l3*(N_max+1) + l4

        idx = idx1 * num_l_config + idx2
    end subroutine

    function radial_overlap_integral(n1,l1,n2,l2,n3,l3,n4,l4, nu) result(val) !!Compute integral dr r^4 * R_1 R_2 R_3 R_4 from 0 to infty
        integer, intent(in) :: n1,l1,n2,l2,n3,l3, n4,l4 !!quantum numbers
        real(r_kind) :: val, eta, w1,w2,w3,w4, nu
        integer :: rr, num_n, num_l, num_c, idx
        real(r_kind), dimension(:), allocatable, save :: mat
        logical,save, allocatable :: first_time(:)
        logical, save :: first = .true.
        if(first) then
            num_n = choose(N_max/2 + 4, 4) !!How many combinations of n1,n2,n3,n4?
            num_l= (N_max+1)**4
            num_c = num_n*num_l
            allocate(first_time(0:num_c-1), mat(0:num_c-1))
            first_time = .true.
            first = .false.
        endif
        call qnbr_to_idx(idx,n1,l1,n2,l2,n3,l3,n4,l4)
        if(first_time(idx)) then


        else
            
        endif

        !write(*,*) "Radial integral:"
        w1 = 2.0_r_kind**(n1/2.0) * sqrt(fac(n1)/ffac(2*n1+2*l1+1))
        w2 = 2.0_r_kind**(n2/2.0) * sqrt(fac(n2)/ffac(2*n2+2*l2+1))
        w3 = 2.0_r_kind**(n3/2.0) * sqrt(fac(n3)/ffac(2*n3+2*l3+1))
        w4 = 2.0_r_kind**(n4/2.0) * sqrt(fac(n4)/ffac(2*n4+2*l4+1))
        !write(*,*) w1, w2, w3, w4
        val = 0
        do rr = 1, nquad 
            eta = lag_x(rr)
            !if(n1 .ge. 4.or. n2 .ge. 4 .or. n3 .ge. 4 .or. n4 .ge. 4)            write(*,*) n1, n2, n3 ,n4

            if(lnl(rr,n1,l1) > 1e6 .or. lnl(rr,n2,l2) > 1e6.or.lnl(rr,n3,l3) > 1e6 .or. lnl(rr,n4,l4)> 1e6 ) then
                write(*,*) "Err: lnl out of bounds:", lnl(rr,n1,l1), lnl(rr,n2,l2),lnl(rr,n3,l3),lnl(rr,n4,l4)
                stop
            endif
            val = val + lag_w(rr) * eta**((l1+l2+l3+l4)*0.5_r_kind) * eta**(1.5_r_kind) * lnl(rr,n1,l1) * lnl(rr,n2,l2) * lnl(rr,n3,l3) * lnl(rr,n4,l4) 
        end do
        val = val * w1 * w2 * w3 * w4 *2.0 * sqrt(nu)/ pi
    end function

    real(r_kind) function angular_integral(l1,m1,l2,m2,l3,m3,l4,m4) result(val) !!Compute integrl Y1* Y2* Y3 Y4 over all angles
        integer, intent(in) :: l1,m1,l2,m2,l3,m3,l4,m4 !!quantum numbers
        integer :: l13, u13, l24, u24, uL, lL, L
        if(m4-m2 /= m3-m1) then
            val = 0
            return
        endif
        l13 = abs(l3-l1)
        u13 = l3+l1
        l24 = abs(l4-l2)
        u24 = l4+l2

        lL = max(l13, l24)
        uL = min(u13, u24)

        val = 0

        do L = ll, uL
            val = val + 1.0/(2*L+1) * cg(l1, -m1, l3, m3, L) * cg(l1,0,l3,0,L) * cg(l2,0,l4,0,L) * cg(l2,-m2,l4,m4,L)
        end do 

        val = val * sqrt(real((2*l1+1)*(2*l2+1)*(2*l3+1)*(2*l4+1),r_kind)) / (4*pi)

        if(mod(m1+m2,2) == 1) val = val*(-1.0)  !phase
        

    end function


end module pot