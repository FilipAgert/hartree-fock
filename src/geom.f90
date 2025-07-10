module geom !!some helper functions e.g. clebsh-gordan, factorial etc
    use constants, only: r_kind
    implicit none

    private

    INTERFACE cg
        MODULE PROCEDURE cg_real,cg_int
    END INTERFACE
    public :: cg, fac, ffac

    contains

    integer(8) function fac(n) result(f)
        integer, intent(in) :: n
        integer(8), save :: fc(0:169)
        logical, save :: first = .true.
        integer :: ii
        if(n < 0) THEN
            write(*,*) "error: negative argument for factorial"
            stop
        endif
        IF (first) THEN
            fc(0) = 1
            fc(1) = 1
            DO ii = 2, 169
               fc(ii) = fc(ii-1)*ii
            END DO
            first = .FALSE.
        END IF

        f = fc(n)
    end function

    integer(8) function ffac(n) result(f) !! double factorial (!!)
            
        integer, intent(in) :: n
        integer(8), save :: fc(0:169)
        logical, save :: first = .true.
        integer :: ii

        IF (first) THEN
            fc(0) = 1
            fc(1) = 1
            fc(2) = 2
            do ii = 3, 169
                fc(ii) = fc(ii-2) * ii
            end do
            first = .FALSE.
        END IF

        f = fc(n)
    end function


    FUNCTION cg_real(fj1,fm1,fj2,fm2,fj) RESULT(cg)
    
        ! Clebsch-Gordan coefficient, REAL(8) parameters.
        
        IMPLICIT REAL(r_kind) (a-h, o-z)
        IMPLICIT integer (i-m)
    
        REAL(r_kind) :: cg 
    
        REAL(r_kind), SAVE :: fc(170)
        LOGICAL, SAVE :: first=.TRUE.
    
        IF (first) THEN
           fc(1) = 1.d0 
           DO i = 1, 169
              i1 = i+1 
              fc(i1) = fc(i)*i 
           END DO
           first = .FALSE.
        END IF
        cg = 0.d0 
        jm1 = INT(fj1-fm1+1.01d0) 
        jp1 = INT(fj1+fm1+1.01d0) 
        jm2 = INT(fj2-fm2+1.01d0) 
        jp2 = INT(fj2+fm2+1.01d0) 
        j12 = INT(fj1+fj2-fj+1.01d0) 
        j13 = INT(fj1+fj-fj2+1.01d0) 
        j23 = INT(fj2+fj-fj1+1.01d0) 
        jm = INT(fj-fm1-fm2+1.01d0) 
        jp = INT(fj+fm1+fm2+1.01d0) 
        IF (MIN(jm1, jp1, jm2, jp2, j12, j13, j23, jm, jp) < 1) RETURN
        j123 = INT(fj1+fj2+fj+2.01d0) 
        jjm1 = INT((fj-fj2+fm1)*1.01d0) 
        jjm2 = INT((fj-fj1-fm2)*1.01d0) 
        izx = MIN(j12, jm1, jp2) 
        izn = MAX(0,-jjm1,-jjm2)+1 
        sum = 0.d0 
        sn = -(-1.0d0)**izn 
        DO iz1 = izn, izx 
           iz = iz1-1 
           sum = sum+sn / (fc (iz1)*fc (j12-iz)*fc (jm1-iz)     &
               *fc (jp2-iz)*fc (jjm1+iz1)*fc (jjm2+iz1) )          
           sn =-sn
        END DO
        ff = (2*fj+1)*fc(jp1)*fc(jm1)*fc(jp2)*fc(jm2)     &
            *fc(jp)*fc(jm)*fc(j12)*fc(j13)*fc(j23) / fc(j123)  
        cg = sum*SQRT(ff)
    
        RETURN 
    
      END FUNCTION cg_real

      FUNCTION cg_int(j1,m1,j2,m2,j) RESULT(cg)

        ! Clebsch-Gordan coefficient, INTEGER parameters.
    
        IMPLICIT NONE
    
        INTEGER, INTENT(in) :: j1,m1,j2,m2,j
        REAL(r_kind) :: cg,fj1,fm1,fj2,fm2,fj
    
        fj1 = REAL(j1,kind=8)
        fm1 = REAL(m1,kind=8)
        fj2 = REAL(j2,kind=8)
        fm2 = REAL(m2,kind=8)
        fj = REAL(j,kind=8)
    
        cg = cg_real(fj1,fm1,fj2,fm2,fj)
    
      END FUNCTION cg_int

end module geom