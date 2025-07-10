module hartree
    use constants
    implicit none

    contains


    pure function dens(coeff)result(mat) !!density matrix from coefficient matrix
        real(r_kind),intent(in), dimension(:,:) :: coeff !coefficient matrix dim(numstates, numparts). 
        real(r_kind), dimension(size(coeff,1), size(coeff,1)) :: mat
        integer :: ii, jj, p
        mat = 0
        do ii = 1, size(mat,1)
            do jj = 1, size(mat,2)
                do p = 1, num_part
                    mat(ii,jj) = mat(ii,jj) + coeff(ii,p)* coeff(jj,p)

                end do
            end do
        end do
    end function

    pure real(r_kind) function tr(A) !!trace of a matrix
        real(r_kind), intent(in), dimension(:,:) :: A
        integer :: ii
        tr = 0
        do ii = 1, size(A)
            tr = tr + A(ii,ii)
        end do
    end function

    pure function gamma_mat(V,rho)!!self-consistent field
        real(r_kind), dimension(:,:,:,:), intent(in) :: V !!two body interaction potential
        real(r_kind), dimension(:,:),  intent(in) :: rho !!density matrix
        real(r_kind), dimension(size(rho,1),size(rho,2)) :: gamma_mat !!self consistent field matrix
        integer :: ii, jj, ll, kk
        gamma_mat = 0
        do ll = 1, size(gamma_mat,1)
            do kk = 1, size(gamma_mat,2)
                do ii = 1, size(rho,1)
                    do jj = 1, size(rho,2)
                        gamma_mat(ll,kk) = gamma_mat(ll,kk) + V(ll, jj, kk, ii) * rho(ii,jj)
                    end do
                end do
            end do
        end do
        
    end function

    subroutine diagonalize(E, V, H) !!Diagnoalize H and return V and E as eigenvectors sorted with lowest energy first.
        real(r_kind), intent(in) :: H(:,:)
        real(r_kind), intent(out) :: E(size(H,1)), V(size(H,1),size(H,1))
        external :: dsyev !!lapack routine for diagnoalizing symmetric matrix

        !!lapack variables!!
        character(len=1), parameter :: jobz = 'V', uplo = 'U'
        integer :: N
        integer :: lda
        real(r_kind) :: w(size(H,1))
        real(r_kind), allocatable :: work(:)
        integer :: lwork
        integer :: info

        N = size(H,1)
        ! write(*,*) N
        lda = N
        lwork = 20*N
        allocate(work(lwork))
        call dsyev(jobz, uplo, N, H, lda, w, work, lwork, info)

        if (info /= 0) then
            write(*,'(A,I10)') "Error using dsyev in diagonalisation, info:", info
            call exit
        endif
        if(W(1) > W(size(E))) then !Reverse order if wrong order
            E = W(size(W):1:-1) 
            V = H(:,size(W):1:-1)
        else
            E = w
            V = H
        endif
        deallocate(work)

    end subroutine


end module hartree