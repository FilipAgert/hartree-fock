module constants

    integer, parameter :: r_kind = 8
    integer, parameter :: nquad = 32 !!number of Gaussian quadrature integration points

    integer, parameter :: verbose = 0 !!Level of printing.

    integer, parameter :: maxiter = 50 !!maximum iterations of H.F. method before aborting


    ! ++++++++++++++++++++++++++++++ Harmonic oscillator parameters ++++++++++++++++++++++++++++++
    integer, parameter :: N_max = 4 !!maximum harmonic oscillator shell
    logical, parameter :: time_reversal_symmetry = .false.
    integer, parameter :: num_part = 2









    !+++++++++++++++++++++++++++++++ natural constants ++++++++++++++++++++++++++
    real(r_kind), parameter :: hbarc = 197.327! [MeV fm]
    real(r_kind), parameter :: mass_p = 938.27208943
    real(r_kind), parameter :: mass_e = 0.5109989461
    real(r_kind), parameter :: mass_n = 939.56542194
    real(r_kind), parameter :: dalton = 931.49410372
    real(r_kind), parameter :: d =0.5461 !! fm
    real(r_kind), parameter :: e_squared = 1.4399764_r_kind !! MeV fm
    real(r_kind), parameter :: pi = ACOS(-1.0_r_kind)
    real(r_kind), parameter :: pi2 = pi*pi
    real(r_kind), parameter :: epsilonzero = 0.0552634936 !! e^2 / (MeV * fm)
end module