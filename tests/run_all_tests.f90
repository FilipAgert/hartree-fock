program run_tests
    use test_geom
    implicit none
  
    print *, "==============================="
    print *, "  FORTRAN TEST SUITE RUNNER"
    print *, "==============================="
  
    call run_tests_geom()
  
    print *, "==============================="
    print *, "         TESTING DONE"
    print *, "==============================="
  
  end program run_tests
  