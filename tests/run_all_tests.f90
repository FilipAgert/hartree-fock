program run_tests
    use test_geom
    use test_sort
    implicit none
  
    print *, "==============================="
    print *, "  FORTRAN TEST SUITE RUNNER"
    print *, "==============================="
  
    call run_tests_geom()
    call run_tests_sort()
  
    print *, "==============================="
    print *, "         TESTING DONE"
    print *, "==============================="
  
  end program run_tests
  