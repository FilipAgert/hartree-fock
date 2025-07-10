module geom !!some helper functions e.g. clebsh-gordan, factorial etc
    use constants, only: r_kind
    implicit none


    contains

    pure elemental integer(8) recursive function fac(n) result(f)
        integer, intent(in) :: n
        if(n == 0 .or. n == 1) then
            f = 1
            return
        endif
        f = f * fac(n-1)
    end function

    pure elemental integer(8) recursive function ffac(n) result(f) !! double factorial (!!)
        integer, intent(in) :: n
        if(n == 0 .or. n == 1) then
            f = 1
            return
        endif
        f = f * fac(n-2)
    end function

end module geom