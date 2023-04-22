program map_str_int
    use, intrinsic :: iso_fortran_env
    use :: maps
    implicit none

    type(map_str_int_type) :: map

    call map%initialize("chaining")

    call map%put("apple", 100)
    call map%put("orange", 150)
    call map%put("banana", 200)

    print *, map%get("banana")
end program map_str_int
