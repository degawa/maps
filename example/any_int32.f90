program any_int32
    use, intrinsic :: iso_fortran_env
    use :: maps
    implicit none

    type(any_to_int32_map_type) :: map

    call map%initialize()

    call map%put(100, 200)

    print *, map%get(100) ! 200
end program any_int32
