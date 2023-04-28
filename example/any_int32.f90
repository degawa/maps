program any_int32
    use, intrinsic :: iso_fortran_env
    use :: maps
    implicit none

    type(any_to_int32_map_type) :: map
    call map%initialize()

    call map%put(100, 200)
    print *, map%get(100) ! 200

    call map%replace(100, 300)
    print *, map%get(100) ! 300

    call map%replace_if(100, 300, 150)
    call map%remove_if(100, 150)

    print *, map%is_empty() ! T

    call map%finalize()
end program any_int32
