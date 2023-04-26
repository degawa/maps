program hasher
    use, intrinsic :: iso_fortran_env
    use :: maps
    implicit none

    type(char_to_int32_map_type) :: map

    call map%initialize(hasher=hash_functions%FNV_1)
    call map%finalize()

    call map%initialize(hasher=hash_functions%FNV_1A)
    call map%finalize()

    call map%initialize(hasher=hash_functions%NMHASH32)
    call map%finalize()

    call map%initialize(hasher=hash_functions%NMHASH32X)
    call map%finalize()

    call map%initialize(hasher=hash_functions%WATER)
    call map%finalize()
end program hasher
