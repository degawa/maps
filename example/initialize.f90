program char_int32
    use, intrinsic :: iso_fortran_env
    use :: maps
    implicit none

    type(char_to_int32_map_type) :: map
    call map%initialize(collision_resolvers%Separate_Chaining, &
                        hash_functions%NMHASH32)

    call map%put("apple", 200)
    call map%put("orange", 250)
    call map%put("banana", 300)

    print *, map%get("banana") ! 300
    call map%finalize()
end program char_int32
