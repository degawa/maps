program char_int32
    use, intrinsic :: iso_fortran_env
    use :: maps
    implicit none

    type(char_to_int32_map_type) :: map

    call map%initialize()

    call map%put("apple", 100)
    call map%put("orange", 150)
    call map%put("banana", 200)

    print *, map%get("banana") ! 200

    print *, map%get_or_default("orange", 0), map%get_or_default("berry", 500)

    print *, map%contains("orange") ! T
    call map%remove_if("orange", 100)
    print *, map%contains("orange") ! T
    call map%remove_if("orange", 150)
    print *, map%contains("orange") ! F

    call map%replace("apple", 300)
    print *, map%get("apple") ! 300

    call map%replace_if("banana", 300, map%get("apple")/2)
    print *, map%get("banana") ! 200

    call map%replace_if("banana", 200, map%get("apple")/2)
    print *, map%get("banana") ! 150

    call map%finalize()
end program char_int32
