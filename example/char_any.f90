program char_any
    use, intrinsic :: iso_fortran_env
    use :: maps
    implicit none

    type(char_to_any_map_type) :: map

    call map%initialize()

    call map%put("int8.max", huge(0_int8))
    call map%put("int16.max", huge(0_int16))
    call map%put("int32.max", huge(0_int32))
    call map%put("int64.max", huge(0_int64))
    call map%put("real32.max", huge(0._real32))
    call map%put("real64.max", huge(0._real64))

    print *, as_int32(map%get("int32.max")) ! 2147483647
    print *, as_real64(map%get("real64.max")) ! 1.7976931348623157E+308

    call map%remove_if("real64.max", -huge(0._real64))
    print *, map%contains("real64.max") ! T
end program char_any
