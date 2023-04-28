program char_int32_err
    use, intrinsic :: iso_fortran_env
    use :: maps
    use :: errstat
    implicit none

    type(char_to_int32_map_type) :: map
    type(error_stat_type) :: status

    call map%initialize(status=status)

    call map%put("apple", 100, status)
    call map%put("apple", 200, status)
    if (error_occurred(status)) then
        print *, status%get_message()
    end if

    print *, map%get("apple") ! 100

    call map%remove_key("apple")
    call map%replace("apple", 100, status)
    if (has_message(status)) print *, status%get_message()

    call map%finalize(status)
end program char_int32_err
