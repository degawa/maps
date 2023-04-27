module maps_common_proc_get
    use :: stdlib_hashmap_wrappers, only:key_type, other_type, set, get
    use :: stdlib_hashmaps, only:hashmap_type
    use :: errstat
    use :: maps_common_error_repository
    implicit none
    private
    public :: get_other

    interface get_other
        procedure :: get_other_char_key
        procedure :: get_other_int8_key
    end interface
contains
    !>Gets data as `class(*)` from the map.
    subroutine get_other_char_key(map, key, other, status)
        implicit none
        class(hashmap_type), intent(inout) :: map
            !! a map in which the value is retrieved
        character(*), intent(in) :: key
            !! the key to mapping to the specified value
        class(*), allocatable, intent(out) :: other
            !! the value in any intrinsic type,
            !! mapped to the specified key, represented as `class(*)`
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        type(key_type) :: hash_key
        type(other_type) :: hash_other
        logical :: exists

        call set(hash_key, trim(key))
        call map%key_test(hash_key, exists)
        if (.not. exists) then
            call catch_error(err%key_not_exist, err, status)
            return
        end if

        call map%get_other_data(hash_key, hash_other, exists)
        if (.not. exists) then
            call catch_error(err%value_not_exist, err, status)
            return
        end if

        call get(hash_other, other)
        call set_success(status)
    end subroutine get_other_char_key

    !>Gets data as `class(*)` from the map.
    subroutine get_other_int8_key(map, key, other, status)
        use, intrinsic :: iso_fortran_env
        implicit none
        class(hashmap_type), intent(inout) :: map
            !! a map in which the value is retrieved
        integer(int8), intent(in) :: key(:)
            !! the key to mapping to the specified value
        class(*), allocatable, intent(out) :: other
            !! the value in any intrinsic type,
            !! mapped to the specified key, represented as `class(*)`
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        type(key_type) :: hash_key
        type(other_type) :: hash_other
        logical :: exists

        call set(hash_key, key)
        call map%key_test(hash_key, exists)
        if (.not. exists) then
            call catch_error(err%key_not_exist, err, status)
            return
        end if

        call map%get_other_data(hash_key, hash_other)
        if (.not. exists) then
            call catch_error(err%value_not_exist, err, status)
            return
        end if

        call get(hash_other, other)
        call set_success(status)
    end subroutine get_other_int8_key
end module maps_common_proc_get
