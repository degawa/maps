module maps_common_proc_replace
    use, intrinsic :: iso_fortran_env
    use :: stdlib_hashmap_wrappers, only:key_type, other_type, set
    use :: stdlib_hashmaps, only:hashmap_type
    use :: errstat
    use :: maps_common_error_repository
    implicit none
    private
    public :: remove_and_append

    interface remove_and_append
        procedure :: replace_char_int32
    end interface
contains
    !>Remoevs the existing key-value mapping
    !>and then appends the key-new value mapping to the specified map.
    subroutine replace_char_int32(map, key, new_value, status)
        implicit none
        class(hashmap_type), intent(inout) :: map
            !! a map to which the specified key-value mapping is appended
        character(*), intent(in) :: key
            !! the key to mapping to the specified value
        integer(int32), intent(in) :: new_value
            !! the value to be mapped to the specified key
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        type(key_type) :: hash_key
        type(other_type) :: hash_value
        logical :: existed, conflict

        call set(hash_key, key)
        call map%remove(hash_key, existed)
        if (.not. existed) then
            call catch_error(err%key_not_exist, err, status)
            return
        end if

        call set(hash_value, new_value)
        call map%map_entry(hash_key, hash_value, conflict)
        if (conflict) then
            call catch_error(err%conflict, err, status)
            return
        end if

        call set_success(status)
    end subroutine replace_char_int32
end module maps_common_proc_replace
