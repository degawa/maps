module maps_common_proc_append
    use, intrinsic :: iso_fortran_env
    use :: stdlib_hashmap_wrappers, only:key_type, other_type, set
    use :: stdlib_hashmaps, only:hashmap_type
    use :: store_proc
    implicit none
    private
    public :: append

    interface append
        procedure :: append_char_int32
    end interface
contains
    !>Appends the key-value mapping to the specified map.
    subroutine append_char_int32(map, key, value, status)
        implicit none
        class(hashmap_type), intent(inout) :: map
            !! a map to which the specified key-value mapping is appended
        character(*), intent(in) :: key
            !! the key to mapping to the specified value
        integer(int32), intent(in) :: value
            !! the value to be mapped to the specified key
        integer(int32), intent(out), optional :: status
            !! the status of the operation

        type(key_type) :: hash_key
        type(other_type) :: hash_value
        logical :: conflict

        call store(status, 0)

        call set(hash_key, key)
        call set(hash_value, value)
        call map%map_entry(hash_key, hash_value, conflict)
        if (conflict) then
            call store(status, 2)
        end if
    end subroutine append_char_int32
end module maps_common_proc_append
