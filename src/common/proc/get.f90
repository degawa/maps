module maps_common_proc_get
    use :: stdlib_hashmap_wrappers, only:key_type, other_type, set, get
    use :: stdlib_hashmaps, only:hashmap_type
    implicit none
    private
    public :: get_other

    interface get_other
        procedure :: get_other_char
    end interface
contains
    !>Gets data as `class(*)` from the map.
        implicit none
        class(hashmap_type), intent(inout) :: map
            !! a map in which the value is retrieved
        character(*), intent(in) :: key
            !! the key to mapping to the specified value
        class(*), allocatable, intent(out) :: other
            !! the value in any intrinsic type,
            !! mapped to the specified key, represented as `class(*)`

        type(key_type) :: hash_key
        type(other_type) :: hash_other

        call set(hash_key, trim(key))
        call map%get_other_data(hash_key, hash_other)
        call get(hash_other, other)
    end subroutine get_other_char
end module maps_common_proc_get
