module maps_common_proc_get
    use :: stdlib_hashmap_wrappers, only:key_type, other_type, get
    use :: stdlib_hashmaps, only:hashmap_type
    implicit none
    private
    public :: get_other

    interface get_other
        procedure :: get_other_char
    end interface
contains
    !>gets data from the hashmap
    subroutine get_other_char(map, key, other)
        use :: stdlib_hashmap_wrappers, only:set, get
        implicit none
        class(hashmap_type), intent(inout) :: map
        character(*), intent(in) :: key
        class(*), allocatable, intent(out) :: other

        type(key_type) :: hash_key
        type(other_type) :: hash_other

        call set(hash_key, trim(key))
        call map%get_other_data(hash_key, hash_other)
        call get(hash_other, other)
    end subroutine get_other_char
end module maps_common_proc_get
