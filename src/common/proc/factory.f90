module maps_common_proc_factory
    use :: stdlib_hashmaps, only:hashmap_type, chaining_hashmap_type, open_hashmap_type
    implicit none
    private
    public :: hashmap_factory

contains
    !>Returns an instance of hashmap type
    function hashmap_factory(hashmap) result(map)
        use :: stdlib_optval
        use :: stdlib_ascii
        implicit none
        character(*), intent(in), optional :: hashmap
            !! the name of the hashmap type
        class(hashmap_type), allocatable :: map
            !! an instance of the hashmap type

        character(:), allocatable :: hashmap_str
        hashmap_str = to_lower(optval(hashmap, "chaining"))

        select case (hashmap_str)
        case ("chaining")
            allocate (chaining_hashmap_type :: map)
        case ("open")
            allocate (open_hashmap_type :: map)
        case default
            allocate (chaining_hashmap_type :: map)
        end select
    end function hashmap_factory
end module maps_common_proc_factory
