module maps_common_proc_initialize
    use, intrinsic :: iso_fortran_env
    use :: stdlib_hashmap_wrappers, only:fnv_1_hasher, fnv_1a_hasher
    use :: stdlib_hashmaps, only:hashmap_type
    use :: stdlib_optval
    use :: stdlib_ascii
    implicit none
    private
    public :: initialize_map

contains
    !>Initializes an instance of the hashmap.
    subroutine initialize_map(map, hasher, slots_bits, status)
        implicit none
        class(hashmap_type), intent(inout) :: map
            !! an instance of a map
        character(*), intent(in), optional :: hasher
            !! the name of the hash function
        integer(int32), intent(in), optional :: slots_bits
            !! the number of bits initially used to map to the slots
        integer(int32), intent(out), optional :: status
            !! the status of the operation

        character(:), allocatable :: hasher_str
        hasher_str = to_upper(optval(hasher, ""))

        select case (hasher_str)
        case ("FNV_1")
            call map%init(fnv_1_hasher, slots_bits, status)
        case ("FNV_1A")
            call map%init(fnv_1a_hasher, slots_bits, status)
        case default
            call map%init(fnv_1_hasher, slots_bits, status)
        end select
    end subroutine initialize_map
end module maps_common_proc_initialize
