module maps_common_proc
    use, intrinsic :: iso_fortran_env
    use :: stdlib_hashmap_wrappers, only:fnv_1_hasher, fnv_1a_hasher
    use :: stdlib_hashmaps, only:hashmap_type
    use :: stdlib_optval
    use :: stdlib_ascii
    implicit none
    private
    public :: initialize_map

contains
    subroutine initialize_map(map, hasher, slots_bits, status)
        implicit none
        class(hashmap_type), intent(inout) :: map
        character(*), intent(in), optional :: hasher
        integer(int32), intent(in), optional :: slots_bits
        integer(int32), intent(out), optional :: status

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
end module maps_common_proc
