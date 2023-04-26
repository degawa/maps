module maps_common_proc_initialize
    use, intrinsic :: iso_fortran_env
    use :: stdlib_hashmap_wrappers, only: &
        fnv_1_hasher, &
        fnv_1a_hasher, &
        seeded_nmhash32_hasher, &
        seeded_nmhash32x_hasher, &
        seeded_water_hasher
    use :: stdlib_hashmaps, only:hashmap_type
    use :: maps_common_type_hashFunction
    implicit none
    private
    public :: initialize_map

contains
    !>Initializes an instance of the hashmap.
    subroutine initialize_map(map, hasher, slots_bits, status)
        implicit none
        class(hashmap_type), intent(inout) :: map
            !! an instance of a map
        type(hash_function_type), intent(in), optional :: hasher
            !! the hash function enumerator
        integer(int32), intent(in), optional :: slots_bits
            !! the number of bits initially used to map to the slots
        integer(int32), intent(out), optional :: status
            !! the status of the operation

        type(hash_function_type) :: hasher_
        hasher_ = optval(hasher, &
                         default=Hash_Function_FNV_1)

        select case (hasher_%enum)
        case (Hash_Function_FNV_1%enum)
            call map%init(fnv_1_hasher, slots_bits, status)

        case (Hash_Function_FNV_1A%enum)
            call map%init(fnv_1a_hasher, slots_bits, status)

        case (Hash_Function_NMHASH32%enum)
            call map%init(seeded_nmhash32_hasher, slots_bits, status)

        case (Hash_Function_NMHASH32X%enum)
            call map%init(seeded_nmhash32x_hasher, slots_bits, status)

        case (Hash_Function_WATER%enum)
            call map%init(seeded_water_hasher, slots_bits, status)

        end select
    end subroutine initialize_map
end module maps_common_proc_initialize
