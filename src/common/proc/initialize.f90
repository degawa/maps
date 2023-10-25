module maps_common_proc_initialize
    use, intrinsic :: iso_fortran_env
    use :: stdlib_hashmap_wrappers, only: &
        fnv_1_hasher, &
        fnv_1a_hasher, &
        seeded_nmhash32_hasher, &
        seeded_nmhash32x_hasher, &
        seeded_water_hasher
    use :: stdlib_hashmaps, only:hashmap_type, success
    use :: maps_common_type_hashFunction
    use :: errstat
    use :: maps_common_error_repository
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
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        integer(int32) :: stat
        type(hash_function_type) :: hasher_
        hasher_ = optval(hasher, &
                         default=Hash_Function_FNV_1)

        stat = success
        select case (hasher_%enum)
        case (Hash_Function_FNV_1%enum)
            call map%init(fnv_1_hasher, slots_bits, stat)

        case (Hash_Function_FNV_1A%enum)
            call map%init(fnv_1a_hasher, slots_bits, stat)

        case (Hash_Function_NMHASH32%enum)
            call map%init(seeded_nmhash32_hasher, slots_bits, stat)

        case (Hash_Function_NMHASH32X%enum)
            call map%init(seeded_nmhash32x_hasher, slots_bits, stat)

        case (Hash_Function_WATER%enum)
            call map%init(seeded_water_hasher, slots_bits, stat)

        case default
            call catch_error(err%unkwnon_hash_function, err, status)
            return
        end select

        ! catch the error occurred in the init procedure of hashmap_type
        if (stat /= success) then
            call catch_init_error()
            return
        end if
    contains
        subroutine catch_init_error()
            use :: stdlib_hashmaps, only:alloc_fault, array_size_error
            use :: maps_common_error_task_appendMessage
            implicit none
            select case (stat)
            case (alloc_fault)
                call catch_error(err%error_in_hashmap_type, err, status, &
                                 append_message_task("allocation failure in init"))

            case (array_size_error)
                call catch_error(err%error_in_hashmap_type, err, status, &
                                 append_message_task("incorrect slot bits"))

            case default
                call catch_error(err%error_in_hashmap_type, err, status, &
                                 append_message_task("unknown error"))
            end select
        end subroutine catch_init_error
    end subroutine initialize_map
end module maps_common_proc_initialize
