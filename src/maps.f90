module maps
    use :: map_char_int32, only:char_to_int32_map_type
    use :: map_char_any, only:char_to_any_map_type
    use :: map_any_int32, only:any_to_int32_map_type
    use :: map_bitset_int32, only:bitset_to_int32_map_type
    use :: maps_common_type_mappable, only: &
        as_int8, &
        as_int16, &
        as_int32, &
        as_int64, &
        as_real32, &
        as_real64, &
        as_real128, &
        as_complex32, &
        as_complex64, &
        as_complex128, &
        as_logical, &
        as_char
    use :: maps_common_type_hashFunction, only:hash_functions
    use :: maps_common_type_collisionResolver, only:collision_resolvers
    implicit none
    ! user-defined types
    public :: char_to_int32_map_type
    public :: char_to_any_map_type
    public :: any_to_int32_map_type
    public :: bitset_to_int32_map_type
    ! procedures
    public :: as_int8, &
              as_int16, &
              as_int32, &
              as_int64, &
              as_real32, &
              as_real64, &
              as_real128, &
              as_complex32, &
              as_complex64, &
              as_complex128, &
              as_logical, &
              as_char
    ! typed enumerator
    public :: hash_functions
    public :: collision_resolvers
end module maps
