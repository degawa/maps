module maps
    use :: map_char_int32
    use :: map_char_any
    use :: map_any_int32
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
end module maps
