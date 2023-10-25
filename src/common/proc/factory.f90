module maps_common_proc_factory
    use :: stdlib_hashmaps, only:hashmap_type, chaining_hashmap_type, open_hashmap_type
    use :: maps_common_type_collisionResolver
    use :: errstat
    use :: maps_common_error_repository
    implicit none
    private
    public :: hashmap_factory

contains
    !>Returns an instance of hashmap type
    function hashmap_factory(collision_resolver, status) result(map)
        implicit none
        type(collision_resolver_type), intent(in), optional :: collision_resolver
            !! the collision resolution algorithm enumerator of the hashmap
        class(hashmap_type), allocatable :: map
            !! an instance of the hashmap type
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        type(collision_resolver_type) :: collision_resolver_
        collision_resolver_ = optval(collision_resolver, &
                                     default=Collision_Resolver_Separate_Chaining)

        select case (collision_resolver_%enum)
        case (Collision_Resolver_Separate_Chaining%enum)
            allocate (chaining_hashmap_type :: map)

        case (Collision_Resolver_Open_Addressing%enum)
            allocate (open_hashmap_type :: map)

        case default
            call catch_error(err%unkwnon_collision_resolver, err, status)
            return
        end select
    end function hashmap_factory
end module maps_common_proc_factory
