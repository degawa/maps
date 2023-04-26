module maps_common_type_collisionResolver
    use, intrinsic :: iso_c_binding
    use :: enumul
    implicit none
    private
    public :: optval

    enum, bind(c)
        enumerator :: chaining
        enumerator :: open
    end enum

    !>Typed enumerator for collision resolution algorithm
    type, public, extends(enum_atype) :: collision_resolver_type
    end type collision_resolver_type

    type(collision_resolver_type), public, parameter :: &
        Collision_Resolver_Separate_Chaining = collision_resolver_type(chaining)
        !! the enumerator representing the separate chaining
    type(collision_resolver_type), public, parameter :: &
        Collision_Resolver_Open_Addressing = collision_resolver_type(open)
        !! the enumerator representing the open addressing

    !>list of typed enumerator for collision resolution algorithm
    type :: collision_resolver_list
        type(collision_resolver_type) :: Separate_Chaining
            !! the enumerator representing the separate chaining
        type(collision_resolver_type) :: Open_Addressing
            !! the enumerator representing the open addressing
    end type collision_resolver_list

    type(collision_resolver_list), public, parameter :: &
        collision_resolvers = collision_resolver_list( &
                                Separate_Chaining = Collision_Resolver_Separate_Chaining, &
                                Open_Addressing   = Collision_Resolver_Open_Addressing &
                              ) !&
        !! typed enumerator list of the collision resolution algorithms

    interface optval
        procedure :: optval_collision_resolver
    end interface
contains
    !>Returns `x` if it is present
    !>and returns `default` if it is not present.
    function optval_collision_resolver(x, default) result(y)
        implicit none
        type(collision_resolver_type), intent(in), optional :: x
            !! the result if `x` is present
        type(collision_resolver_type), intent(in) :: default
            !! the result if `x` is not present
        type(collision_resolver_type) :: y
            !! the result

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_collision_resolver
end module maps_common_type_collisionResolver
