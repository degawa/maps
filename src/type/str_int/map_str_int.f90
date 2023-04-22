module map_str_int
    use, intrinsic :: iso_fortran_env
    use :: stdlib_hashmaps, only:hashmap_type, chaining_hashmap_type
    use :: stdlib_hashmap_wrappers, only:key_type, other_type
    use :: maps_common_proc_toHashKey
    use :: maps_common_proc_toHashValue
    use :: maps_common_proc_get
    use :: maps_common_proc_append
    implicit none
    private

    type, public :: map_str_int_type
        class(hashmap_type), allocatable :: map
    contains
        procedure, public, pass :: put

        procedure, public, pass :: get_value
        procedure, public, pass :: get_or_default
        generic :: get => get_value, get_or_default

        procedure, public, pass :: size
        procedure, public, pass :: is_empty
        procedure, public, pass :: contains_key
        generic :: contains => contains_key

        procedure, public, pass :: remove_key
        procedure, public, pass :: remove_if

        ! procedure, public, pass :: contains_value
        ! procedure, public, pass :: put_all
        ! procedure, public, pass :: clear
        ! procedure, public, pass :: keys
        ! procedure, public, pass :: values
        ! procedure, public, pass :: equals
        ! procedure, public, pass :: hash_code
        ! procedure, public, pass :: put_if_absent
        ! procedure, public, pass :: replace
        ! procedure, public, pass :: merge
        procedure, public, pass :: initialize
    end type map_str_int_type

contains
    subroutine put(this, key, value, status)
        implicit none
        class(map_str_int_type), intent(inout) :: this
        character(*), intent(in) :: key
        integer(int32), intent(in) :: value
        integer(int32), intent(out), optional :: status

        call append(this%map, key, value, status)
    end subroutine put

    function get_value(this, key) result(val)
        implicit none
        class(map_str_int_type), intent(inout) :: this
        character(*), intent(in) :: key
        integer(int32) :: val

        class(*), allocatable :: other
        call get_other(this%map, key, other)

        select type (other); type is (integer(int32))
            val = other
        end select
    end function get_value

    function get_or_default(this, key, default) result(val)
        implicit none
        class(map_str_int_type), intent(inout) :: this
        character(*), intent(in) :: key
        integer(int32), intent(in) :: default
        integer(int32) :: val

        val = this%get_value(key)
        if (val /= default) &
            val = default
    end function get_or_default

    function size(this)
        implicit none
        class(map_str_int_type), intent(in) :: this
        integer(int32) :: size

        size = this%map%entries()

        ! return int32 maximum value if overflwoing (greater than them max value of int32)
        if (size < 0) then
            size = huge(size)
        end if
    end function size

    function is_empty(this)
        implicit none
        class(map_str_int_type), intent(in) :: this
        logical :: is_empty
        is_empty = (this%size() == 0)
    end function is_empty

    function contains_key(this, key)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        class(map_str_int_type), intent(inout) :: this
        character(*), intent(in) :: key
        logical :: contains_key

        call this%map%key_test(to_hash_key(key), contains_key)
    end function contains_key

    subroutine remove_key(this, key, status)
        use :: stdlib_hashmap_wrappers, only:set
        use :: store_proc
        implicit none
        class(map_str_int_type), intent(inout) :: this
        character(*), intent(in) :: key
        integer(int32), intent(out), optional :: status

        logical :: existed

        call this%map%remove(to_hash_key(key), existed)
        if (existed) then
            call store(status, 0)
        else
            call store(status, 1)
        end if
    end subroutine remove_key

    subroutine remove_if(this, key, value, status)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        class(map_str_int_type), intent(inout) :: this
        character(*), intent(in) :: key
        integer(int32), intent(in) :: value
        integer(int32), intent(out), optional :: status

        integer(int32) :: mapped_value
        mapped_value = this%get(key)

        if (mapped_value == value) then
            call this%remove_key(key, status)
        end if
    end subroutine remove_if

    subroutine initialize(this, hashmap, hasher, slots_bits, status)
        use :: stdlib_hashmaps, only:chaining_hashmap_type, open_hashmap_type
        use :: maps_common_proc
        use :: stdlib_optval
        use :: stdlib_ascii
        implicit none
        class(map_str_int_type), intent(inout) :: this
        character(*), intent(in), optional :: hashmap
        character(*), intent(in), optional :: hasher
        integer(int32), intent(in), optional :: slots_bits
        integer(int32), intent(out), optional :: status

        character(:), allocatable :: hashmap_str

        hashmap_str = to_lower(optval(hashmap, "chaining"))

        select case (hashmap_str)
        case ("chaining")
            allocate (chaining_hashmap_type :: this%map)
        case ("open")
            allocate (open_hashmap_type :: this%map)
        case default
            allocate (chaining_hashmap_type :: this%map)
        end select

        call initialize_map(this%map, hasher, slots_bits, status)
    end subroutine initialize
end module map_str_int
