module map_char_int32
    use, intrinsic :: iso_fortran_env
    use :: stdlib_hashmaps, only:hashmap_type, chaining_hashmap_type
    use :: stdlib_hashmap_wrappers, only:key_type, other_type
    use :: maps_common_proc_toHashKey
    use :: maps_common_proc_toHashValue
    implicit none
    private

    !>User-defined type for mapping keys of `character(*)`
    !>to `integer(int32)` values.
    !>
    !>Dupulicated keys are not allowed.
    type, public :: char_to_int32_map_type
        class(hashmap_type), allocatable, private :: map
            !! hashmap
    contains
        procedure, public, pass :: put
        !* maps a value to a key.

        procedure, public, pass :: get_value
        !* returns the value mapped to a key.
        procedure, public, pass :: get_or_default
        !* returns the value mapped to a key or
        !  returns the specified value as a default
        !  if the key is not contained.
        generic :: get => get_value, get_or_default

        procedure, public, pass :: size
        !* returns the number of key-value mappings.
        procedure, public, pass :: is_empty
        !* returns true if the map is empty.

        procedure, public, pass :: contains_key
        !* returns true if the map contains the key.
        generic :: contains => contains_key

        procedure, public, pass :: remove_key
        !* removes the key-value mapping.
        procedure, public, pass :: remove_if
        !* removes the key-value mapping
        !  only if the key is mapped to the specific value.

        procedure, public, pass :: initialize
        !* initialize the instance of `char_to_int32_map_type`.
        procedure, public, pass :: finalize
        !* finalize the instance of `char_to_int32_map_type`.
    end type char_to_int32_map_type

contains
    !>Maps the specified value to the specified key in the map.
    subroutine put(this, key, value, status)
        use :: maps_common_proc_append
        implicit none
        class(char_to_int32_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to mapping to the specified value
        integer(int32), intent(in) :: value
            !! the value to be mapped to the specified key
        integer(int32), intent(out), optional :: status
            !! the status of the operation

        call append(this%map, key, value, status)
    end subroutine put

    !>Returns the value to which the specified key is mapped.
    !>The value is undefined if the type of `value` in the map
    !>is not `integer(int32)`.
    function get_value(this, key) result(val)
        use :: maps_common_proc_get
        implicit none
        class(char_to_int32_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to mapping to the returned value
        integer(int32) :: val
            !! the value to be mapped to the specified key

        class(*), allocatable :: other
        call get_other(this%map, key, other)

        select type (other); type is (integer(int32))
            val = other
        end select
    end function get_value

    !>Returns the value to which the specified key is mapped or
    !>returns the specified value as a default
    !>if the key is not contained in the map.
    function get_or_default(this, key, default) result(val)
        implicit none
        class(char_to_int32_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to mapping to the returned value
        integer(int32), intent(in) :: default
            !! the default value mapped to the specified key
        integer(int32) :: val
            !! the value to be mapped to the specified key

        val = this%get_value(key)
        if (val /= default) &
            val = default
    end function get_or_default

    !>Returns the number of key-value mappings the map contains
    !>and returns the maximum value of the 32-bit integer
    !>if the number is negative, assuming an overflow occurred.
    function size(this)
        implicit none
        class(char_to_int32_map_type), intent(in) :: this
            !! passed-object dummy argument
        integer(int32) :: size
            !! the number of key-value mappings in the map

        size = this%map%entries()

        ! if the number is negative,
        ! it is assumed that an overflow occurred.
        if (size < 0) then
            size = huge(size)
        end if
    end function size

    !>Returns `.true.` if no key-value mappings are contained
    !>in the map and returns `.false.` elsewhere.
    function is_empty(this)
        implicit none
        class(char_to_int32_map_type), intent(in) :: this
            !! passed-object dummy argument
        logical :: is_empty
            !! the status flag that the map is empty

        is_empty = (this%size() == 0)
    end function is_empty

    !>Returns `.true.` if the map contains the key
    !>and returns  `.false.` elsewhere.
    function contains_key(this, key)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        class(char_to_int32_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to test the its existence in the map
        logical :: contains_key
            !! the status flag that the map contains the key

        call this%map%key_test(to_hash_key(key), contains_key)
    end function contains_key

    !>Removes the key-value mapping of the specified key.
    subroutine remove_key(this, key, status)
        use :: stdlib_hashmap_wrappers, only:set
        use :: store_proc
        implicit none
        class(char_to_int32_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to be deleted from the map.
        integer(int32), intent(out), optional :: status
            !! the status of the operation

        logical :: existed

        call this%map%remove(to_hash_key(key), existed)
        if (existed) then
            call store(status, 0)
        else
            call store(status, 1)
        end if
    end subroutine remove_key

    !>Removes the key-value mapping of the specified key
    !>only if the key is mapped to the specific value.
    subroutine remove_if(this, key, value, status)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        class(char_to_int32_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to be deleted from the map
        integer(int32), intent(in) :: value
            !! the value to be mapped to the specified key
        integer(int32), intent(out), optional :: status
            !! the status of the operation

        integer(int32) :: mapped_value
        mapped_value = this%get(key)

        if (mapped_value == value) then
            call this%remove_key(key, status)
        end if
    end subroutine remove_if

    !>Initialize the instance of `char_to_int32_map_type`.
    subroutine initialize(this, hashmap, hasher, slots_bits, status)
        use :: maps_common_proc_initialize
        use :: maps_common_proc_factory
        implicit none
        class(char_to_int32_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in), optional :: hashmap
            !! the name of the hashmap type
        character(*), intent(in), optional :: hasher
            !! the name of the hash function
        integer(int32), intent(in), optional :: slots_bits
            !! the number of bits initially used to map to the slots
        integer(int32), intent(out), optional :: status
            !! the status of the operation

        allocate (this%map, source=hashmap_factory(hashmap))
        call initialize_map(this%map, hasher, slots_bits, status)
    end subroutine initialize

    !>Finalizes the instance of `char_to_int32_map_type`.
    subroutine finalize(this)
        implicit none
        class(char_to_int32_map_type), intent(inout) :: this
            !! passed-object dummy argument

        deallocate (this%map)
    end subroutine finalize
end module map_char_int32
