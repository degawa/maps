module map_char_int32
    use, intrinsic :: iso_fortran_env
    use :: stdlib_hashmaps, only:hashmap_type, chaining_hashmap_type
    use :: stdlib_hashmap_wrappers, only:key_type, other_type
    use :: maps_common_proc_toHashKey
    use :: maps_common_proc_toHashValue
    use :: errstat
    use :: maps_common_error_repository
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

        procedure, public, pass :: entries
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

        procedure, public, pass :: replace
        !* replaces the key-value mapping.
        procedure, public, pass :: replace_if
        !* replaces the key-value mapping
        !* only if the key is mapped to the specified value.

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
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        call append(this%map, key, value, status)
        if (error_occurred(status)) return

        call set_success(status)
    end subroutine put

    !>Returns the value to which the specified key is mapped.
    !>The value is undefined if the type of `value` in the map
    !>is not `integer(int32)`.
    function get_value(this, key, status) result(val)
        use :: maps_common_proc_get
        implicit none
        class(char_to_int32_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to mapping to the returned value
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation
        integer(int32) :: val
            !! the value to be mapped to the specified key

        class(*), allocatable :: other
        call get_other(this%map, key, other, status)
        if (error_occurred(status)) return

        select type (other)
        type is (integer(int32))
            val = other
        class default
            call catch_error(err%value_type_error, err, status)
            return
        end select

        call set_success(status)
    end function get_value

    !>Returns the value to which the specified key is mapped or
    !>returns the specified value as a default
    !>if the key is not contained in the map.
    function get_or_default(this, key, default, status) result(val)
        implicit none
        class(char_to_int32_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to mapping to the returned value
        integer(int32), intent(in) :: default
            !! the default value mapped to the specified key
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation
        integer(int32) :: val
            !! the value to be mapped to the specified key

        if (this%contains(key)) then
            val = this%get_value(key, status)
            if (error_occurred(status)) return
        else
            val = default
        end if

        call set_success(status)
    end function get_or_default

    !>Returns the number of key-value mappings the map contains
    !>and returns the maximum value of the 32-bit integer
    !>if the number is negative, assuming an overflow occurred.
    function entries(this, status)
        implicit none
        class(char_to_int32_map_type), intent(in) :: this
            !! passed-object dummy argument
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation
        integer(int32) :: entries
            !! the number of key-value mappings in the map

        character(:), allocatable :: msg
        msg = success_status_msg

        entries = this%map%entries()

        ! if the number is negative,
        ! it is assumed that an overflow occurred.
        if (entries < 0) then
            entries = huge(entries)
            msg = err%get(err%warn_overflow_occured)
        end if

        call set_success(status, msg)
    end function entries

    !>Returns `.true.` if no key-value mappings are contained
    !>in the map and returns `.false.` elsewhere.
    function is_empty(this, status)
        implicit none
        class(char_to_int32_map_type), intent(in) :: this
            !! passed-object dummy argument
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation
        logical :: is_empty
            !! the status flag that the map is empty

        is_empty = (this%entries() == 0)
        call set_success(status)
    end function is_empty

    !>Returns `.true.` if the map contains the key
    !>and returns  `.false.` elsewhere.
    function contains_key(this, key, status)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        class(char_to_int32_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to test the its existence in the map
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation
        logical :: contains_key
            !! the status flag that the map contains the key

        call this%map%key_test(to_hash_key(key), contains_key)
        call set_success(status)
    end function contains_key

    !>Removes the key-value mapping of the specified key.
    subroutine remove_key(this, key, status)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        class(char_to_int32_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to be deleted from the map.
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        logical :: existed

        call this%map%remove(to_hash_key(key), existed)
        if (.not. existed) then
            call catch_error(err%key_not_exist, err, status)
            return
        end if

        call set_success(status)
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
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        integer(int32) :: mapped_value
        mapped_value = this%get(key, status)
        if (error_occurred(status)) return

        if (mapped_value == value) then
            call this%remove_key(key, status)
            if (error_occurred(status)) return
        end if

        call set_success(status)
    end subroutine remove_if

    !>Replaces the key-value mapping
    !>if the specified key is contained in the map.
    subroutine replace(this, key, value, status)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        class(char_to_int32_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to be replaced.
        integer(int32), intent(in) :: value
            !! the value to be mapped to the specified key
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        if (this%contains(key)) then
            call this%remove_key(key, status)
            if (error_occurred(status)) return

            call this%put(key, value, status)
            if (error_occurred(status)) return
        end if

        call set_success(status)
    end subroutine replace

    !>Replaces the key-value mapping
    !>only if the key is mapped to the specific value.
    subroutine replace_if(this, key, old_value, new_value, status)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        class(char_to_int32_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to be replaced
        integer(int32), intent(in) :: old_value
            !! the value currently mapped to the specified key
        integer(int32), intent(in) :: new_value
            !! the value to be mapped to the specified key
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        if (this%contains(key)) then
            if (this%get(key) == old_value) then
                call this%replace(key, new_value, status)
                if (error_occurred(status)) return
            end if
        end if

        call set_success(status)
    end subroutine replace_if

    !>Initialize the instance of `char_to_int32_map_type`.
    subroutine initialize(this, collision_resolver, hasher, slots_bits, status)
        use :: maps_common_proc_initialize
        use :: maps_common_proc_factory
        use :: maps_common_type_collisionResolver
        use :: maps_common_type_hashFunction
        use :: maps_common_error_task_appendMessage
        implicit none
        class(char_to_int32_map_type), intent(inout) :: this
            !! passed-object dummy argument
        type(collision_resolver_type), intent(in), optional :: collision_resolver
            !! the collision resolution algorithm enumerator of the hashmap
        type(hash_function_type), intent(in), optional :: hasher
            !! the hash function enumerator
        integer(int32), intent(in), optional :: slots_bits
            !! the number of bits initially used to map to the slots
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        integer(int32) :: alloc_stat
        character(128) :: msg

        allocate (this%map, source=hashmap_factory(collision_resolver), stat=alloc_stat, errmsg=msg)
        if (alloc_stat /= 0) then
            call catch_error(err%allocation_failed, err, status, &
                             append_message_task(trim(msg)))
            return
        end if

        call initialize_map(this%map, hasher, slots_bits, status)
        if (error_occurred(status)) return

        call set_success(status)
    end subroutine initialize

    !>Finalizes the instance of `char_to_int32_map_type`.
    subroutine finalize(this, status)
        use :: maps_common_error_task_appendMessage
        implicit none
        class(char_to_int32_map_type), intent(inout) :: this
            !! passed-object dummy argument
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        integer(int32) :: alloc_stat
        character(128) :: msg

        deallocate (this%map, stat=alloc_stat, errmsg=msg)
        if (alloc_stat /= 0) then
            call catch_error(err%deallocation_failed, err, status, &
                             append_message_task(msg))
            return
        end if

        call set_success(status)
    end subroutine finalize
end module map_char_int32
