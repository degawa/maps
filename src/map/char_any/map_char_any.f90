module map_char_any
    use, intrinsic :: iso_fortran_env
    use :: stdlib_hashmaps, only:hashmap_type, chaining_hashmap_type
    use :: stdlib_hashmap_wrappers, only:key_type, other_type
    use :: maps_common_proc_toHashKey
    use :: maps_common_proc_toHashValue
    use :: maps_common_proc_get
    use :: maps_common_proc_append
    use :: errstat
    use :: maps_common_error_repository
    implicit none
    private

    !>User-defined type for mapping keys of `character(*)`
    !>to values of the intrinsic types.
    !>
    !>Dupulicated keys are not allowed.
    type, public :: char_to_any_map_type
        class(hashmap_type), allocatable, private :: map
            !! hashmap
    contains
        procedure, public, pass :: put_int8
        !* maps a 1-byte integer value to a key.
        procedure, public, pass :: put_int16
        !* maps a 2-byte integer value to a key.
        procedure, public, pass :: put_int32
        !* maps a 4-byte integer value to a key.
        procedure, public, pass :: put_int64
        !* maps an 8-byte integer value to a key.
        procedure, public, pass :: put_real32
        !* maps a 4-byte floating point number value to a key.
        procedure, public, pass :: put_real64
        !* maps an 8-byte floating point number value to a key.
        procedure, public, pass :: put_real128
        !* maps a 16-byte floating point number value to a key.
        procedure, public, pass :: put_cmplx32
        !* maps a 4-byte complex number value to a key.
        procedure, public, pass :: put_cmplx64
        !* maps an 8-byte complex number value to a key.
        procedure, public, pass :: put_cmplx128
        !* maps a 16-byte complex number value to a key.
        procedure, public, pass :: put_logical
        !* maps a logical value to a key.
        procedure, public, pass :: put_char
        !* maps a character(*) value to a key.
        generic :: &
            put => &
            put_int8, &
            put_int16, &
            put_int32, &
            put_int64, &
            put_real32, &
            put_real64, &
            put_real128, &
            put_cmplx32, &
            put_cmplx64, &
            put_cmplx128, &
            put_logical, &
            put_char

        procedure, public, pass :: get_value
        !* returns the value mapped to a key.
        generic :: get => get_value

        procedure, public, pass :: size
        !* returns the number of key-value mappings.
        procedure, public, pass :: is_empty
        !* returns true if the map is empty.

        procedure, public, pass :: contains_key
        !* returns true if the map contains the key.
        generic :: contains => contains_key

        procedure, public, pass :: remove_key
        !* removes the key-value mapping.
        procedure, public, pass :: remove_if_int8
        !* removes the key-value mapping
        !  only if the key is mapped to the specific value.
        procedure, public, pass :: remove_if_int16
        !* removes the key-value mapping
        !  only if the key is mapped to the specific value.
        procedure, public, pass :: remove_if_int32
        !* removes the key-value mapping
        !  only if the key is mapped to the specific value.
        procedure, public, pass :: remove_if_int64
        !* removes the key-value mapping
        !  only if the key is mapped to the specific value.
        procedure, public, pass :: remove_if_real32
        !* removes the key-value mapping
        !  only if the key is mapped to the specific value.
        procedure, public, pass :: remove_if_real64
        !* removes the key-value mapping
        !  only if the key is mapped to the specific value.
        procedure, public, pass :: remove_if_real128
        !* removes the key-value mapping
        !  only if the key is mapped to the specific value.
        procedure, public, pass :: remove_if_cmplx32
        !* removes the key-value mapping
        !  only if the key is mapped to the specific value.
        procedure, public, pass :: remove_if_cmplx64
        !* removes the key-value mapping
        !  only if the key is mapped to the specific value.
        procedure, public, pass :: remove_if_cmplx128
        !* removes the key-value mapping
        !  only if the key is mapped to the specific value.
        procedure, public, pass :: remove_if_logical
        !* removes the key-value mapping
        !  only if the key is mapped to the specific value.
        procedure, public, pass :: remove_if_char
        !* removes the key-value mapping
        !  only if the key is mapped to the specific value.
        generic :: &
            remove_if => &
            remove_if_int8, &
            remove_if_int16, &
            remove_if_int32, &
            remove_if_int64, &
            remove_if_real32, &
            remove_if_real64, &
            remove_if_real128, &
            remove_if_cmplx32, &
            remove_if_cmplx64, &
            remove_if_cmplx128, &
            remove_if_logical, &
            remove_if_char

        procedure, public, pass :: initialize
        !* initialize the instance of `char_to_any_map_type`.
        procedure, public, pass :: finalize
        !* finalize the instance of `char_to_any_map_type`.
    end type char_to_any_map_type

contains
    !>Maps the specified value to the specified key in the map.
    subroutine put_int8(this, key, value, status)
        implicit none
        class(char_to_any_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to mapping to the specified value
        integer(int8), intent(in) :: value
            !! the value to be mapped to the specified key
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        call append(this%map, key, value, status)
        if (error_occurred(status)) return

        call set_success(status)
    end subroutine put_int8

    !>Maps the specified value to the specified key in the map.
    subroutine put_int16(this, key, value, status)
        implicit none
        class(char_to_any_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to mapping to the specified value
        integer(int16), intent(in) :: value
            !! the value to be mapped to the specified key
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        call append(this%map, key, value, status)
        if (error_occurred(status)) return

        call set_success(status)
    end subroutine put_int16

    !>Maps the specified value to the specified key in the map.
    subroutine put_int32(this, key, value, status)
        implicit none
        class(char_to_any_map_type), intent(inout) :: this
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
    end subroutine put_int32

    !>Maps the specified value to the specified key in the map.
    subroutine put_int64(this, key, value, status)
        implicit none
        class(char_to_any_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to mapping to the specified value
        integer(int64), intent(in) :: value
            !! the value to be mapped to the specified key
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        call append(this%map, key, value, status)
        if (error_occurred(status)) return

        call set_success(status)
    end subroutine put_int64

    !>Maps the specified value to the specified key in the map.
    subroutine put_real32(this, key, value, status)
        implicit none
        class(char_to_any_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to mapping to the specified value
        real(real32), intent(in) :: value
            !! the value to be mapped to the specified key
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        call append(this%map, key, value, status)
        if (error_occurred(status)) return

        call set_success(status)
    end subroutine put_real32

    !>Maps the specified value to the specified key in the map.
    subroutine put_real64(this, key, value, status)
        implicit none
        class(char_to_any_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to mapping to the specified value
        real(real64), intent(in) :: value
            !! the value to be mapped to the specified key
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        call append(this%map, key, value, status)
        if (error_occurred(status)) return

        call set_success(status)
    end subroutine put_real64

    !>Maps the specified value to the specified key in the map.
    subroutine put_real128(this, key, value, status)
        implicit none
        class(char_to_any_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to mapping to the specified value
        real(real128), intent(in) :: value
            !! the value to be mapped to the specified key
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        call append(this%map, key, value, status)
        if (error_occurred(status)) return

        call set_success(status)
    end subroutine put_real128

    !>Maps the specified value to the specified key in the map.
    subroutine put_cmplx32(this, key, value, status)
        implicit none
        class(char_to_any_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to mapping to the specified value
        complex(real32), intent(in) :: value
            !! the value to be mapped to the specified key
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        call append(this%map, key, value, status)
        if (error_occurred(status)) return

        call set_success(status)
    end subroutine put_cmplx32

    !>Maps the specified value to the specified key in the map.
    subroutine put_cmplx64(this, key, value, status)
        implicit none
        class(char_to_any_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to mapping to the specified value
        complex(real64), intent(in) :: value
            !! the value to be mapped to the specified key
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        call append(this%map, key, value, status)
        if (error_occurred(status)) return

        call set_success(status)
    end subroutine put_cmplx64

    !>Maps the specified value to the specified key in the map.
    subroutine put_cmplx128(this, key, value, status)
        implicit none
        class(char_to_any_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to mapping to the specified value
        complex(real128), intent(in) :: value
            !! the value to be mapped to the specified key
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        call append(this%map, key, value, status)
        if (error_occurred(status)) return

        call set_success(status)
    end subroutine put_cmplx128

    !>Maps the specified value to the specified key in the map.
    subroutine put_logical(this, key, value, status)
        implicit none
        class(char_to_any_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to mapping to the specified value
        logical, intent(in) :: value
            !! the value to be mapped to the specified key
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        call append(this%map, key, value, status)
        if (error_occurred(status)) return

        call set_success(status)
    end subroutine put_logical

    !>Maps the specified value to the specified key in the map.
    subroutine put_char(this, key, value, status)
        implicit none
        class(char_to_any_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to mapping to the specified value
        character(*), intent(in) :: value
            !! the value to be mapped to the specified key
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        call append(this%map, key, value, status)
        if (error_occurred(status)) return

        call set_success(status)
    end subroutine put_char

    !>Returns the value to which the specified key is mapped.
    !>The value is undefined if the type of `value` in the map
    !>is not the intrinsic types.
    function get_value(this, key, status) result(val)
        use :: maps_common_type_mappable
        implicit none
        class(char_to_any_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to mapping to the returned value
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation
        type(mappable_type) :: val
            !! the value to be mapped to the specified key

        class(*), allocatable :: other
        call get_other(this%map, key, other, status)
        if (error_occurred(status)) return

        select type (other)
        type is (integer(int8))
            val = other
        type is (integer(int16))
            val = other
        type is (integer(int32))
            val = other
        type is (integer(int64))
            val = other
        type is (real(real32))
            val = other
        type is (real(real64))
            val = other
        type is (real(real128))
            val = other
        type is (complex(real32))
            val = other
        type is (complex(real64))
            val = other
        type is (complex(real128))
            val = other
        type is (logical)
            val = other
        type is (character(*))
            val = other
        class default
            call catch_error(err%value_type_error, err, status)
            return
        end select

        call set_success(status)
    end function get_value

    !>Returns the number of key-value mappings the map contains
    !>and returns the maximum value of the 32-bit integer
    !>if the number is negative, assuming an overflow occurred.
    function size(this, status)
        implicit none
        class(char_to_any_map_type), intent(in) :: this
            !! passed-object dummy argument
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation
        integer(int32) :: size
            !! the number of key-value mappings in the map

        size = this%map%entries()

        ! if the number is negative,
        ! it is assumed that an overflow occurred.
        if (size < 0) then
            size = huge(size)
            call catch_error(success_status_code, err%get(err%warn_overflow_occured), status)
            return
        end if
        call set_success(status)
    end function size

    !>Returns `.true.` if no key-value mappings are contained
    !>in the map and returns `.false.` elsewhere.
    function is_empty(this, status)
        implicit none
        class(char_to_any_map_type), intent(in) :: this
            !! passed-object dummy argument
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation
        logical :: is_empty
            !! the status flag that the map is empty

        is_empty = (this%size() == 0)
        call set_success(status)
    end function is_empty

    !>Returns `.true.` if the map contains the key
    !>and returns  `.false.` elsewhere.
    function contains_key(this, key, status)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        class(char_to_any_map_type), intent(inout) :: this
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
        class(char_to_any_map_type), intent(inout) :: this
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
    subroutine remove_if_int8(this, key, value, status)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        class(char_to_any_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to be deleted from the map
        integer(int8), intent(in) :: value
            !! the value to be mapped to the specified key
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        integer(int8) :: mapped_value
        mapped_value = this%get(key, status)
        if (error_occurred(status)) return

        if (mapped_value == value) then
            call this%remove_key(key, status)
            if (error_occurred(status)) return
        end if

        call set_success(status)
    end subroutine remove_if_int8

    !>Removes the key-value mapping of the specified key
    !>only if the key is mapped to the specific value.
    subroutine remove_if_int16(this, key, value, status)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        class(char_to_any_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to be deleted from the map
        integer(int16), intent(in) :: value
            !! the value to be mapped to the specified key
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        integer(int16) :: mapped_value
        mapped_value = this%get(key, status)
        if (error_occurred(status)) return

        if (mapped_value == value) then
            call this%remove_key(key, status)
            if (error_occurred(status)) return
        end if

        call set_success(status)
    end subroutine remove_if_int16

    !>Removes the key-value mapping of the specified key
    !>only if the key is mapped to the specific value.
    subroutine remove_if_int32(this, key, value, status)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        class(char_to_any_map_type), intent(inout) :: this
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
    end subroutine remove_if_int32

    !>Removes the key-value mapping of the specified key
    !>only if the key is mapped to the specific value.
    subroutine remove_if_int64(this, key, value, status)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        class(char_to_any_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to be deleted from the map
        integer(int64), intent(in) :: value
            !! the value to be mapped to the specified key
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        integer(int64) :: mapped_value
        mapped_value = this%get(key, status)
        if (error_occurred(status)) return

        if (mapped_value == value) then
            call this%remove_key(key, status)
            if (error_occurred(status)) return
        end if

        call set_success(status)
    end subroutine remove_if_int64

    !>Removes the key-value mapping of the specified key
    !>only if the key is mapped to the specific value.
    subroutine remove_if_real32(this, key, value, status)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        class(char_to_any_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to be deleted from the map
        real(real32), intent(in) :: value
            !! the value to be mapped to the specified key
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        real(real32) :: mapped_value
        mapped_value = this%get(key, status)
        if (error_occurred(status)) return

        if (abs(mapped_value - value) <= epsilon(value)) then
            call this%remove_key(key, status)
            if (error_occurred(status)) return
        end if

        call set_success(status)
    end subroutine remove_if_real32

    !>Removes the key-value mapping of the specified key
    !>only if the key is mapped to the specific value.
    subroutine remove_if_real64(this, key, value, status)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        class(char_to_any_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to be deleted from the map
        real(real64), intent(in) :: value
            !! the value to be mapped to the specified key
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        real(real64) :: mapped_value
        mapped_value = this%get(key, status)
        if (error_occurred(status)) return

        if (abs(mapped_value - value) <= epsilon(value)) then
            call this%remove_key(key, status)
            if (error_occurred(status)) return
        end if

        call set_success(status)
    end subroutine remove_if_real64

    !>Removes the key-value mapping of the specified key
    !>only if the key is mapped to the specific value.
    subroutine remove_if_real128(this, key, value, status)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        class(char_to_any_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to be deleted from the map
        real(real128), intent(in) :: value
            !! the value to be mapped to the specified key
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        real(real128) :: mapped_value
        mapped_value = this%get(key, status)
        if (error_occurred(status)) return

        if (abs(mapped_value - value) <= epsilon(value)) then
            call this%remove_key(key, status)
            if (error_occurred(status)) return
        end if

        call set_success(status)
    end subroutine remove_if_real128

    !>Removes the key-value mapping of the specified key
    !>only if the key is mapped to the specific value.
    subroutine remove_if_cmplx32(this, key, value, status)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        class(char_to_any_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to be deleted from the map
        complex(real32), intent(in) :: value
            !! the value to be mapped to the specified key
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        complex(real32) :: mapped_value
        mapped_value = this%get(key, status)
        if (error_occurred(status)) return

        if (abs(mapped_value - value) <= epsilon(real(0, kind=real32))) then
            call this%remove_key(key, status)
            if (error_occurred(status)) return
        end if

        call set_success(status)
    end subroutine remove_if_cmplx32

    !>Removes the key-value mapping of the specified key
    !>only if the key is mapped to the specific value.
    subroutine remove_if_cmplx64(this, key, value, status)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        class(char_to_any_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to be deleted from the map
        complex(real64), intent(in) :: value
            !! the value to be mapped to the specified key
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        complex(real64) :: mapped_value
        mapped_value = this%get(key, status)
        if (error_occurred(status)) return

        if (abs(mapped_value - value) <= epsilon(real(0, kind=real64))) then
            call this%remove_key(key, status)
            if (error_occurred(status)) return
        end if

        call set_success(status)
    end subroutine remove_if_cmplx64

    !>Removes the key-value mapping of the specified key
    !>only if the key is mapped to the specific value.
    subroutine remove_if_cmplx128(this, key, value, status)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        class(char_to_any_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to be deleted from the map
        complex(real128), intent(in) :: value
            !! the value to be mapped to the specified key
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        complex(real128) :: mapped_value
        mapped_value = this%get(key, status)
        if (error_occurred(status)) return

        if (abs(mapped_value - value) <= epsilon(real(0, kind=real128))) then
            call this%remove_key(key, status)
            if (error_occurred(status)) return
        end if

        call set_success(status)
    end subroutine remove_if_cmplx128

    !>Removes the key-value mapping of the specified key
    !>only if the key is mapped to the specific value.
    subroutine remove_if_logical(this, key, value, status)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        class(char_to_any_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to be deleted from the map
        logical, intent(in) :: value
            !! the value to be mapped to the specified key
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        logical :: mapped_value
        mapped_value = this%get(key, status)
        if (error_occurred(status)) return

        if (mapped_value .eqv. value) then
            call this%remove_key(key, status)
            if (error_occurred(status)) return
        end if

        call set_success(status)
    end subroutine remove_if_logical

    !>Removes the key-value mapping of the specified key
    !>only if the key is mapped to the specific value.
    subroutine remove_if_char(this, key, value, status)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        class(char_to_any_map_type), intent(inout) :: this
            !! passed-object dummy argument
        character(*), intent(in) :: key
            !! the key to be deleted from the map
        character(*), intent(in) :: value
            !! the value to be mapped to the specified key
        type(error_stat_type), intent(out), optional :: status
            !! the status of the operation

        character(:), allocatable :: mapped_value
        mapped_value = this%get(key, status)
        if (error_occurred(status)) return

        if (all([len(mapped_value) == len(value), &
                 mapped_value == value])) then
            call this%remove_key(key, status)
            if (error_occurred(status)) return
        end if

        call set_success(status)
    end subroutine remove_if_char

    !>Initialize the instance of `char_to_int32_map_type`.
    subroutine initialize(this, collision_resolver, hasher, slots_bits, status)
        use :: maps_common_proc_initialize
        use :: maps_common_proc_factory
        use :: maps_common_type_collisionResolver
        use :: maps_common_type_hashFunction
        use :: maps_common_error_task_appendMessage
        implicit none
        class(char_to_any_map_type), intent(inout) :: this
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

    !>Finalizes the instance of `char_to_any_map_type`.
    subroutine finalize(this, status)
        use :: maps_common_error_task_appendMessage
        implicit none
        class(char_to_any_map_type), intent(inout) :: this
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
    end subroutine finalize
end module map_char_any
