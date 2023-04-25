module maps_common_proc_append
    use, intrinsic :: iso_fortran_env
    use :: stdlib_hashmap_wrappers, only:key_type, other_type, set
    use :: stdlib_hashmaps, only:hashmap_type
    use :: store_proc
    implicit none
    private
    public :: append

    interface append
        procedure :: append_char_int8
        procedure :: append_char_int16
        procedure :: append_char_int32
        procedure :: append_char_int64
        procedure :: append_char_real32
        procedure :: append_char_real64
        procedure :: append_char_real128
        procedure :: append_char_cmplx32
        procedure :: append_char_cmplx64
        procedure :: append_char_cmplx128
        procedure :: append_char_logical
        procedure :: append_char_char
    end interface
contains
    !>Appends the key-value mapping to the specified map.
    subroutine append_char_int8(map, key, value, status)
        implicit none
        class(hashmap_type), intent(inout) :: map
            !! a map to which the specified key-value mapping is appended
        character(*), intent(in) :: key
            !! the key to mapping to the specified value
        integer(int8), intent(in) :: value
            !! the value to be mapped to the specified key
        integer(int32), intent(out), optional :: status
            !! the status of the operation

        type(key_type) :: hash_key
        type(other_type) :: hash_value
        logical :: conflict

        call store(status, 0)

        call set(hash_key, key)
        call set(hash_value, value)
        call map%map_entry(hash_key, hash_value, conflict)
        if (conflict) then
            call store(status, 2)
        end if
    end subroutine append_char_int8

    !>Appends the key-value mapping to the specified map.
    subroutine append_char_int16(map, key, value, status)
        implicit none
        class(hashmap_type), intent(inout) :: map
            !! a map to which the specified key-value mapping is appended
        character(*), intent(in) :: key
            !! the key to mapping to the specified value
        integer(int16), intent(in) :: value
            !! the value to be mapped to the specified key
        integer(int32), intent(out), optional :: status
            !! the status of the operation

        type(key_type) :: hash_key
        type(other_type) :: hash_value
        logical :: conflict

        call store(status, 0)

        call set(hash_key, key)
        call set(hash_value, value)
        call map%map_entry(hash_key, hash_value, conflict)
        if (conflict) then
            call store(status, 2)
        end if
    end subroutine append_char_int16

    !>Appends the key-value mapping to the specified map.
    subroutine append_char_int32(map, key, value, status)
        implicit none
        class(hashmap_type), intent(inout) :: map
            !! a map to which the specified key-value mapping is appended
        character(*), intent(in) :: key
            !! the key to mapping to the specified value
        integer(int32), intent(in) :: value
            !! the value to be mapped to the specified key
        integer(int32), intent(out), optional :: status
            !! the status of the operation

        type(key_type) :: hash_key
        type(other_type) :: hash_value
        logical :: conflict

        call store(status, 0)

        call set(hash_key, key)
        call set(hash_value, value)
        call map%map_entry(hash_key, hash_value, conflict)
        if (conflict) then
            call store(status, 2)
        end if
    end subroutine append_char_int32

    !>Appends the key-value mapping to the specified map.
    subroutine append_char_int64(map, key, value, status)
        implicit none
        class(hashmap_type), intent(inout) :: map
            !! a map to which the specified key-value mapping is appended
        character(*), intent(in) :: key
            !! the key to mapping to the specified value
        integer(int64), intent(in) :: value
            !! the value to be mapped to the specified key
        integer(int32), intent(out), optional :: status
            !! the status of the operation

        type(key_type) :: hash_key
        type(other_type) :: hash_value
        logical :: conflict

        call store(status, 0)

        call set(hash_key, key)
        call set(hash_value, value)
        call map%map_entry(hash_key, hash_value, conflict)
        if (conflict) then
            call store(status, 2)
        end if
    end subroutine append_char_int64

    !>Appends the key-value mapping to the specified map.
    subroutine append_char_real32(map, key, value, status)
        implicit none
        class(hashmap_type), intent(inout) :: map
            !! a map to which the specified key-value mapping is appended
        character(*), intent(in) :: key
            !! the key to mapping to the specified value
        real(real32), intent(in) :: value
            !! the value to be mapped to the specified key
        integer(int32), intent(out), optional :: status
            !! the status of the operation

        type(key_type) :: hash_key
        type(other_type) :: hash_value
        logical :: conflict

        call store(status, 0)

        call set(hash_key, key)
        call set(hash_value, value)
        call map%map_entry(hash_key, hash_value, conflict)
        if (conflict) then
            call store(status, 2)
        end if
    end subroutine append_char_real32

    !>Appends the key-value mapping to the specified map.
    subroutine append_char_real64(map, key, value, status)
        implicit none
        class(hashmap_type), intent(inout) :: map
            !! a map to which the specified key-value mapping is appended
        character(*), intent(in) :: key
            !! the key to mapping to the specified value
        real(real64), intent(in) :: value
            !! the value to be mapped to the specified key
        integer(int32), intent(out), optional :: status
            !! the status of the operation

        type(key_type) :: hash_key
        type(other_type) :: hash_value
        logical :: conflict

        call store(status, 0)

        call set(hash_key, key)
        call set(hash_value, value)
        call map%map_entry(hash_key, hash_value, conflict)
        if (conflict) then
            call store(status, 2)
        end if
    end subroutine append_char_real64

    !>Appends the key-value mapping to the specified map.
    subroutine append_char_real128(map, key, value, status)
        implicit none
        class(hashmap_type), intent(inout) :: map
            !! a map to which the specified key-value mapping is appended
        character(*), intent(in) :: key
            !! the key to mapping to the specified value
        real(real128), intent(in) :: value
            !! the value to be mapped to the specified key
        integer(int32), intent(out), optional :: status
            !! the status of the operation

        type(key_type) :: hash_key
        type(other_type) :: hash_value
        logical :: conflict

        call store(status, 0)

        call set(hash_key, key)
        call set(hash_value, value)
        call map%map_entry(hash_key, hash_value, conflict)
        if (conflict) then
            call store(status, 2)
        end if
    end subroutine append_char_real128

    !>Appends the key-value mapping to the specified map.
    subroutine append_char_cmplx32(map, key, value, status)
        implicit none
        class(hashmap_type), intent(inout) :: map
            !! a map to which the specified key-value mapping is appended
        character(*), intent(in) :: key
            !! the key to mapping to the specified value
        complex(real32), intent(in) :: value
            !! the value to be mapped to the specified key
        integer(int32), intent(out), optional :: status
            !! the status of the operation

        type(key_type) :: hash_key
        type(other_type) :: hash_value
        logical :: conflict

        call store(status, 0)

        call set(hash_key, key)
        call set(hash_value, value)
        call map%map_entry(hash_key, hash_value, conflict)
        if (conflict) then
            call store(status, 2)
        end if
    end subroutine append_char_cmplx32

    !>Appends the key-value mapping to the specified map.
    subroutine append_char_cmplx64(map, key, value, status)
        implicit none
        class(hashmap_type), intent(inout) :: map
            !! a map to which the specified key-value mapping is appended
        character(*), intent(in) :: key
            !! the key to mapping to the specified value
        complex(real64), intent(in) :: value
            !! the value to be mapped to the specified key
        integer(int32), intent(out), optional :: status
            !! the status of the operation

        type(key_type) :: hash_key
        type(other_type) :: hash_value
        logical :: conflict

        call store(status, 0)

        call set(hash_key, key)
        call set(hash_value, value)
        call map%map_entry(hash_key, hash_value, conflict)
        if (conflict) then
            call store(status, 2)
        end if
    end subroutine append_char_cmplx64

    !>Appends the key-value mapping to the specified map.
    subroutine append_char_cmplx128(map, key, value, status)
        implicit none
        class(hashmap_type), intent(inout) :: map
            !! a map to which the specified key-value mapping is appended
        character(*), intent(in) :: key
            !! the key to mapping to the specified value
        complex(real128), intent(in) :: value
            !! the value to be mapped to the specified key
        integer(int32), intent(out), optional :: status
            !! the status of the operation

        type(key_type) :: hash_key
        type(other_type) :: hash_value
        logical :: conflict

        call store(status, 0)

        call set(hash_key, key)
        call set(hash_value, value)
        call map%map_entry(hash_key, hash_value, conflict)
        if (conflict) then
            call store(status, 2)
        end if
    end subroutine append_char_cmplx128

    !>Appends the key-value mapping to the specified map.
    subroutine append_char_logical(map, key, value, status)
        implicit none
        class(hashmap_type), intent(inout) :: map
            !! a map to which the specified key-value mapping is appended
        character(*), intent(in) :: key
            !! the key to mapping to the specified value
        logical, intent(in) :: value
            !! the value to be mapped to the specified key
        integer(int32), intent(out), optional :: status
            !! the status of the operation

        type(key_type) :: hash_key
        type(other_type) :: hash_value
        logical :: conflict

        call store(status, 0)

        call set(hash_key, key)
        call set(hash_value, value)
        call map%map_entry(hash_key, hash_value, conflict)
        if (conflict) then
            call store(status, 2)
        end if
    end subroutine append_char_logical

    !>Appends the key-value mapping to the specified map.
    subroutine append_char_char(map, key, value, status)
        implicit none
        class(hashmap_type), intent(inout) :: map
            !! a map to which the specified key-value mapping is appended
        character(*), intent(in) :: key
            !! the key to mapping to the specified value
        character(*), intent(in) :: value
            !! the value to be mapped to the specified key
        integer(int32), intent(out), optional :: status
            !! the status of the operation

        type(key_type) :: hash_key
        type(other_type) :: hash_value
        logical :: conflict

        call store(status, 0)

        call set(hash_key, key)
        call set(hash_value, value)
        call map%map_entry(hash_key, hash_value, conflict)
        if (conflict) then
            call store(status, 2)
        end if
    end subroutine append_char_char
end module maps_common_proc_append
