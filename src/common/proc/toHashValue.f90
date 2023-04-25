module maps_common_proc_toHashValue
    use, intrinsic :: iso_fortran_env
    use :: stdlib_hashmap_wrappers, only:other_type, set
    implicit none
    private
    public :: to_hash_value

    interface to_hash_value
        procedure :: to_hash_value_int8
        procedure :: to_hash_value_int16
        procedure :: to_hash_value_int32
        procedure :: to_hash_value_int64
        procedure :: to_hash_value_real32
        procedure :: to_hash_value_real64
        procedure :: to_hash_value_cmplx32
        procedure :: to_hash_value_cmplx64
        procedure :: to_hash_value_logical
        procedure :: to_hash_value_char
    end interface
contains
    !>Returns the value set to `other_type`.
    function to_hash_value_int8(value) result(hash_value)
        implicit none
        integer(int8), intent(in) :: value
            !! a value to be mapped in a map
        type(other_type) :: hash_value
            !! the value set to `other_type`

        call set(hash_value, value)
    end function to_hash_value_int8

    !>Returns the value set to `other_type`.
    function to_hash_value_int16(value) result(hash_value)
        implicit none
        integer(int16), intent(in) :: value
            !! a value to be mapped in a map
        type(other_type) :: hash_value
            !! the value set to `other_type`

        call set(hash_value, value)
    end function to_hash_value_int16

    !>Returns the value set to `other_type`.
    function to_hash_value_int32(value) result(hash_value)
        implicit none
        integer(int32), intent(in) :: value
            !! a value to be mapped in a map
        type(other_type) :: hash_value
            !! the value set to `other_type`

        call set(hash_value, value)
    end function to_hash_value_int32

    !>Returns the value set to `other_type`.
    function to_hash_value_int64(value) result(hash_value)
        implicit none
        integer(int64), intent(in) :: value
            !! a value to be mapped in a map
        type(other_type) :: hash_value

        call set(hash_value, value)
    end function to_hash_value_int64
            !! the value set to `other_type`

    !>Returns the value set to `other_type`.
    function to_hash_value_real32(value) result(hash_value)
        implicit none
        real(real32), intent(in) :: value
            !! a value to be mapped in a map
        type(other_type) :: hash_value
            !! the value set to `other_type`

        call set(hash_value, value)
    end function to_hash_value_real32

    !>Returns the value set to `other_type`.
    function to_hash_value_real64(value) result(hash_value)
        implicit none
        real(real64), intent(in) :: value
            !! a value to be mapped in a map
        type(other_type) :: hash_value
            !! the value set to `other_type`

        call set(hash_value, value)
    end function to_hash_value_real64

    !>Returns the value set to `other_type`.
    function to_hash_value_cmplx32(value) result(hash_value)
        implicit none
        complex(real32), intent(in) :: value
            !! a value to be mapped in a map
        type(other_type) :: hash_value
            !! the value set to `other_type`

        call set(hash_value, value)
    end function to_hash_value_cmplx32

    !>Returns the value set to `other_type`.
    function to_hash_value_cmplx64(value) result(hash_value)
        implicit none
        complex(real64), intent(in) :: value
            !! a value to be mapped in a map
        type(other_type) :: hash_value
            !! the value set to `other_type`

        call set(hash_value, value)
    end function to_hash_value_cmplx64

    !>Returns the value set to `other_type`.
    function to_hash_value_logical(value) result(hash_value)
        implicit none
        logical, intent(in) :: value
            !! a value to be mapped in a map
        type(other_type) :: hash_value
            !! the value set to `other_type`

        call set(hash_value, value)
    end function to_hash_value_logical

    !>Returns the value set to `other_type`.
    function to_hash_value_char(value) result(hash_value)
        implicit none
        character(*), intent(in) :: value
            !! a value to be mapped in a map
        type(other_type) :: hash_value
            !! the value set to `other_type`

        call set(hash_value, value)
    end function to_hash_value_char
end module maps_common_proc_toHashValue
