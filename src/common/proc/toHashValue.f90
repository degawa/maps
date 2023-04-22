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
    function to_hash_value_int8(value) result(hash_value)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        integer(int8), intent(in) :: value
        type(other_type) :: hash_value

        call set(hash_value, value)
    end function to_hash_value_int8

    function to_hash_value_int16(value) result(hash_value)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        integer(int16), intent(in) :: value
        type(other_type) :: hash_value

        call set(hash_value, value)
    end function to_hash_value_int16

    function to_hash_value_int32(value) result(hash_value)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        integer(int32), intent(in) :: value
        type(other_type) :: hash_value

        call set(hash_value, value)
    end function to_hash_value_int32

    function to_hash_value_int64(value) result(hash_value)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        integer(int64), intent(in) :: value
        type(other_type) :: hash_value

        call set(hash_value, value)
    end function to_hash_value_int64

    function to_hash_value_real32(value) result(hash_value)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        real(real32), intent(in) :: value
        type(other_type) :: hash_value

        call set(hash_value, value)
    end function to_hash_value_real32

    function to_hash_value_real64(value) result(hash_value)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        real(real64), intent(in) :: value
        type(other_type) :: hash_value

        call set(hash_value, value)
    end function to_hash_value_real64

    function to_hash_value_cmplx32(value) result(hash_value)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        complex(real32), intent(in) :: value
        type(other_type) :: hash_value

        call set(hash_value, value)
    end function to_hash_value_cmplx32

    function to_hash_value_cmplx64(value) result(hash_value)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        complex(real64), intent(in) :: value
        type(other_type) :: hash_value

        call set(hash_value, value)
    end function to_hash_value_cmplx64

    function to_hash_value_logical(value) result(hash_value)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        logical, intent(in) :: value
        type(other_type) :: hash_value

        call set(hash_value, value)
    end function to_hash_value_logical

    function to_hash_value_char(value) result(hash_value)
        use :: stdlib_hashmap_wrappers, only:set
        implicit none
        character(*), intent(in) :: value
        type(other_type) :: hash_value

        call set(hash_value, value)
    end function to_hash_value_char
end module maps_common_proc_toHashValue
