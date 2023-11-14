module maps_common_proc_key
    use, intrinsic :: iso_fortran_env
    use :: stdlib_hashmap_wrappers, only:key_type, get
    implicit none
    private
    public :: to_settable
    public :: get_byte_of_component
    public :: to_char

contains
    !>Returns the key converted to an array of 1-byte integers,
    !>acceptable to a hashmap.
    function to_settable(key) result(key_int8)
        implicit none
        class(*), intent(in) :: key
            !! a key in any intrinsic type represented as `class(*)`
        integer(int8), allocatable :: key_int8(:)
            !! the key converted to an array of 1-byte integers

        integer(int32) :: num_int8_elem
        num_int8_elem = storage_size(key)/storage_size(0_int8)

        key_int8 = transfer(key, mold=0_int8, size=num_int8_elem)
    end function to_settable

    !>Returns the byte of key's component `value`.
    pure elemental function get_byte_of_component(key) result(value_byte)
        implicit none
        type(key_type), intent(in) :: key
            !! a key
        integer(int32) :: value_byte
            !! byte of `key%value`

        value_byte = size(key%value)
    end function get_byte_of_component

    !>Returns character converted from a key.
    function to_char(key) result(key_as_char)
        implicit none
        type(key_type), intent(in) :: key
            !! a hash key
        character(:), allocatable :: key_as_char
            !! a key in `character(*)`

        call get(key, key_as_char)
        key_as_char = trim(key_as_char)
    end function to_char
end module maps_common_proc_key
