module maps_common_proc_key
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: to_settable

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

        key_int8 = transfer(key, mold=key_int8, size=num_int8_elem)
    end function to_settable
end module maps_common_proc_key
