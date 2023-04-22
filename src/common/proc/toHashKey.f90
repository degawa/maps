module maps_common_proc_toHashKey
    use, intrinsic :: iso_fortran_env
    use :: stdlib_hashmap_wrappers, only:key_type, set
    implicit none
    private
    public :: to_hash_key

    interface to_hash_key
        procedure :: to_hash_key_char
        procedure :: to_hash_key_int8
    end interface
contains
    function to_hash_key_char(key) result(hash_key)
        implicit none
        character(*), intent(in) :: key
        type(key_type) :: hash_key

        call set(hash_key, key)
    end function to_hash_key_char

    function to_hash_key_int8(key) result(hash_key)
        implicit none
        integer(int8), intent(in) :: key(:)
        type(key_type) :: hash_key

        call set(hash_key, key)
    end function to_hash_key_int8
end module maps_common_proc_toHashKey
