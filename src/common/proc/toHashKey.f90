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
    !>Returns an instance of `key_type` constructed from the `key`.
    function to_hash_key_char(key) result(hash_key)
        implicit none
        character(*), intent(in) :: key
            !! a key in `character(*)`
        type(key_type) :: hash_key
            !! an instance of `key_type` constructed from the `value`

        call set(hash_key, key)
    end function to_hash_key_char

    !>Returns an instance of `key_type` constructed from the `key`.
    function to_hash_key_int8(key) result(hash_key)
        implicit none
        integer(int8), intent(in) :: key(:)
            !! a key in an array of 1-byte integers
        type(key_type) :: hash_key
            !! an instance of `key_type` constructed from the `value`

        call set(hash_key, key)
    end function to_hash_key_int8
end module maps_common_proc_toHashKey
