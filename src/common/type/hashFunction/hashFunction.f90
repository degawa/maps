module maps_common_type_hashFunction
    use, intrinsic :: iso_c_binding
    use :: enumul
    implicit none
    private
    public :: optval

    enum, bind(c)
        enumerator :: FNV_1
        enumerator :: FNV_1A
        enumerator :: NMHASH32
        enumerator :: NMHASH32X
        enumerator :: WATER
    end enum

    !>Typed enumerator for hash function (hasher)
    type, public, extends(enum_atype) :: hash_function_type
    end type hash_function_type

    type(hash_function_type), public, parameter :: Hash_Function_FNV_1 = hash_function_type(FNV_1)
        !! the enumerator representing FNV-1 hash code
    type(hash_function_type), public, parameter :: Hash_Function_FNV_1A = hash_function_type(FNV_1A)
        !! the enumerator representing FNV-1A hash code
    type(hash_function_type), public, parameter :: Hash_Function_NMHASH32 = hash_function_type(NMHASH32)
        !! the enumerator representing NMHASH32 hash code
    type(hash_function_type), public, parameter :: Hash_Function_NMHASH32X = hash_function_type(NMHASH32X)
        !! the enumerator representing NMHASH32X hash code
    type(hash_function_type), public, parameter :: Hash_Function_WATER = hash_function_type(WATER)
        !! the enumerator representing WATERHASH hash code

    !>list of typed enumerator for hash function (hasher)
    type :: hash_function_list
        type(hash_function_type) :: FNV_1
            !! the enumerator representing FNV-1 hash code
        type(hash_function_type) :: FNV_1A
            !! the enumerator representing FNV-1A hash code
        type(hash_function_type) :: NMHASH32
            !! the enumerator representing NMHASH32 hash code
        type(hash_function_type) :: NMHASH32X
            !! the enumerator representing NMHASH32X hash code
        type(hash_function_type) :: WATER
            !! the enumerator representing WATERHASH hash code
    end type hash_function_list

    type(hash_function_list), public, parameter :: &
        hash_functions = hash_function_list(FNV_1    =Hash_Function_FNV_1, &
                                            FNV_1A   =Hash_Function_FNV_1A, &
                                            NMHASH32 =Hash_Function_NMHASH32, &
                                            NMHASH32X=Hash_Function_NMHASH32X, &
                                            WATER    =Hash_Function_WATER) !&
        !! typed enumerator list of the hash functions

    interface optval
        procedure :: optval_hash_function
    end interface
contains
    !>Returns `x` if it is present
    !>and returns `default` if it is not present.
    function optval_hash_function(x, default) result(y)
        implicit none
        type(hash_function_type), intent(in), optional :: x
            !! the result if `x` is present
        type(hash_function_type), intent(in) :: default
            !! the result if `x` is not present
        type(hash_function_type) :: y
            !! the result

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_hash_function
end module maps_common_type_hashFunction
