program hashmap
    use, intrinsic :: iso_fortran_env
    use stdlib_hashmaps, only: hashmap_type, open_hashmap_type, chaining_hashmap_type
    use stdlib_hashmap_wrappers, only: fnv_1_hasher, key_type, other_type, set, get
    implicit none

    class(hashmap_type), allocatable :: map

    allocate (chaining_hashmap_type :: map)
    call map%init(fnv_1_hasher)

    block
        type(key_type) :: key
        type(other_type) :: other
        logical :: conflict
        class(*), allocatable :: data

        call set(key, "apple")
        call set(other, 100)
        call map%map_entry(key, other, conflict)

        if (.not. conflict) then
            call set(key, "apple")
            call map%get_other_data(key, other)
            call get(other, data)

            select type (data); type is (integer(int32))
                print *, data, "is mapped to apple"
            end select
        end if
    end block
end program hashmap
