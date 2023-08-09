program bitset_int32
    use, intrinsic :: iso_fortran_env
    use :: stdlib_bitsets
    use :: maps
    implicit none

    type(bitset_to_int32_map_type) :: map
    type(bitset_large), allocatable :: key(:)
    integer(int32) :: id

    call map%initialize()

    key = [bitset("00000010"), &
           bitset("01000011"), &
           bitset("01100100"), &
           bitset("01110100"), &
           bitset("10000100"), &
           bitset("10010100"), &
           bitset("10100011")]

    call map%put(key(1), 3)
    call map%put(key(2), 67)
    call map%put(key(3), 100)
    call map%put(key(4), 116)
    call map%put(key(5), 132)
    call map%put(key(6), 148)
    call map%put(key(7), 163)

    if (map%contains(key(7))) id = map%get(key(7))
    print *, id ! 163

    call map%remove_key(key(6))
    print *, map%contains(key(6)) ! F
    call map%put(bitset("11000011"), 195)
    call map%put(bitset("11100011"), 227)

    if (map%contains(bitset("11000011"))) id = map%get(bitset("11000011"))
    print *, id ! 195

    call map%finalize()

contains
    function bitset(bits_str) result(new_bitset)
        implicit none
        character(*), intent(in) :: bits_str
        type(bitset_large) :: new_bitset

        call new_bitset%from_string(bits_str)
    end function bitset
end program bitset_int32
