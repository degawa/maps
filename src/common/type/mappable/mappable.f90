module maps_common_type_mappable
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: as_int8
    public :: as_int16
    public :: as_int32
    public :: as_int64
    public :: as_real32
    public :: as_real64
    public :: as_real128
    public :: as_complex32
    public :: as_complex64
    public :: as_complex128
    public :: as_logical
    public :: as_char

    !>User-defined type for buffering a value of the intrinsic types.
    !>The type is introduced to simulate
    !>a function returning different types
    !>based on the left-hand-side variable.
    type, public :: mappable_type
        !&<
        integer(int8)   , private :: i8
            !! a buffer for a 1-byte integer
        integer(int16)  , private :: i16
            !! a buffer for a 2-byte integer
        integer(int32)  , private :: i32
            !! a buffer for a 4-byte integer
        integer(int64)  , private :: i64
            !! a buffer for an 8-byte integer
        real(real32)    , private :: r32
            !! a buffer for a 4-byte floating-point number value
        real(real64)    , private :: r64
            !! a buffer for an 8-byte floating-point number value
        real(real128)   , private :: r128
            !! a buffer for a 16-byte floating-point number value
        complex(real32) , private :: c32
            !! a buffer for a 4-byte complex number value
        complex(real64) , private :: c64
            !! a buffer for an 8-byte complex number value
        complex(real128), private :: c128
            !! a buffer for a 128-byte complex number value
        logical         , private :: l
            !! a buffer for a logical value
        character(:)    , private, allocatable :: char
            !! a buffer for a character(*)
        !&>
    contains
        procedure, public, pass(lhs) :: assign_int8
        !* assigns the right-hand-side value to a buffer
        procedure, public, pass(lhs) :: assign_int16
        !* assigns the right-hand-side value to a buffer
        procedure, public, pass(lhs) :: assign_int32
        !* assigns the right-hand-side value to a buffer
        procedure, public, pass(lhs) :: assign_int64
        !* assigns the right-hand-side value to a buffer
        procedure, public, pass(lhs) :: assign_real32
        !* assigns the right-hand-side value to a buffer
        procedure, public, pass(lhs) :: assign_real64
        !* assigns the right-hand-side value to a buffer
        procedure, public, pass(lhs) :: assign_real128
        !* assigns the right-hand-side value to a buffer
        procedure, public, pass(lhs) :: assign_cmplx32
        !* assigns the right-hand-side value to a buffer
        procedure, public, pass(lhs) :: assign_cmplx64
        !* assigns the right-hand-side value to a buffer
        procedure, public, pass(lhs) :: assign_cmplx128
        !* assigns the right-hand-side value to a buffer
        procedure, public, pass(lhs) :: assign_logical
        !* assigns the right-hand-side value to a buffer
        procedure, public, pass(lhs) :: assign_character
        !* assigns the right-hand-side value to a buffer
        procedure, public, pass(rhs) :: assign_to_int8
        !* Assigns a value in the buffer to the left-hand-side variable
        procedure, public, pass(rhs) :: assign_to_int16
        !* Assigns a value in the buffer to the left-hand-side variable
        procedure, public, pass(rhs) :: assign_to_int32
        !* Assigns a value in the buffer to the left-hand-side variable
        procedure, public, pass(rhs) :: assign_to_int64
        !* Assigns a value in the buffer to the left-hand-side variable
        procedure, public, pass(rhs) :: assign_to_real32
        !* Assigns a value in the buffer to the left-hand-side variable
        procedure, public, pass(rhs) :: assign_to_real64
        !* Assigns a value in the buffer to the left-hand-side variable
        procedure, public, pass(rhs) :: assign_to_real128
        !* Assigns a value in the buffer to the left-hand-side variable
        procedure, public, pass(rhs) :: assign_to_cmplx32
        !* Assigns a value in the buffer to the left-hand-side variable
        procedure, public, pass(rhs) :: assign_to_cmplx64
        !* Assigns a value in the buffer to the left-hand-side variable
        procedure, public, pass(rhs) :: assign_to_cmplx128
        !* Assigns a value in the buffer to the left-hand-side variable
        procedure, public, pass(rhs) :: assign_to_logical
        !* Assigns a value in the buffer to the left-hand-side variable
        procedure, public, pass(rhs) :: assign_to_character_alloc
        !* Assigns a value in the buffer to the left-hand-side variable
        generic :: &
            assignment(=) => &
            assign_int8, &
            assign_int16, &
            assign_int32, &
            assign_int64, &
            assign_real32, &
            assign_real64, &
            assign_real128, &
            assign_cmplx32, &
            assign_cmplx64, &
            assign_cmplx128, &
            assign_logical, &
            assign_character, &
            assign_to_int8, &
            assign_to_int16, &
            assign_to_int32, &
            assign_to_int64, &
            assign_to_real32, &
            assign_to_real64, &
            assign_to_real128, &
            assign_to_cmplx32, &
            assign_to_cmplx64, &
            assign_to_cmplx128, &
            assign_to_logical, &
            assign_to_character_alloc
    end type mappable_type
contains
    !>Assigns the right-hand-side value to a buffer.
    pure subroutine assign_int8(lhs, rhs)
        implicit none
        !&<
        class(mappable_type), intent(inout) :: lhs
            !! left-hand-side variable, passed-object dummy argument
        integer(int8)       , intent(in)    :: rhs
            !! right-hand-side value
        !&>
        lhs%i8 = rhs
    end subroutine assign_int8

    !>Assigns the right-hand-side value to a buffer.
    pure subroutine assign_int16(lhs, rhs)
        implicit none
        !&<
        class(mappable_type), intent(inout) :: lhs
            !! left-hand-side variable, passed-object dummy argument
        integer(int16)      , intent(in)    :: rhs
            !! right-hand-side value
        !&>
        lhs%i16 = rhs
    end subroutine assign_int16

    !>Assigns the right-hand-side value to a buffer.
    pure subroutine assign_int32(lhs, rhs)
        implicit none
        !&<
        class(mappable_type), intent(inout) :: lhs
            !! left-hand-side variable, passed-object dummy argument
        integer(int32)      , intent(in)    :: rhs
            !! right-hand-side value
        !&>
        lhs%i32 = rhs
    end subroutine assign_int32

    !>Assigns the right-hand-side value to a buffer.
    pure subroutine assign_int64(lhs, rhs)
        implicit none
        !&<
        class(mappable_type), intent(inout) :: lhs
            !! left-hand-side variable, passed-object dummy argument
        integer(int64)      , intent(in)    :: rhs
            !! right-hand-side value
        !&>
        lhs%i64 = rhs
    end subroutine assign_int64

    !>Assigns the right-hand-side value to a buffer.
    pure subroutine assign_real32(lhs, rhs)
        implicit none
        !&<
        class(mappable_type), intent(inout) :: lhs
            !! left-hand-side variable, passed-object dummy argument
        real(real32)        , intent(in)    :: rhs
            !! right-hand-side value
        !&>
        lhs%r32 = rhs
    end subroutine assign_real32

    !>Assigns the right-hand-side value to a buffer.
    pure subroutine assign_real64(lhs, rhs)
        implicit none
        !&<
        class(mappable_type), intent(inout) :: lhs
            !! left-hand-side variable, passed-object dummy argument
        real(real64)        , intent(in)    :: rhs
            !! right-hand-side value
        !&>
        lhs%r64 = rhs
    end subroutine assign_real64

    !>Assigns the right-hand-side value to a buffer.
    pure subroutine assign_real128(lhs, rhs)
        implicit none
        !&<
        class(mappable_type), intent(inout) :: lhs
            !! left-hand-side variable, passed-object dummy argument
        real(real128)       , intent(in)    :: rhs
            !! right-hand-side value
        !&>
        lhs%r128 = rhs
    end subroutine assign_real128

    !>Assigns the right-hand-side value to a buffer.
    pure subroutine assign_cmplx32(lhs, rhs)
        implicit none
        !&<
        class(mappable_type), intent(inout) :: lhs
            !! left-hand-side variable, passed-object dummy argument
        complex(real32)     , intent(in)    :: rhs
            !! right-hand-side value
        !&>
        lhs%c32 = rhs
    end subroutine assign_cmplx32

    !>Assigns the right-hand-side value to a buffer.
    pure subroutine assign_cmplx64(lhs, rhs)
        implicit none
        !&<
        class(mappable_type), intent(inout) :: lhs
            !! left-hand-side variable, passed-object dummy argument
        complex(real64)     , intent(in)    :: rhs
            !! right-hand-side value
        !&>
        lhs%c64 = rhs
    end subroutine assign_cmplx64

    !>Assigns the right-hand-side value to a buffer.
    pure subroutine assign_cmplx128(lhs, rhs)
        implicit none
        !&<
        class(mappable_type), intent(inout) :: lhs
            !! left-hand-side variable, passed-object dummy argument
        complex(real128)    , intent(in)    :: rhs
            !! right-hand-side value
        !&>
        lhs%c128 = rhs
    end subroutine assign_cmplx128

    !>Assigns the right-hand-side value to a buffer.
    pure subroutine assign_logical(lhs, rhs)
        implicit none
        !&<
        class(mappable_type), intent(inout) :: lhs
            !! left-hand-side variable, passed-object dummy argument
        logical             , intent(in)    :: rhs
            !! right-hand-side value
        !&>
        lhs%l = rhs
    end subroutine assign_logical

    !>Assigns the right-hand-side value to a buffer.
    pure subroutine assign_character(lhs, rhs)
        implicit none
        !&<
        class(mappable_type), intent(inout) :: lhs
            !! left-hand-side variable, passed-object dummy argument
        character(*)        , intent(in)    :: rhs
            !! right-hand-side value
        !&>
        lhs%char = rhs
    end subroutine assign_character

    !>Assigns a value in the buffer to the left-hand-side variable
    pure subroutine assign_to_int8(lhs, rhs)
        implicit none
        !&<
        integer(int8)       , intent(inout) :: lhs
            !! left-hand-side variable
        class(mappable_type), intent(in)    :: rhs
            !! right-hand-side value, passed-object dummy argument
        !&>
        lhs = rhs%i8
    end subroutine assign_to_int8

    !>Assigns a value in the buffer to the left-hand-side variable
    pure subroutine assign_to_int16(lhs, rhs)
        implicit none
        !&<
        integer(int16)      , intent(inout) :: lhs
            !! left-hand-side variable
        class(mappable_type), intent(in)    :: rhs
            !! right-hand-side value, passed-object dummy argument
        !&>
        lhs = rhs%i16
    end subroutine assign_to_int16

    !>Assigns a value in the buffer to the left-hand-side variable
    pure subroutine assign_to_int32(lhs, rhs)
        implicit none
        !&<
        integer(int32)      , intent(inout) :: lhs
            !! left-hand-side variable
        class(mappable_type), intent(in)    :: rhs
            !! right-hand-side value, passed-object dummy argument
        !&>
        lhs = rhs%i32
    end subroutine assign_to_int32

    !>Assigns a value in the buffer to the left-hand-side variable
    pure subroutine assign_to_int64(lhs, rhs)
        implicit none
        !&<
        integer(int64)      , intent(inout) :: lhs
            !! left-hand-side variable
        class(mappable_type), intent(in)    :: rhs
            !! right-hand-side value, passed-object dummy argument
        !&>
        lhs = rhs%i64
    end subroutine assign_to_int64

    !>Assigns a value in the buffer to the left-hand-side variable
    pure subroutine assign_to_real32(lhs, rhs)
        implicit none
        !&<
        real(real32)        , intent(inout) :: lhs
            !! left-hand-side variable
        class(mappable_type), intent(in)    :: rhs
            !! right-hand-side value, passed-object dummy argument
        !&>
        lhs = rhs%r32
    end subroutine assign_to_real32

    !>Assigns a value in the buffer to the left-hand-side variable
    pure subroutine assign_to_real64(lhs, rhs)
        implicit none
        !&<
        real(real64)        , intent(inout) :: lhs
            !! left-hand-side variable
        class(mappable_type), intent(in)    :: rhs
            !! right-hand-side value, passed-object dummy argument
        !&>
        lhs = rhs%r64
    end subroutine assign_to_real64

    !>Assigns a value in the buffer to the left-hand-side variable
    pure subroutine assign_to_real128(lhs, rhs)
        implicit none
        !&<
        real(real128)       , intent(inout) :: lhs
            !! left-hand-side variable
        class(mappable_type), intent(in)    :: rhs
            !! right-hand-side value, passed-object dummy argument
        !&>
        lhs = rhs%r128
    end subroutine assign_to_real128

    !>Assigns a value in the buffer to the left-hand-side variable
    pure subroutine assign_to_cmplx32(lhs, rhs)
        implicit none
        !&<
        complex(real32)     , intent(inout) :: lhs
            !! left-hand-side variable
        class(mappable_type), intent(in)    :: rhs
            !! right-hand-side value, passed-object dummy argument
        !&>
        lhs = rhs%c32
    end subroutine assign_to_cmplx32

    !>Assigns a value in the buffer to the left-hand-side variable
    pure subroutine assign_to_cmplx64(lhs, rhs)
        implicit none
        !&<
        complex(real64)     , intent(inout) :: lhs
            !! left-hand-side variable
        class(mappable_type), intent(in)    :: rhs
            !! right-hand-side value, passed-object dummy argument
        !&>
        lhs = rhs%c64
    end subroutine assign_to_cmplx64

    !>Assigns a value in the buffer to the left-hand-side variable
    pure subroutine assign_to_cmplx128(lhs, rhs)
        implicit none
        !&<
        complex(real128)    , intent(inout) :: lhs
            !! left-hand-side variable
        class(mappable_type), intent(in)    :: rhs
            !! right-hand-side value, passed-object dummy argument
        !&>
        lhs = rhs%c128
    end subroutine assign_to_cmplx128

    !>Assigns a value in the buffer to the left-hand-side variable
    pure subroutine assign_to_logical(lhs, rhs)
        implicit none
        !&<
        logical             , intent(inout) :: lhs
            !! left-hand-side variable
        class(mappable_type), intent(in)    :: rhs
            !! right-hand-side value, passed-object dummy argument
        !&>
        lhs = rhs%l
    end subroutine assign_to_logical

    !>Assigns a value in the buffer to the left-hand-side variable
    pure subroutine assign_to_character_alloc(lhs, rhs)
        implicit none
        !&<
        character(:)        , allocatable   , intent(inout) :: lhs
            !! left-hand-side variable
        class(mappable_type)                , intent(in)    :: rhs
            !! right-hand-side value, passed-object dummy argument
        !&>
        lhs = rhs%char
    end subroutine assign_to_character_alloc

    !>Returns a 1-byte integer in the buffer
    pure function as_int8(this) result(val)
        implicit none
        class(mappable_type), intent(in) :: this
            !! passed-object dummy argument
        integer(int8) :: val
            !! the 1-byte integer in the buffer

        val = this%i8
    end function as_int8

    !>Returns a 2-byte integer in the buffer
    pure function as_int16(this) result(val)
        implicit none
        class(mappable_type), intent(in) :: this
            !! passed-object dummy argument
        integer(int16) :: val
            !! the 2-byte integer in the buffer

        val = this%i16
    end function as_int16

    !>Returns a 4-byte integer in the buffer
    pure function as_int32(this) result(val)
        implicit none
        class(mappable_type), intent(in) :: this
            !! passed-object dummy argument
        integer(int32) :: val
            !! the 4-byte integer in the buffer

        val = this%i32
    end function as_int32

    !>Returns an 8-byte integer in the buffer
    pure function as_int64(this) result(val)
        implicit none
        class(mappable_type), intent(in) :: this
            !! passed-object dummy argument
        integer(int64) :: val
            !! the 8-byte integer in the buffer

        val = this%i64
    end function as_int64

    !>Returns a 4-byte floating-point number value in the buffer
    pure function as_real32(this) result(val)
        implicit none
        class(mappable_type), intent(in) :: this
            !! passed-object dummy argument
        real(real32) :: val
            !! the 4-byte floating-point number value in the buffer

        val = this%r32
    end function as_real32

    !>Returns an 8-byte floating-point number value in the buffer
    pure function as_real64(this) result(val)
        implicit none
        class(mappable_type), intent(in) :: this
            !! passed-object dummy argument
        real(real64) :: val
            !! the 8-byte floating-point number value in the buffer

        val = this%r64
    end function as_real64

    !>Returns a 16-byte floating-point number value in the buffer
    pure function as_real128(this) result(val)
        implicit none
        class(mappable_type), intent(in) :: this
            !! passed-object dummy argument
        real(real128) :: val
            !! the 16-byte floating-point number value in the buffer

        val = this%r128
    end function as_real128

    !>Returns a 4-byte complex number value in the buffer
    pure function as_complex32(this) result(val)
        implicit none
        class(mappable_type), intent(in) :: this
            !! passed-object dummy argument
        complex(real32) :: val
            !! the 4-byte complex number value in the buffer

        val = this%c32
    end function as_complex32

    !>Returns an 8-byte complex number value in the buffer
    pure function as_complex64(this) result(val)
        implicit none
        class(mappable_type), intent(in) :: this
            !! passed-object dummy argument
        complex(real64) :: val
            !! the 8-byte complex number value in the buffer

        val = this%c64
    end function as_complex64

    !>Returns a 16-byte complex number value in the buffer
    pure function as_complex128(this) result(val)
        implicit none
        class(mappable_type), intent(in) :: this
            !! passed-object dummy argument
        complex(real128) :: val
            !! the 16-byte complex number value in the buffer

        val = this%c128
    end function as_complex128

    !>Returns a logical value in the buffer
    pure function as_logical(this) result(val)
        implicit none
        class(mappable_type), intent(in) :: this
            !! passed-object dummy argument
        logical :: val
            !! the logical value in the buffer

        val = this%l
    end function as_logical

    !>Returns a character(*) value in the buffer
    pure function as_char(this) result(char)
        implicit none
        class(mappable_type), intent(in) :: this
            !! passed-object dummy argument
        character(:), allocatable :: char
            !! the character(*) value in the buffer

        char = this%char
    end function as_char
end module maps_common_type_mappable
