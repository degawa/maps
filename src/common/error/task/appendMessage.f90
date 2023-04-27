module maps_common_error_task_appendMessage
    use, intrinsic :: iso_fortran_env
    use :: errstat
    implicit none
    private
    public :: append_message_task

    !>An user-defined type to define a task
    !>for appending message to the error message
    type, public, extends(task_type) :: append_message_task_type
        character(:), allocatable :: msg
            !! appending message
    contains
        procedure, public, pass :: execute
        !*appends the message
    end type append_message_task_type

contains
    !>Appends the message to the error message contained in err.
    subroutine execute(this, err, stat, msg)
        implicit none
        class(append_message_task_type), intent(in) :: this
            !! passed-object dummy argument
        type(error_stat_type), intent(inout), optional :: err
            !! the error status
        integer(int32), intent(in), optional :: stat
            !! the error code
        character(*), intent(inout), optional :: msg
            !! the error message

        if (present(err)) then
            call err%append_message(this%msg)
        end if

        return
        if (present(stat)) continue
        if (present(msg)) continue
    end subroutine execute

    !>Returns a `append_message_task_type` instance.
    function append_message_task(msg) result(new_task)
        implicit none
        character(*), intent(in) :: msg
            !! appending message

        type(append_message_task_type) :: new_task
            !! an instance

        new_task%msg = msg
    end function append_message_task
end module maps_common_error_task_appendMessage
