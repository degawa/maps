module maps_common_error_repository
    use, intrinsic :: iso_fortran_env
    use :: errstat
    implicit none
    private

    enum, bind(c)
        enumerator :: success = 0
        enumerator :: Error_key_already_exist
        enumerator :: Error_key_not_exist
        enumerator :: Error_conflict
        enumerator :: Error_value_not_exist
        enumerator :: Error_unkwnon_collision_resolver
        enumerator :: Error_unkwnon_hash_function
        enumerator :: Error_initialization_failed
        enumerator :: Error_value_type_error
        enumerator :: Error_allocation_failed
        enumerator :: Error_deallocation_failed
        enumerator :: Error_error_in_hashmap_type

        enumerator :: Warning_not_exist
        enumerator :: Warning_value_not_equal
        enumerator :: Warning_overflow_occured
    end enum

    !>A type for pairing error code and message
    type, private :: means
        integer(int32) :: stat
            !! error code
        character(128) :: msg
            !! error message
    end type means

    !&<
    type(means), parameter :: &
        key_already_exist           = means(Error_key_already_exist, &
                                            "The key already exists in the map")
    type(means), parameter :: &
        key_not_exist               = means(Error_key_not_exist, &
                                            "The key does not exist in the map")
    type(means), parameter :: &
        conflict                    = means(Error_conflict, &
                                            "The key already exists and the entry was not entered into the map")
    type(means), parameter :: &
        value_not_exist             = means(Error_value_not_exist, &
                                            "The value mapped to the key does not exist in the map")
    type(means), parameter :: &
        unkwnon_collision_resolver  = means(Error_unkwnon_collision_resolver, &
                                            "Unkwnon collision resolution algorithm")
    type(means), parameter :: &
        unkwnon_hash_function       = means(Error_unkwnon_hash_function, &
                                            "Unkwnon hash function")
    type(means), parameter :: &
        initialization_failed       = means(Error_initialization_failed, &
                                            "Hashmap initialization failed")
    type(means), parameter :: &
        value_type_error            = means(Error_value_type_error, &
                                            "Value type is not of the upported type")
    type(means), parameter :: &
        allocation_failed           = means(Error_allocation_failed, &
                                            "Allocation failure")
    type(means), parameter :: &
        deallocation_failed         = means(Error_deallocation_failed, &
                                            "Deallocation failure")
    type(means), parameter :: &
        error_in_hashmap_type       = means(Error_error_in_hashmap_type, &
                                            "Error occurred in a procedure of hashmap_type")
    type(means), parameter :: &
        warn_not_exist              = means(Warning_not_exist, &
                                            "Key-value is not replaced because key does not exist")
    type(means), parameter :: &
        warn_value_not_equal        = means(Warning_value_not_equal, &
                                            "Key-value is not replaced because the value is not equal to old value")
    type(means), parameter :: &
        warn_overflow_occured       = means(Warning_overflow_occured, &
                                            "Overflow may have occurred")
    !&>

    !>A repository to contain error codes and messages of maps
    type, private, extends(message_repository_atype) :: maps_error_type
        !&<
        integer(int32) :: key_already_exist             = key_already_exist%stat
        integer(int32) :: key_not_exist                 = key_not_exist%stat
        integer(int32) :: conflict                      = conflict%stat
        integer(int32) :: value_not_exist               = value_not_exist%stat
        integer(int32) :: unkwnon_collision_resolver    = unkwnon_collision_resolver%stat
        integer(int32) :: unkwnon_hash_function         = unkwnon_hash_function%stat
        integer(int32) :: initialization_failed         = initialization_failed%stat
        integer(int32) :: value_type_error              = value_type_error%stat
        integer(int32) :: allocation_failed             = allocation_failed%stat
        integer(int32) :: deallocation_failed           = deallocation_failed%stat
        integer(int32) :: error_in_hashmap_type         = error_in_hashmap_type%stat
        integer(int32) :: warn_not_exist                = warn_not_exist%stat
        integer(int32) :: warn_value_not_equal          = warn_value_not_equal%stat
        integer(int32) :: warn_overflow_occured         = warn_overflow_occured%stat
        !&>
    contains
        procedure, public, pass :: get_message
        !* returns the error message from a code
    end type maps_error_type

    type(maps_error_type), public, parameter :: err = maps_error_type()
        !! the error codes and messages of maps

contains
    !>Returns the error message corresponding the error code.
    function get_message(this, stat) result(msg)
        implicit none
        !&<
        class(maps_error_type)   , intent(in) :: this
            !! passed-object dummy argument
        integer(int32)              , intent(in) :: stat
            !! error status code
        !&>
        character(:), allocatable :: msg
            !! error message

        !&<
        select case (stat)
        case (success_status_code)              ; msg = success_status_msg
        case (key_already_exist%stat)           ; msg = trim(key_already_exist%msg)
        case (key_not_exist%stat)               ; msg = trim(key_not_exist%msg)
        case (conflict%stat)                    ; msg = trim(conflict%msg)
        case (value_not_exist%stat)             ; msg = trim(value_not_exist%msg)
        case (unkwnon_collision_resolver%stat)  ; msg = trim(unkwnon_collision_resolver%msg)
        case (unkwnon_hash_function%stat)       ; msg = trim(unkwnon_hash_function%msg)
        case (initialization_failed%stat)       ; msg = trim(initialization_failed%msg)
        case (value_type_error%stat)            ; msg = trim(value_type_error%msg)
        case (allocation_failed%stat)           ; msg = trim(allocation_failed%msg)
        case (deallocation_failed%stat)         ; msg = trim(deallocation_failed%msg)
        case (error_in_hashmap_type%stat)       ; msg = trim(error_in_hashmap_type%msg)
        case (warn_not_exist%stat)              ; msg = trim(warn_not_exist%msg)
        case (warn_value_not_equal%stat)        ; msg = trim(warn_value_not_equal%msg)
        case (warn_overflow_occured%stat)       ; msg = trim(warn_overflow_occured%msg)
        case default
            msg = "unkown error encountered. status code = "//stat_to_string(stat)
        end select
        !&>

        return
        if (same_type_as(this, this)) continue
    end function get_message
end module maps_common_error_repository
