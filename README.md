# maps
A practical wrapper for stdlib_hashmaps

## Motivation
Hashmaps are one of the most practical tools in versatile programming. Many versatile, relatively modern, and popular programming languages have hashmaps.

Fortran does not have hashmaps as the language standard, but hashmaps are being developed in the community-made standard library, [stdlib](https://github.com/fortran-lang/stdlib). However, the current implementation of the hashmaps requires some steps to perform the desired tasks: adding a key-value mapping and getting the value mapped to a key.

```Fortran
type(chaining_hashmap_type) :: map
call map%init(fnv_1_hasher)

block
    type(key_type) :: key
    type(other_type) :: other
    class(*), allocatable :: data

    ! adding a key-value mapping to the map
    call set(key, "apple")
    call set(other, 100)
    call map%map_entry(key, other)

    ! getting the value mapped to a key
    call set(key, "apple")
    call map%get_other_data(key, other)
    call get(other, data)
    select type (data); type is (integer(int32))
        print *, data, "is mapped to apple"
    end select
end block
```

The wrapper provides a similar feel for operations to the hashmaps in other languages, hiding the abovementioned steps.

```Fortran
use :: maps
type(char_to_int32_map_type) :: map
call map%initialize()

! adding a key-value mapping to the map
call map%put("apple", 100)

! getting the value mapped to a key
print *, map%get("apple")
```

## Getting started
### Requirements
Since maps use hashmaps in the stdlib, an environment that can build at least the stdlib is required. The compilers and versions listed below have been used to develop maps.

- Modern Fortran compiler
    - gfortran 11.2 bundled with [quickstart Fortran on Windows](https://github.com/LKedward/quickstart-fortran)
    - Intel Fortran Classic 2021.5.0 Build 20211109_000000
    - NAG Fortran 7.1 Build 7117
- [Fortran-stdlib](https://github.com/fortran-lang/stdlib)
- [Fortran Package Manager](https://github.com/fortran-lang/fpm) (fpm) 0.7.0 alpha

### Get the code
To get the code, execute the following commnad:

```console
git clone https://github.com/degawa/maps.git
cd maps
```

### Build with fpm
To build the library using fpm, execute the following command:

```console
fpm build
```

### Reference from your project
Add the following `use` statement to modules or procedures that use maps.

```Fortran
use :: maps
```

### Reference as a fpm project's dependency
To use maps in your fpm project, add the following to the fpm.toml.

```TOML
[dependencies]
maps = {git = "https://github.com/degawa/maps.git"}
```

## usage
To use a hashmap defined in maps, declare a hashmap instance and initialize it with the `initialize` procedure. When it is no longer needed, the `finalize` procedure destorys the instance.

The names of the hashmap types defined in maps follow the convention: `<type of key>_to_<type of value>_map_type`. For instance, the hashmap named `char_to_int32_map_type` maps keys of the `character(*)` type to `integer(int32)` type values.

`any` means scalar values of the intrinsic types, including `integer(int8)`, `integer(int16)`, `integer(int32)`, `integer(int64)`, `real(real32)`, `real(real64)`, `real(real128)`, `complex(real32)`, `complex(real64)`, `complex(real128)`, `logical`, and `character(*)`. User-defined types are currently not supported.

`char_to_any_map_type` can contain values of the intrinsic types. The value mapped to a key can be retrieved with the type-bound procedure `get`, regardless of its type. But conversion to the obtained type is required when putting it directly to a print or write statement, or passing it to a procedure:

```Fortran
type(char_to_any_map_type) :: map
call map%initialize()

call map%put("int32.max", huge(0_int32))
block
    integer(int32) :: i32
    i32 = map%get("int32.max")
    print *, i32, as_int32(map%get("int32.max")) ! 2147483647  2147483647
end block
```

The conversion functions are listed below:
- `as_int8()`
- `as_int16()`
- `as_int32()`
- `as_int64()`
- `as_real32()`
- `as_real64()`
- `as_real128()`
- `as_complex32()`
- `as_complex64()`
- `as_complex128()`
- `as_logical()`
- `as_char()`

### Basic examples
#### char_to_int32_map
The `char_to_int32_map_type` maps keys of the `character(*)` type to `integer(int32)` type values.

```Fortran
use :: maps
implicit none

type(char_to_int32_map_type) :: map
call map%initialize()

call map%put("apple", 100)
call map%put("orange", 150)
call map%put("banana", 200)

print *, map%get("banana") ! 200

print *, map%contains("orange") ! T
call map%remove_if("orange", 100)
print *, map%contains("orange") ! T
call map%remove_if("orange", 150)
print *, map%contains("orange") ! F
```

#### char_to_any_map
The `char_to_any_map_type` maps keys of the `character(*)` type to values of the intrinsic types.

```Fortran
use, intrinsic :: iso_fortran_env
use :: maps
implicit none

type(char_to_any_map_type) :: map
call map%initialize()

call map%put("int8.max", huge(0_int8))
call map%put("int16.max", huge(0_int16))
call map%put("int32.max", huge(0_int32))
call map%put("int64.max", huge(0_int64))
call map%put("real32.max", huge(0._real32))
call map%put("real64.max", huge(0._real64))

print *, as_int32(map%get("int32.max")) ! 2147483647
print *, as_real64(map%get("real64.max")) ! 1.7976931348623157E+308

block
    integer(int8) :: i8
    real(real32) :: r32
    i8 = map%get("int8.max")
    r32 = map%get("real32.max")
    print *, i8, r32 ! 127   3.40282347E+38
end block

call map%remove_if("real64.max", -huge(0._real64))
print *, map%contains("real64.max") ! T
```

#### any_to_int32_map
The `any_to_int32_map_type` maps keys of the intrinsic types to `integer(int32)` type values. This type is provided only for demonstrating the feasibility of the hashmaps in stdlib and may not be suitable for practical use.

```Fortran
use :: maps
implicit none

type(any_to_int32_map_type) :: map
call map%initialize()

call map%put(100, 200)
print *, map%get(100) ! 200
```

## Todo
- [ ] To add unit tests.
- [ ] To support user-defined types.
- [x] To add finializer.
- [ ] To add some procedures.
- [ ] To add some hashmaps.
- [ ] To define status code.