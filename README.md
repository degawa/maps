# maps
A practical wrapper for stdlib_hashmaps

```Fortran
use :: maps
implicit none

type(map_str_int_type) :: map

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
