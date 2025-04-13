# ieee_class_fortran

Additional functions for `ieee_class_type`

## How to run

## [Fypp — Python powered Fortran metaprogramming](https://github.com/aradi/fypp)

### src

```
cd src
fypp ieee_class_fortran.fypp ieee_class_fortran.f90
```

### test

```
cd test
fypp --line-length=256 test.fypp test.f90
```

## [fpm (Fortran Package Manager)](https://github.com/fortran-lang/fpm)

### Build

```
fpm build --flag "-Wall -Wextra -ffree-line-length-none"
```

### Test

```
fpm test --flag "-Wall -Wextra -ffree-line-length-none"
```
