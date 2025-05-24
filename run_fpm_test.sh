#!/bin/sh -e
cd test

fypp -F          ieee_class_fortran_test.fypp ieee_class_fortran_test.f90
fypp -F          test_preconditions.fypp      test_preconditions.f90
fypp -F -DMODE=1 test_ieee_is_X.fypp          test_ieee_is_finite.f90
fypp -F -DMODE=2 test_ieee_is_X.fypp          test_ieee_is_nan.f90
fypp -F -DMODE=3 test_ieee_is_X.fypp          test_ieee_is_negative.f90
fypp -F -DMODE=1 test_is_ieee_X.fypp          test_is_ieee_negative_inf.f90
fypp -F -DMODE=2 test_is_ieee_X.fypp          test_is_ieee_negative_zero.f90
fypp -F -DMODE=3 test_is_ieee_X.fypp          test_is_ieee_either_zero.f90
fypp -F -DMODE=4 test_is_ieee_X.fypp          test_is_ieee_positive_zero.f90
fypp -F -DMODE=5 test_is_ieee_X.fypp          test_is_ieee_positive_inf.f90
fypp -F -DMODE=6 test_is_ieee_X.fypp          test_is_ieee_quiet_nan.f90
fypp -F -DMODE=7 test_is_ieee_X.fypp          test_is_ieee_signaling_nan.f90
fypp -F          test.fypp                    test.f90

fypp -F test_set_ieee_class.fypp test_set_ieee_class.f90

cd ..

fpm test --flag "-Wall -Wextra -Werror -pedantic -std=f2008 -ffree-line-length-none"
