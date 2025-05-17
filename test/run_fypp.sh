#!/bin/sh -e
fypp -F -DMODE=1 test_ieee_is_X.fypp test_ieee_is_finite.f90
fypp -F -DMODE=2 test_ieee_is_X.fypp test_ieee_is_nan.f90
fypp -F -DMODE=3 test_ieee_is_X.fypp test_ieee_is_negative.f90

fypp -F test_is_ieee_class.fypp  test_is_ieee_class.f90
fypp -F test_set_ieee_class.fypp test_set_ieee_class.f90
