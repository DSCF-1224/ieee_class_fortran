#!/bin/sh -e
fypp -F test_is_ieee_class.fypp  test_is_ieee_class.f90
fypp -F test_set_ieee_class.fypp test_set_ieee_class.f90
