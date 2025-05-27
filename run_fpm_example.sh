#!/bin/sh -e
cd example

fypp -F demo.fypp demo.f90

cd ..

fpm run --example
