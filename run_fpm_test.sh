#!/bin/sh -e
fpm test --flag "-Wall -Wextra -Werror -pedantic -std=f2008 -ffree-line-length-none"
