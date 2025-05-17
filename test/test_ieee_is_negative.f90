program check

    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128
    use, intrinsic :: iso_fortran_env, only: compiler_options
    use, intrinsic :: iso_fortran_env, only: compiler_version


    use, intrinsic :: ieee_arithmetic, only: ieee_is_negative


    use, non_intrinsic :: ieee_class_fortran


    implicit none


    print * , compiler_version()
    print * , compiler_options()

    call test_ieee_is_negative_real32
    call test_ieee_is_negative_real64
    call test_ieee_is_negative_real128

    print '(A)', 'OK: test_ieee_is_negative'


    contains


    subroutine test_ieee_is_negative_real32()


        block

            real(real32) :: test_real32

            call set_ieee_negative_inf(test_real32)
            if ( .not. ieee_is_negative(test_real32) ) error stop '; NG; result of set_ieee_negative_inf(test_real32) must be .true.'

        end block


        block

            real(real32) :: test_real32

            call set_ieee_negative_zero(test_real32)
            if ( .not. ieee_is_negative(test_real32) ) error stop '; NG; result of set_ieee_negative_zero(test_real32) must be .true.'

        end block




        block

            real(real32) :: test_real32

            call set_ieee_positive_zero(test_real32)
            if ( ieee_is_negative(test_real32) ) error stop '; NG; result of set_ieee_positive_zero(test_real32) must be .false.'

        end block


        block

            real(real32) :: test_real32

            call set_ieee_positive_inf(test_real32)
            if ( ieee_is_negative(test_real32) ) error stop '; NG; result of set_ieee_positive_inf(test_real32) must be .false.'

        end block


    end subroutine test_ieee_is_negative_real32

    subroutine test_ieee_is_negative_real64()


        block

            real(real64) :: test_real64

            call set_ieee_negative_inf(test_real64)
            if ( .not. ieee_is_negative(test_real64) ) error stop '; NG; result of set_ieee_negative_inf(test_real64) must be .true.'

        end block


        block

            real(real64) :: test_real64

            call set_ieee_negative_zero(test_real64)
            if ( .not. ieee_is_negative(test_real64) ) error stop '; NG; result of set_ieee_negative_zero(test_real64) must be .true.'

        end block




        block

            real(real64) :: test_real64

            call set_ieee_positive_zero(test_real64)
            if ( ieee_is_negative(test_real64) ) error stop '; NG; result of set_ieee_positive_zero(test_real64) must be .false.'

        end block


        block

            real(real64) :: test_real64

            call set_ieee_positive_inf(test_real64)
            if ( ieee_is_negative(test_real64) ) error stop '; NG; result of set_ieee_positive_inf(test_real64) must be .false.'

        end block


    end subroutine test_ieee_is_negative_real64

    subroutine test_ieee_is_negative_real128()


        block

            real(real128) :: test_real128

            call set_ieee_negative_inf(test_real128)
            if ( .not. ieee_is_negative(test_real128) ) error stop '; NG; result of set_ieee_negative_inf(test_real128) must be .true.'

        end block


        block

            real(real128) :: test_real128

            call set_ieee_negative_zero(test_real128)
            if ( .not. ieee_is_negative(test_real128) ) error stop '; NG; result of set_ieee_negative_zero(test_real128) must be .true.'

        end block




        block

            real(real128) :: test_real128

            call set_ieee_positive_zero(test_real128)
            if ( ieee_is_negative(test_real128) ) error stop '; NG; result of set_ieee_positive_zero(test_real128) must be .false.'

        end block


        block

            real(real128) :: test_real128

            call set_ieee_positive_inf(test_real128)
            if ( ieee_is_negative(test_real128) ) error stop '; NG; result of set_ieee_positive_inf(test_real128) must be .false.'

        end block


    end subroutine test_ieee_is_negative_real128

end program check
