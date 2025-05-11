program check

    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128
    use, intrinsic :: iso_fortran_env, only: compiler_options
    use, intrinsic :: iso_fortran_env, only: compiler_version


    use, intrinsic :: ieee_arithmetic, only: ieee_signaling_nan
    use, intrinsic :: ieee_arithmetic, only: ieee_quiet_nan
    use, intrinsic :: ieee_arithmetic, only: ieee_negative_inf
    use, intrinsic :: ieee_arithmetic, only: ieee_negative_normal
    use, intrinsic :: ieee_arithmetic, only: ieee_negative_denormal
    use, intrinsic :: ieee_arithmetic, only: ieee_negative_subnormal
    use, intrinsic :: ieee_arithmetic, only: ieee_negative_zero
    use, intrinsic :: ieee_arithmetic, only: ieee_positive_zero
    use, intrinsic :: ieee_arithmetic, only: ieee_positive_subnormal
    use, intrinsic :: ieee_arithmetic, only: ieee_positive_denormal
    use, intrinsic :: ieee_arithmetic, only: ieee_positive_normal
    use, intrinsic :: ieee_arithmetic, only: ieee_positive_inf
    use, intrinsic :: ieee_arithmetic, only: ieee_value


    use, non_intrinsic :: ieee_class_fortran


    implicit none


    print * , compiler_version()
    print * , compiler_options()

    call test_set_ieee_negative_inf
    call test_set_ieee_negative_zero
    call test_set_ieee_positive_zero
    call test_set_ieee_positive_inf
    call test_set_ieee_quiet_nan
    call test_set_ieee_signaling_nan


    contains



    subroutine test_set_ieee_negative_inf()

        real(real32) :: test_real32
        real(real64) :: test_real64
        real(real128) :: test_real128


        call set_ieee_negative_inf(test_real32)
        call set_ieee_negative_inf(test_real64)
        call set_ieee_negative_inf(test_real128)


        if ( is_ieee_negative_inf( ieee_value( test_real32 , ieee_signaling_nan ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real32) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( test_real32 , ieee_quiet_nan ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real32) must be .false.'
        end if

        if ( .not. is_ieee_negative_inf( ieee_value( test_real32 , ieee_negative_inf ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real32) must be .true.'
        end if

        if ( is_ieee_negative_inf( ieee_value( test_real32 , ieee_negative_normal ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real32) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( test_real32 , ieee_negative_denormal ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real32) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( test_real32 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real32) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( test_real32 , ieee_negative_zero ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real32) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( test_real32 , ieee_positive_zero ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real32) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( test_real32 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real32) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( test_real32 , ieee_positive_denormal ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real32) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( test_real32 , ieee_positive_normal ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real32) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( test_real32 , ieee_positive_inf ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real32) must be .false.'
        end if


        if ( is_ieee_negative_inf( ieee_value( test_real64 , ieee_signaling_nan ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real64) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( test_real64 , ieee_quiet_nan ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real64) must be .false.'
        end if

        if ( .not. is_ieee_negative_inf( ieee_value( test_real64 , ieee_negative_inf ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real64) must be .true.'
        end if

        if ( is_ieee_negative_inf( ieee_value( test_real64 , ieee_negative_normal ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real64) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( test_real64 , ieee_negative_denormal ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real64) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( test_real64 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real64) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( test_real64 , ieee_negative_zero ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real64) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( test_real64 , ieee_positive_zero ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real64) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( test_real64 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real64) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( test_real64 , ieee_positive_denormal ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real64) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( test_real64 , ieee_positive_normal ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real64) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( test_real64 , ieee_positive_inf ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real64) must be .false.'
        end if


        if ( is_ieee_negative_inf( ieee_value( test_real128 , ieee_signaling_nan ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real128) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( test_real128 , ieee_quiet_nan ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real128) must be .false.'
        end if

        if ( .not. is_ieee_negative_inf( ieee_value( test_real128 , ieee_negative_inf ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real128) must be .true.'
        end if

        if ( is_ieee_negative_inf( ieee_value( test_real128 , ieee_negative_normal ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real128) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( test_real128 , ieee_negative_denormal ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real128) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( test_real128 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real128) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( test_real128 , ieee_negative_zero ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real128) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( test_real128 , ieee_positive_zero ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real128) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( test_real128 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real128) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( test_real128 , ieee_positive_denormal ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real128) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( test_real128 , ieee_positive_normal ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real128) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( test_real128 , ieee_positive_inf ) ) ) then
            error stop '; NG; result of set_ieee_negative_inf(test_real128) must be .false.'
        end if



        print '(A)', 'OK: test_set_ieee_negative_inf'

    end subroutine test_set_ieee_negative_inf



    subroutine test_set_ieee_negative_zero()

        real(real32) :: test_real32
        real(real64) :: test_real64
        real(real128) :: test_real128


        call set_ieee_negative_zero(test_real32)
        call set_ieee_negative_zero(test_real64)
        call set_ieee_negative_zero(test_real128)


        if ( is_ieee_negative_zero( ieee_value( test_real32 , ieee_signaling_nan ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real32) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( test_real32 , ieee_quiet_nan ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real32) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( test_real32 , ieee_negative_inf ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real32) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( test_real32 , ieee_negative_normal ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real32) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( test_real32 , ieee_negative_denormal ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real32) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( test_real32 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real32) must be .false.'
        end if

        if ( .not. is_ieee_negative_zero( ieee_value( test_real32 , ieee_negative_zero ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real32) must be .true.'
        end if

        if ( is_ieee_negative_zero( ieee_value( test_real32 , ieee_positive_zero ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real32) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( test_real32 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real32) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( test_real32 , ieee_positive_denormal ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real32) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( test_real32 , ieee_positive_normal ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real32) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( test_real32 , ieee_positive_inf ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real32) must be .false.'
        end if


        if ( is_ieee_negative_zero( ieee_value( test_real64 , ieee_signaling_nan ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real64) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( test_real64 , ieee_quiet_nan ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real64) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( test_real64 , ieee_negative_inf ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real64) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( test_real64 , ieee_negative_normal ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real64) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( test_real64 , ieee_negative_denormal ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real64) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( test_real64 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real64) must be .false.'
        end if

        if ( .not. is_ieee_negative_zero( ieee_value( test_real64 , ieee_negative_zero ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real64) must be .true.'
        end if

        if ( is_ieee_negative_zero( ieee_value( test_real64 , ieee_positive_zero ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real64) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( test_real64 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real64) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( test_real64 , ieee_positive_denormal ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real64) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( test_real64 , ieee_positive_normal ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real64) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( test_real64 , ieee_positive_inf ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real64) must be .false.'
        end if


        if ( is_ieee_negative_zero( ieee_value( test_real128 , ieee_signaling_nan ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real128) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( test_real128 , ieee_quiet_nan ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real128) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( test_real128 , ieee_negative_inf ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real128) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( test_real128 , ieee_negative_normal ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real128) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( test_real128 , ieee_negative_denormal ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real128) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( test_real128 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real128) must be .false.'
        end if

        if ( .not. is_ieee_negative_zero( ieee_value( test_real128 , ieee_negative_zero ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real128) must be .true.'
        end if

        if ( is_ieee_negative_zero( ieee_value( test_real128 , ieee_positive_zero ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real128) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( test_real128 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real128) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( test_real128 , ieee_positive_denormal ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real128) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( test_real128 , ieee_positive_normal ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real128) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( test_real128 , ieee_positive_inf ) ) ) then
            error stop '; NG; result of set_ieee_negative_zero(test_real128) must be .false.'
        end if



        print '(A)', 'OK: test_set_ieee_negative_zero'

    end subroutine test_set_ieee_negative_zero



    subroutine test_set_ieee_positive_zero()

        real(real32) :: test_real32
        real(real64) :: test_real64
        real(real128) :: test_real128


        call set_ieee_positive_zero(test_real32)
        call set_ieee_positive_zero(test_real64)
        call set_ieee_positive_zero(test_real128)


        if ( is_ieee_positive_zero( ieee_value( test_real32 , ieee_signaling_nan ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real32) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( test_real32 , ieee_quiet_nan ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real32) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( test_real32 , ieee_negative_inf ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real32) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( test_real32 , ieee_negative_normal ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real32) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( test_real32 , ieee_negative_denormal ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real32) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( test_real32 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real32) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( test_real32 , ieee_negative_zero ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real32) must be .false.'
        end if

        if ( .not. is_ieee_positive_zero( ieee_value( test_real32 , ieee_positive_zero ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real32) must be .true.'
        end if

        if ( is_ieee_positive_zero( ieee_value( test_real32 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real32) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( test_real32 , ieee_positive_denormal ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real32) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( test_real32 , ieee_positive_normal ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real32) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( test_real32 , ieee_positive_inf ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real32) must be .false.'
        end if


        if ( is_ieee_positive_zero( ieee_value( test_real64 , ieee_signaling_nan ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real64) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( test_real64 , ieee_quiet_nan ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real64) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( test_real64 , ieee_negative_inf ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real64) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( test_real64 , ieee_negative_normal ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real64) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( test_real64 , ieee_negative_denormal ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real64) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( test_real64 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real64) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( test_real64 , ieee_negative_zero ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real64) must be .false.'
        end if

        if ( .not. is_ieee_positive_zero( ieee_value( test_real64 , ieee_positive_zero ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real64) must be .true.'
        end if

        if ( is_ieee_positive_zero( ieee_value( test_real64 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real64) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( test_real64 , ieee_positive_denormal ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real64) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( test_real64 , ieee_positive_normal ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real64) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( test_real64 , ieee_positive_inf ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real64) must be .false.'
        end if


        if ( is_ieee_positive_zero( ieee_value( test_real128 , ieee_signaling_nan ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real128) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( test_real128 , ieee_quiet_nan ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real128) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( test_real128 , ieee_negative_inf ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real128) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( test_real128 , ieee_negative_normal ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real128) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( test_real128 , ieee_negative_denormal ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real128) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( test_real128 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real128) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( test_real128 , ieee_negative_zero ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real128) must be .false.'
        end if

        if ( .not. is_ieee_positive_zero( ieee_value( test_real128 , ieee_positive_zero ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real128) must be .true.'
        end if

        if ( is_ieee_positive_zero( ieee_value( test_real128 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real128) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( test_real128 , ieee_positive_denormal ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real128) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( test_real128 , ieee_positive_normal ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real128) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( test_real128 , ieee_positive_inf ) ) ) then
            error stop '; NG; result of set_ieee_positive_zero(test_real128) must be .false.'
        end if



        print '(A)', 'OK: test_set_ieee_positive_zero'

    end subroutine test_set_ieee_positive_zero



    subroutine test_set_ieee_positive_inf()

        real(real32) :: test_real32
        real(real64) :: test_real64
        real(real128) :: test_real128


        call set_ieee_positive_inf(test_real32)
        call set_ieee_positive_inf(test_real64)
        call set_ieee_positive_inf(test_real128)


        if ( is_ieee_positive_inf( ieee_value( test_real32 , ieee_signaling_nan ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real32) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( test_real32 , ieee_quiet_nan ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real32) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( test_real32 , ieee_negative_inf ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real32) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( test_real32 , ieee_negative_normal ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real32) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( test_real32 , ieee_negative_denormal ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real32) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( test_real32 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real32) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( test_real32 , ieee_negative_zero ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real32) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( test_real32 , ieee_positive_zero ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real32) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( test_real32 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real32) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( test_real32 , ieee_positive_denormal ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real32) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( test_real32 , ieee_positive_normal ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real32) must be .false.'
        end if

        if ( .not. is_ieee_positive_inf( ieee_value( test_real32 , ieee_positive_inf ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real32) must be .true.'
        end if


        if ( is_ieee_positive_inf( ieee_value( test_real64 , ieee_signaling_nan ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real64) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( test_real64 , ieee_quiet_nan ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real64) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( test_real64 , ieee_negative_inf ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real64) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( test_real64 , ieee_negative_normal ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real64) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( test_real64 , ieee_negative_denormal ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real64) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( test_real64 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real64) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( test_real64 , ieee_negative_zero ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real64) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( test_real64 , ieee_positive_zero ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real64) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( test_real64 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real64) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( test_real64 , ieee_positive_denormal ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real64) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( test_real64 , ieee_positive_normal ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real64) must be .false.'
        end if

        if ( .not. is_ieee_positive_inf( ieee_value( test_real64 , ieee_positive_inf ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real64) must be .true.'
        end if


        if ( is_ieee_positive_inf( ieee_value( test_real128 , ieee_signaling_nan ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real128) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( test_real128 , ieee_quiet_nan ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real128) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( test_real128 , ieee_negative_inf ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real128) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( test_real128 , ieee_negative_normal ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real128) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( test_real128 , ieee_negative_denormal ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real128) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( test_real128 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real128) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( test_real128 , ieee_negative_zero ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real128) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( test_real128 , ieee_positive_zero ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real128) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( test_real128 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real128) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( test_real128 , ieee_positive_denormal ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real128) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( test_real128 , ieee_positive_normal ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real128) must be .false.'
        end if

        if ( .not. is_ieee_positive_inf( ieee_value( test_real128 , ieee_positive_inf ) ) ) then
            error stop '; NG; result of set_ieee_positive_inf(test_real128) must be .true.'
        end if



        print '(A)', 'OK: test_set_ieee_positive_inf'

    end subroutine test_set_ieee_positive_inf



    subroutine test_set_ieee_quiet_nan()

        real(real32) :: test_real32
        real(real64) :: test_real64
        real(real128) :: test_real128


        call set_ieee_quiet_nan(test_real32)
        call set_ieee_quiet_nan(test_real64)
        call set_ieee_quiet_nan(test_real128)


        if ( is_ieee_quiet_nan( ieee_value( test_real32 , ieee_signaling_nan ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real32) must be .false.'
        end if

        if ( .not. is_ieee_quiet_nan( ieee_value( test_real32 , ieee_quiet_nan ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real32) must be .true.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( test_real32 , ieee_negative_inf ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real32) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( test_real32 , ieee_negative_normal ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real32) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( test_real32 , ieee_negative_denormal ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real32) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( test_real32 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real32) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( test_real32 , ieee_negative_zero ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real32) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( test_real32 , ieee_positive_zero ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real32) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( test_real32 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real32) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( test_real32 , ieee_positive_denormal ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real32) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( test_real32 , ieee_positive_normal ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real32) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( test_real32 , ieee_positive_inf ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real32) must be .false.'
        end if


        if ( is_ieee_quiet_nan( ieee_value( test_real64 , ieee_signaling_nan ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real64) must be .false.'
        end if

        if ( .not. is_ieee_quiet_nan( ieee_value( test_real64 , ieee_quiet_nan ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real64) must be .true.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( test_real64 , ieee_negative_inf ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real64) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( test_real64 , ieee_negative_normal ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real64) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( test_real64 , ieee_negative_denormal ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real64) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( test_real64 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real64) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( test_real64 , ieee_negative_zero ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real64) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( test_real64 , ieee_positive_zero ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real64) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( test_real64 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real64) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( test_real64 , ieee_positive_denormal ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real64) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( test_real64 , ieee_positive_normal ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real64) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( test_real64 , ieee_positive_inf ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real64) must be .false.'
        end if


        if ( is_ieee_quiet_nan( ieee_value( test_real128 , ieee_signaling_nan ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real128) must be .false.'
        end if

        if ( .not. is_ieee_quiet_nan( ieee_value( test_real128 , ieee_quiet_nan ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real128) must be .true.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( test_real128 , ieee_negative_inf ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real128) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( test_real128 , ieee_negative_normal ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real128) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( test_real128 , ieee_negative_denormal ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real128) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( test_real128 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real128) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( test_real128 , ieee_negative_zero ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real128) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( test_real128 , ieee_positive_zero ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real128) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( test_real128 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real128) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( test_real128 , ieee_positive_denormal ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real128) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( test_real128 , ieee_positive_normal ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real128) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( test_real128 , ieee_positive_inf ) ) ) then
            error stop '; NG; result of set_ieee_quiet_nan(test_real128) must be .false.'
        end if



        print '(A)', 'OK: test_set_ieee_quiet_nan'

    end subroutine test_set_ieee_quiet_nan



    subroutine test_set_ieee_signaling_nan()

        real(real32) :: test_real32
        real(real64) :: test_real64
        real(real128) :: test_real128


        call set_ieee_signaling_nan(test_real32)
        call set_ieee_signaling_nan(test_real64)
        call set_ieee_signaling_nan(test_real128)


        if ( .not. is_ieee_signaling_nan( ieee_value( test_real32 , ieee_signaling_nan ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real32) must be .true.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real32 , ieee_quiet_nan ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real32) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real32 , ieee_negative_inf ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real32) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real32 , ieee_negative_normal ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real32) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real32 , ieee_negative_denormal ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real32) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real32 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real32) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real32 , ieee_negative_zero ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real32) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real32 , ieee_positive_zero ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real32) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real32 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real32) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real32 , ieee_positive_denormal ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real32) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real32 , ieee_positive_normal ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real32) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real32 , ieee_positive_inf ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real32) must be .false.'
        end if


        if ( .not. is_ieee_signaling_nan( ieee_value( test_real64 , ieee_signaling_nan ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real64) must be .true.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real64 , ieee_quiet_nan ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real64) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real64 , ieee_negative_inf ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real64) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real64 , ieee_negative_normal ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real64) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real64 , ieee_negative_denormal ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real64) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real64 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real64) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real64 , ieee_negative_zero ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real64) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real64 , ieee_positive_zero ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real64) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real64 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real64) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real64 , ieee_positive_denormal ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real64) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real64 , ieee_positive_normal ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real64) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real64 , ieee_positive_inf ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real64) must be .false.'
        end if


        if ( .not. is_ieee_signaling_nan( ieee_value( test_real128 , ieee_signaling_nan ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real128) must be .true.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real128 , ieee_quiet_nan ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real128) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real128 , ieee_negative_inf ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real128) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real128 , ieee_negative_normal ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real128) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real128 , ieee_negative_denormal ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real128) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real128 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real128) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real128 , ieee_negative_zero ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real128) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real128 , ieee_positive_zero ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real128) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real128 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real128) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real128 , ieee_positive_denormal ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real128) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real128 , ieee_positive_normal ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real128) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( test_real128 , ieee_positive_inf ) ) ) then
            error stop '; NG; result of set_ieee_signaling_nan(test_real128) must be .false.'
        end if



        print '(A)', 'OK: test_set_ieee_signaling_nan'

    end subroutine test_set_ieee_signaling_nan

end program check
