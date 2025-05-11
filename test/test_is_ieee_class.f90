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

    call test_is_ieee_negative_inf
    call test_is_ieee_negative_zero
    call test_is_ieee_either_zero
    call test_is_ieee_positive_zero
    call test_is_ieee_positive_inf
    call test_is_ieee_quiet_nan
    call test_is_ieee_signaling_nan


    contains



    subroutine test_is_ieee_negative_inf()

        if ( is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_signaling_nan ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_signaling_nan as real32) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_quiet_nan ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_quiet_nan as real32) must be .false.'
        end if

        if ( .not. is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_negative_inf ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_negative_inf as real32) must be .true.'
        end if

        if ( is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_negative_normal ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_negative_normal as real32) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_negative_denormal ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_negative_denormal as real32) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_negative_subnormal as real32) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_negative_zero ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_negative_zero as real32) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_positive_zero ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_positive_zero as real32) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_positive_subnormal as real32) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_positive_denormal ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_positive_denormal as real32) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_positive_normal ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_positive_normal as real32) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_positive_inf ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_positive_inf as real32) must be .false.'
        end if


        if ( is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_signaling_nan ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_signaling_nan as real64) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_quiet_nan ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_quiet_nan as real64) must be .false.'
        end if

        if ( .not. is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_negative_inf ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_negative_inf as real64) must be .true.'
        end if

        if ( is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_negative_normal ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_negative_normal as real64) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_negative_denormal ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_negative_denormal as real64) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_negative_subnormal as real64) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_negative_zero ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_negative_zero as real64) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_positive_zero ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_positive_zero as real64) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_positive_subnormal as real64) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_positive_denormal ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_positive_denormal as real64) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_positive_normal ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_positive_normal as real64) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_positive_inf ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_positive_inf as real64) must be .false.'
        end if


        if ( is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_signaling_nan ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_signaling_nan as real128) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_quiet_nan ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_quiet_nan as real128) must be .false.'
        end if

        if ( .not. is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_negative_inf ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_negative_inf as real128) must be .true.'
        end if

        if ( is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_negative_normal ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_negative_normal as real128) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_negative_denormal ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_negative_denormal as real128) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_negative_subnormal as real128) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_negative_zero ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_negative_zero as real128) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_positive_zero ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_positive_zero as real128) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_positive_subnormal as real128) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_positive_denormal ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_positive_denormal as real128) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_positive_normal ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_positive_normal as real128) must be .false.'
        end if

        if ( is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_positive_inf ) ) ) then
            error stop '; NG; is_ieee_negative_inf(ieee_positive_inf as real128) must be .false.'
        end if



        print '(A)', 'OK: test_is_ieee_negative_inf'

    end subroutine test_is_ieee_negative_inf



    subroutine test_is_ieee_negative_zero()

        if ( is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_signaling_nan ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_signaling_nan as real32) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_quiet_nan ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_quiet_nan as real32) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_negative_inf ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_negative_inf as real32) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_negative_normal ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_negative_normal as real32) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_negative_denormal ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_negative_denormal as real32) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_negative_subnormal as real32) must be .false.'
        end if

        if ( .not. is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_negative_zero ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_negative_zero as real32) must be .true.'
        end if

        if ( is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_positive_zero ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_positive_zero as real32) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_positive_subnormal as real32) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_positive_denormal ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_positive_denormal as real32) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_positive_normal ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_positive_normal as real32) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_positive_inf ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_positive_inf as real32) must be .false.'
        end if


        if ( is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_signaling_nan ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_signaling_nan as real64) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_quiet_nan ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_quiet_nan as real64) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_negative_inf ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_negative_inf as real64) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_negative_normal ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_negative_normal as real64) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_negative_denormal ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_negative_denormal as real64) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_negative_subnormal as real64) must be .false.'
        end if

        if ( .not. is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_negative_zero ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_negative_zero as real64) must be .true.'
        end if

        if ( is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_positive_zero ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_positive_zero as real64) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_positive_subnormal as real64) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_positive_denormal ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_positive_denormal as real64) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_positive_normal ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_positive_normal as real64) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_positive_inf ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_positive_inf as real64) must be .false.'
        end if


        if ( is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_signaling_nan ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_signaling_nan as real128) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_quiet_nan ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_quiet_nan as real128) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_negative_inf ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_negative_inf as real128) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_negative_normal ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_negative_normal as real128) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_negative_denormal ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_negative_denormal as real128) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_negative_subnormal as real128) must be .false.'
        end if

        if ( .not. is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_negative_zero ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_negative_zero as real128) must be .true.'
        end if

        if ( is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_positive_zero ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_positive_zero as real128) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_positive_subnormal as real128) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_positive_denormal ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_positive_denormal as real128) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_positive_normal ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_positive_normal as real128) must be .false.'
        end if

        if ( is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_positive_inf ) ) ) then
            error stop '; NG; is_ieee_negative_zero(ieee_positive_inf as real128) must be .false.'
        end if



        print '(A)', 'OK: test_is_ieee_negative_zero'

    end subroutine test_is_ieee_negative_zero



    subroutine test_is_ieee_either_zero()

        if ( is_ieee_either_zero( ieee_value( 0.0_real32 , ieee_signaling_nan ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_signaling_nan as real32) must be .false.'
        end if

        if ( is_ieee_either_zero( ieee_value( 0.0_real32 , ieee_quiet_nan ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_quiet_nan as real32) must be .false.'
        end if

        if ( is_ieee_either_zero( ieee_value( 0.0_real32 , ieee_negative_inf ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_negative_inf as real32) must be .false.'
        end if

        if ( is_ieee_either_zero( ieee_value( 0.0_real32 , ieee_negative_normal ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_negative_normal as real32) must be .false.'
        end if

        if ( is_ieee_either_zero( ieee_value( 0.0_real32 , ieee_negative_denormal ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_negative_denormal as real32) must be .false.'
        end if

        if ( is_ieee_either_zero( ieee_value( 0.0_real32 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_negative_subnormal as real32) must be .false.'
        end if

        if ( .not. is_ieee_either_zero( ieee_value( 0.0_real32 , ieee_negative_zero ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_negative_zero as real32) must be .true.'
        end if

        if ( .not. is_ieee_either_zero( ieee_value( 0.0_real32 , ieee_positive_zero ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_positive_zero as real32) must be .true.'
        end if

        if ( is_ieee_either_zero( ieee_value( 0.0_real32 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_positive_subnormal as real32) must be .false.'
        end if

        if ( is_ieee_either_zero( ieee_value( 0.0_real32 , ieee_positive_denormal ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_positive_denormal as real32) must be .false.'
        end if

        if ( is_ieee_either_zero( ieee_value( 0.0_real32 , ieee_positive_normal ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_positive_normal as real32) must be .false.'
        end if

        if ( is_ieee_either_zero( ieee_value( 0.0_real32 , ieee_positive_inf ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_positive_inf as real32) must be .false.'
        end if


        if ( is_ieee_either_zero( ieee_value( 0.0_real64 , ieee_signaling_nan ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_signaling_nan as real64) must be .false.'
        end if

        if ( is_ieee_either_zero( ieee_value( 0.0_real64 , ieee_quiet_nan ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_quiet_nan as real64) must be .false.'
        end if

        if ( is_ieee_either_zero( ieee_value( 0.0_real64 , ieee_negative_inf ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_negative_inf as real64) must be .false.'
        end if

        if ( is_ieee_either_zero( ieee_value( 0.0_real64 , ieee_negative_normal ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_negative_normal as real64) must be .false.'
        end if

        if ( is_ieee_either_zero( ieee_value( 0.0_real64 , ieee_negative_denormal ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_negative_denormal as real64) must be .false.'
        end if

        if ( is_ieee_either_zero( ieee_value( 0.0_real64 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_negative_subnormal as real64) must be .false.'
        end if

        if ( .not. is_ieee_either_zero( ieee_value( 0.0_real64 , ieee_negative_zero ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_negative_zero as real64) must be .true.'
        end if

        if ( .not. is_ieee_either_zero( ieee_value( 0.0_real64 , ieee_positive_zero ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_positive_zero as real64) must be .true.'
        end if

        if ( is_ieee_either_zero( ieee_value( 0.0_real64 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_positive_subnormal as real64) must be .false.'
        end if

        if ( is_ieee_either_zero( ieee_value( 0.0_real64 , ieee_positive_denormal ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_positive_denormal as real64) must be .false.'
        end if

        if ( is_ieee_either_zero( ieee_value( 0.0_real64 , ieee_positive_normal ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_positive_normal as real64) must be .false.'
        end if

        if ( is_ieee_either_zero( ieee_value( 0.0_real64 , ieee_positive_inf ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_positive_inf as real64) must be .false.'
        end if


        if ( is_ieee_either_zero( ieee_value( 0.0_real128 , ieee_signaling_nan ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_signaling_nan as real128) must be .false.'
        end if

        if ( is_ieee_either_zero( ieee_value( 0.0_real128 , ieee_quiet_nan ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_quiet_nan as real128) must be .false.'
        end if

        if ( is_ieee_either_zero( ieee_value( 0.0_real128 , ieee_negative_inf ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_negative_inf as real128) must be .false.'
        end if

        if ( is_ieee_either_zero( ieee_value( 0.0_real128 , ieee_negative_normal ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_negative_normal as real128) must be .false.'
        end if

        if ( is_ieee_either_zero( ieee_value( 0.0_real128 , ieee_negative_denormal ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_negative_denormal as real128) must be .false.'
        end if

        if ( is_ieee_either_zero( ieee_value( 0.0_real128 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_negative_subnormal as real128) must be .false.'
        end if

        if ( .not. is_ieee_either_zero( ieee_value( 0.0_real128 , ieee_negative_zero ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_negative_zero as real128) must be .true.'
        end if

        if ( .not. is_ieee_either_zero( ieee_value( 0.0_real128 , ieee_positive_zero ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_positive_zero as real128) must be .true.'
        end if

        if ( is_ieee_either_zero( ieee_value( 0.0_real128 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_positive_subnormal as real128) must be .false.'
        end if

        if ( is_ieee_either_zero( ieee_value( 0.0_real128 , ieee_positive_denormal ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_positive_denormal as real128) must be .false.'
        end if

        if ( is_ieee_either_zero( ieee_value( 0.0_real128 , ieee_positive_normal ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_positive_normal as real128) must be .false.'
        end if

        if ( is_ieee_either_zero( ieee_value( 0.0_real128 , ieee_positive_inf ) ) ) then
            error stop '; NG; is_ieee_either_zero(ieee_positive_inf as real128) must be .false.'
        end if



        print '(A)', 'OK: test_is_ieee_either_zero'

    end subroutine test_is_ieee_either_zero



    subroutine test_is_ieee_positive_zero()

        if ( is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_signaling_nan ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_signaling_nan as real32) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_quiet_nan ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_quiet_nan as real32) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_negative_inf ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_negative_inf as real32) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_negative_normal ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_negative_normal as real32) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_negative_denormal ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_negative_denormal as real32) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_negative_subnormal as real32) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_negative_zero ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_negative_zero as real32) must be .false.'
        end if

        if ( .not. is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_positive_zero ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_positive_zero as real32) must be .true.'
        end if

        if ( is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_positive_subnormal as real32) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_positive_denormal ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_positive_denormal as real32) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_positive_normal ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_positive_normal as real32) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_positive_inf ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_positive_inf as real32) must be .false.'
        end if


        if ( is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_signaling_nan ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_signaling_nan as real64) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_quiet_nan ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_quiet_nan as real64) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_negative_inf ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_negative_inf as real64) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_negative_normal ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_negative_normal as real64) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_negative_denormal ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_negative_denormal as real64) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_negative_subnormal as real64) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_negative_zero ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_negative_zero as real64) must be .false.'
        end if

        if ( .not. is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_positive_zero ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_positive_zero as real64) must be .true.'
        end if

        if ( is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_positive_subnormal as real64) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_positive_denormal ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_positive_denormal as real64) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_positive_normal ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_positive_normal as real64) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_positive_inf ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_positive_inf as real64) must be .false.'
        end if


        if ( is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_signaling_nan ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_signaling_nan as real128) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_quiet_nan ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_quiet_nan as real128) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_negative_inf ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_negative_inf as real128) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_negative_normal ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_negative_normal as real128) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_negative_denormal ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_negative_denormal as real128) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_negative_subnormal as real128) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_negative_zero ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_negative_zero as real128) must be .false.'
        end if

        if ( .not. is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_positive_zero ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_positive_zero as real128) must be .true.'
        end if

        if ( is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_positive_subnormal as real128) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_positive_denormal ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_positive_denormal as real128) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_positive_normal ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_positive_normal as real128) must be .false.'
        end if

        if ( is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_positive_inf ) ) ) then
            error stop '; NG; is_ieee_positive_zero(ieee_positive_inf as real128) must be .false.'
        end if



        print '(A)', 'OK: test_is_ieee_positive_zero'

    end subroutine test_is_ieee_positive_zero



    subroutine test_is_ieee_positive_inf()

        if ( is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_signaling_nan ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_signaling_nan as real32) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_quiet_nan ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_quiet_nan as real32) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_negative_inf ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_negative_inf as real32) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_negative_normal ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_negative_normal as real32) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_negative_denormal ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_negative_denormal as real32) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_negative_subnormal as real32) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_negative_zero ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_negative_zero as real32) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_positive_zero ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_positive_zero as real32) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_positive_subnormal as real32) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_positive_denormal ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_positive_denormal as real32) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_positive_normal ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_positive_normal as real32) must be .false.'
        end if

        if ( .not. is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_positive_inf ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_positive_inf as real32) must be .true.'
        end if


        if ( is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_signaling_nan ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_signaling_nan as real64) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_quiet_nan ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_quiet_nan as real64) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_negative_inf ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_negative_inf as real64) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_negative_normal ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_negative_normal as real64) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_negative_denormal ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_negative_denormal as real64) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_negative_subnormal as real64) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_negative_zero ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_negative_zero as real64) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_positive_zero ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_positive_zero as real64) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_positive_subnormal as real64) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_positive_denormal ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_positive_denormal as real64) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_positive_normal ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_positive_normal as real64) must be .false.'
        end if

        if ( .not. is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_positive_inf ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_positive_inf as real64) must be .true.'
        end if


        if ( is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_signaling_nan ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_signaling_nan as real128) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_quiet_nan ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_quiet_nan as real128) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_negative_inf ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_negative_inf as real128) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_negative_normal ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_negative_normal as real128) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_negative_denormal ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_negative_denormal as real128) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_negative_subnormal as real128) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_negative_zero ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_negative_zero as real128) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_positive_zero ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_positive_zero as real128) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_positive_subnormal as real128) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_positive_denormal ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_positive_denormal as real128) must be .false.'
        end if

        if ( is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_positive_normal ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_positive_normal as real128) must be .false.'
        end if

        if ( .not. is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_positive_inf ) ) ) then
            error stop '; NG; is_ieee_positive_inf(ieee_positive_inf as real128) must be .true.'
        end if



        print '(A)', 'OK: test_is_ieee_positive_inf'

    end subroutine test_is_ieee_positive_inf



    subroutine test_is_ieee_quiet_nan()

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real32 , ieee_signaling_nan ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_signaling_nan as real32) must be .false.'
        end if

        if ( .not. is_ieee_quiet_nan( ieee_value( 0.0_real32 , ieee_quiet_nan ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_quiet_nan as real32) must be .true.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real32 , ieee_negative_inf ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_negative_inf as real32) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real32 , ieee_negative_normal ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_negative_normal as real32) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real32 , ieee_negative_denormal ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_negative_denormal as real32) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real32 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_negative_subnormal as real32) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real32 , ieee_negative_zero ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_negative_zero as real32) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real32 , ieee_positive_zero ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_positive_zero as real32) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real32 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_positive_subnormal as real32) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real32 , ieee_positive_denormal ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_positive_denormal as real32) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real32 , ieee_positive_normal ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_positive_normal as real32) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real32 , ieee_positive_inf ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_positive_inf as real32) must be .false.'
        end if


        if ( is_ieee_quiet_nan( ieee_value( 0.0_real64 , ieee_signaling_nan ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_signaling_nan as real64) must be .false.'
        end if

        if ( .not. is_ieee_quiet_nan( ieee_value( 0.0_real64 , ieee_quiet_nan ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_quiet_nan as real64) must be .true.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real64 , ieee_negative_inf ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_negative_inf as real64) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real64 , ieee_negative_normal ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_negative_normal as real64) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real64 , ieee_negative_denormal ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_negative_denormal as real64) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real64 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_negative_subnormal as real64) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real64 , ieee_negative_zero ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_negative_zero as real64) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real64 , ieee_positive_zero ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_positive_zero as real64) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real64 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_positive_subnormal as real64) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real64 , ieee_positive_denormal ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_positive_denormal as real64) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real64 , ieee_positive_normal ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_positive_normal as real64) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real64 , ieee_positive_inf ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_positive_inf as real64) must be .false.'
        end if


        if ( is_ieee_quiet_nan( ieee_value( 0.0_real128 , ieee_signaling_nan ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_signaling_nan as real128) must be .false.'
        end if

        if ( .not. is_ieee_quiet_nan( ieee_value( 0.0_real128 , ieee_quiet_nan ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_quiet_nan as real128) must be .true.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real128 , ieee_negative_inf ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_negative_inf as real128) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real128 , ieee_negative_normal ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_negative_normal as real128) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real128 , ieee_negative_denormal ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_negative_denormal as real128) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real128 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_negative_subnormal as real128) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real128 , ieee_negative_zero ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_negative_zero as real128) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real128 , ieee_positive_zero ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_positive_zero as real128) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real128 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_positive_subnormal as real128) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real128 , ieee_positive_denormal ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_positive_denormal as real128) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real128 , ieee_positive_normal ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_positive_normal as real128) must be .false.'
        end if

        if ( is_ieee_quiet_nan( ieee_value( 0.0_real128 , ieee_positive_inf ) ) ) then
            error stop '; NG; is_ieee_quiet_nan(ieee_positive_inf as real128) must be .false.'
        end if



        print '(A)', 'OK: test_is_ieee_quiet_nan'

    end subroutine test_is_ieee_quiet_nan



    subroutine test_is_ieee_signaling_nan()

        if ( .not. is_ieee_signaling_nan( ieee_value( 0.0_real32 , ieee_signaling_nan ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_signaling_nan as real32) must be .true.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real32 , ieee_quiet_nan ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_quiet_nan as real32) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real32 , ieee_negative_inf ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_negative_inf as real32) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real32 , ieee_negative_normal ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_negative_normal as real32) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real32 , ieee_negative_denormal ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_negative_denormal as real32) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real32 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_negative_subnormal as real32) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real32 , ieee_negative_zero ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_negative_zero as real32) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real32 , ieee_positive_zero ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_positive_zero as real32) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real32 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_positive_subnormal as real32) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real32 , ieee_positive_denormal ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_positive_denormal as real32) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real32 , ieee_positive_normal ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_positive_normal as real32) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real32 , ieee_positive_inf ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_positive_inf as real32) must be .false.'
        end if


        if ( .not. is_ieee_signaling_nan( ieee_value( 0.0_real64 , ieee_signaling_nan ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_signaling_nan as real64) must be .true.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real64 , ieee_quiet_nan ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_quiet_nan as real64) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real64 , ieee_negative_inf ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_negative_inf as real64) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real64 , ieee_negative_normal ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_negative_normal as real64) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real64 , ieee_negative_denormal ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_negative_denormal as real64) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real64 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_negative_subnormal as real64) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real64 , ieee_negative_zero ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_negative_zero as real64) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real64 , ieee_positive_zero ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_positive_zero as real64) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real64 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_positive_subnormal as real64) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real64 , ieee_positive_denormal ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_positive_denormal as real64) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real64 , ieee_positive_normal ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_positive_normal as real64) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real64 , ieee_positive_inf ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_positive_inf as real64) must be .false.'
        end if


        if ( .not. is_ieee_signaling_nan( ieee_value( 0.0_real128 , ieee_signaling_nan ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_signaling_nan as real128) must be .true.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real128 , ieee_quiet_nan ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_quiet_nan as real128) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real128 , ieee_negative_inf ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_negative_inf as real128) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real128 , ieee_negative_normal ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_negative_normal as real128) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real128 , ieee_negative_denormal ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_negative_denormal as real128) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real128 , ieee_negative_subnormal ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_negative_subnormal as real128) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real128 , ieee_negative_zero ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_negative_zero as real128) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real128 , ieee_positive_zero ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_positive_zero as real128) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real128 , ieee_positive_subnormal ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_positive_subnormal as real128) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real128 , ieee_positive_denormal ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_positive_denormal as real128) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real128 , ieee_positive_normal ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_positive_normal as real128) must be .false.'
        end if

        if ( is_ieee_signaling_nan( ieee_value( 0.0_real128 , ieee_positive_inf ) ) ) then
            error stop '; NG; is_ieee_signaling_nan(ieee_positive_inf as real128) must be .false.'
        end if



        print '(A)', 'OK: test_is_ieee_signaling_nan'

    end subroutine test_is_ieee_signaling_nan

end program check
