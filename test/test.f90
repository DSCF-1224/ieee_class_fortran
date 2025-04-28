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
    call test_is_ieee_positive_inf
    call test_is_ieee_positive_zero


    contains


    subroutine test_is_ieee_negative_inf()


        if (       is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_signaling_nan      ) ) ) error stop 'test.fypp:L50 (real32)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_quiet_nan          ) ) ) error stop 'test.fypp:L51 (real32)'
        if ( .not. is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_negative_inf       ) ) ) error stop 'test.fypp:L52 (real32)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_negative_normal    ) ) ) error stop 'test.fypp:L53 (real32)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_negative_denormal  ) ) ) error stop 'test.fypp:L54 (real32)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_negative_subnormal ) ) ) error stop 'test.fypp:L55 (real32)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_negative_zero      ) ) ) error stop 'test.fypp:L56 (real32)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_positive_zero      ) ) ) error stop 'test.fypp:L57 (real32)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_positive_subnormal ) ) ) error stop 'test.fypp:L58 (real32)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_positive_denormal  ) ) ) error stop 'test.fypp:L59 (real32)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_positive_normal    ) ) ) error stop 'test.fypp:L60 (real32)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_positive_inf       ) ) ) error stop 'test.fypp:L61 (real32)'


        if (       is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_signaling_nan      ) ) ) error stop 'test.fypp:L50 (real64)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_quiet_nan          ) ) ) error stop 'test.fypp:L51 (real64)'
        if ( .not. is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_negative_inf       ) ) ) error stop 'test.fypp:L52 (real64)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_negative_normal    ) ) ) error stop 'test.fypp:L53 (real64)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_negative_denormal  ) ) ) error stop 'test.fypp:L54 (real64)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_negative_subnormal ) ) ) error stop 'test.fypp:L55 (real64)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_negative_zero      ) ) ) error stop 'test.fypp:L56 (real64)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_positive_zero      ) ) ) error stop 'test.fypp:L57 (real64)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_positive_subnormal ) ) ) error stop 'test.fypp:L58 (real64)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_positive_denormal  ) ) ) error stop 'test.fypp:L59 (real64)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_positive_normal    ) ) ) error stop 'test.fypp:L60 (real64)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_positive_inf       ) ) ) error stop 'test.fypp:L61 (real64)'


        if (       is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_signaling_nan      ) ) ) error stop 'test.fypp:L50 (real128)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_quiet_nan          ) ) ) error stop 'test.fypp:L51 (real128)'
        if ( .not. is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_negative_inf       ) ) ) error stop 'test.fypp:L52 (real128)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_negative_normal    ) ) ) error stop 'test.fypp:L53 (real128)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_negative_denormal  ) ) ) error stop 'test.fypp:L54 (real128)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_negative_subnormal ) ) ) error stop 'test.fypp:L55 (real128)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_negative_zero      ) ) ) error stop 'test.fypp:L56 (real128)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_positive_zero      ) ) ) error stop 'test.fypp:L57 (real128)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_positive_subnormal ) ) ) error stop 'test.fypp:L58 (real128)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_positive_denormal  ) ) ) error stop 'test.fypp:L59 (real128)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_positive_normal    ) ) ) error stop 'test.fypp:L60 (real128)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_positive_inf       ) ) ) error stop 'test.fypp:L61 (real128)'


        print '(A)', 'OK: test_is_ieee_negative_inf'

    end subroutine test_is_ieee_negative_inf



    subroutine test_is_ieee_negative_zero()


        if (       is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_signaling_nan      ) ) ) error stop 'test.fypp:L77 (real32)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_quiet_nan          ) ) ) error stop 'test.fypp:L78 (real32)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_negative_inf       ) ) ) error stop 'test.fypp:L79 (real32)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_negative_normal    ) ) ) error stop 'test.fypp:L80 (real32)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_negative_denormal  ) ) ) error stop 'test.fypp:L81 (real32)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_negative_subnormal ) ) ) error stop 'test.fypp:L82 (real32)'
        if ( .not. is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_negative_zero      ) ) ) error stop 'test.fypp:L83 (real32)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_positive_zero      ) ) ) error stop 'test.fypp:L84 (real32)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_positive_subnormal ) ) ) error stop 'test.fypp:L85 (real32)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_positive_denormal  ) ) ) error stop 'test.fypp:L86 (real32)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_positive_normal    ) ) ) error stop 'test.fypp:L87 (real32)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_positive_inf       ) ) ) error stop 'test.fypp:L88 (real32)'


        if (       is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_signaling_nan      ) ) ) error stop 'test.fypp:L77 (real64)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_quiet_nan          ) ) ) error stop 'test.fypp:L78 (real64)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_negative_inf       ) ) ) error stop 'test.fypp:L79 (real64)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_negative_normal    ) ) ) error stop 'test.fypp:L80 (real64)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_negative_denormal  ) ) ) error stop 'test.fypp:L81 (real64)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_negative_subnormal ) ) ) error stop 'test.fypp:L82 (real64)'
        if ( .not. is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_negative_zero      ) ) ) error stop 'test.fypp:L83 (real64)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_positive_zero      ) ) ) error stop 'test.fypp:L84 (real64)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_positive_subnormal ) ) ) error stop 'test.fypp:L85 (real64)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_positive_denormal  ) ) ) error stop 'test.fypp:L86 (real64)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_positive_normal    ) ) ) error stop 'test.fypp:L87 (real64)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_positive_inf       ) ) ) error stop 'test.fypp:L88 (real64)'


        if (       is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_signaling_nan      ) ) ) error stop 'test.fypp:L77 (real128)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_quiet_nan          ) ) ) error stop 'test.fypp:L78 (real128)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_negative_inf       ) ) ) error stop 'test.fypp:L79 (real128)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_negative_normal    ) ) ) error stop 'test.fypp:L80 (real128)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_negative_denormal  ) ) ) error stop 'test.fypp:L81 (real128)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_negative_subnormal ) ) ) error stop 'test.fypp:L82 (real128)'
        if ( .not. is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_negative_zero      ) ) ) error stop 'test.fypp:L83 (real128)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_positive_zero      ) ) ) error stop 'test.fypp:L84 (real128)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_positive_subnormal ) ) ) error stop 'test.fypp:L85 (real128)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_positive_denormal  ) ) ) error stop 'test.fypp:L86 (real128)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_positive_normal    ) ) ) error stop 'test.fypp:L87 (real128)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_positive_inf       ) ) ) error stop 'test.fypp:L88 (real128)'


        print '(A)', 'OK: test_is_ieee_negative_zero'

    end subroutine test_is_ieee_negative_zero



    subroutine test_is_ieee_positive_inf()


        if (       is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_signaling_nan      ) ) ) error stop 'test.fypp:L104 (real32)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_quiet_nan          ) ) ) error stop 'test.fypp:L105 (real32)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_negative_inf       ) ) ) error stop 'test.fypp:L106 (real32)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_negative_normal    ) ) ) error stop 'test.fypp:L107 (real32)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_negative_denormal  ) ) ) error stop 'test.fypp:L108 (real32)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_negative_subnormal ) ) ) error stop 'test.fypp:L109 (real32)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_negative_zero      ) ) ) error stop 'test.fypp:L110 (real32)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_positive_zero      ) ) ) error stop 'test.fypp:L111 (real32)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_positive_subnormal ) ) ) error stop 'test.fypp:L112 (real32)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_positive_denormal  ) ) ) error stop 'test.fypp:L113 (real32)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_positive_normal    ) ) ) error stop 'test.fypp:L114 (real32)'
        if ( .not. is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_positive_inf       ) ) ) error stop 'test.fypp:L115 (real32)'


        if (       is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_signaling_nan      ) ) ) error stop 'test.fypp:L104 (real64)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_quiet_nan          ) ) ) error stop 'test.fypp:L105 (real64)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_negative_inf       ) ) ) error stop 'test.fypp:L106 (real64)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_negative_normal    ) ) ) error stop 'test.fypp:L107 (real64)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_negative_denormal  ) ) ) error stop 'test.fypp:L108 (real64)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_negative_subnormal ) ) ) error stop 'test.fypp:L109 (real64)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_negative_zero      ) ) ) error stop 'test.fypp:L110 (real64)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_positive_zero      ) ) ) error stop 'test.fypp:L111 (real64)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_positive_subnormal ) ) ) error stop 'test.fypp:L112 (real64)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_positive_denormal  ) ) ) error stop 'test.fypp:L113 (real64)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_positive_normal    ) ) ) error stop 'test.fypp:L114 (real64)'
        if ( .not. is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_positive_inf       ) ) ) error stop 'test.fypp:L115 (real64)'


        if (       is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_signaling_nan      ) ) ) error stop 'test.fypp:L104 (real128)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_quiet_nan          ) ) ) error stop 'test.fypp:L105 (real128)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_negative_inf       ) ) ) error stop 'test.fypp:L106 (real128)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_negative_normal    ) ) ) error stop 'test.fypp:L107 (real128)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_negative_denormal  ) ) ) error stop 'test.fypp:L108 (real128)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_negative_subnormal ) ) ) error stop 'test.fypp:L109 (real128)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_negative_zero      ) ) ) error stop 'test.fypp:L110 (real128)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_positive_zero      ) ) ) error stop 'test.fypp:L111 (real128)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_positive_subnormal ) ) ) error stop 'test.fypp:L112 (real128)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_positive_denormal  ) ) ) error stop 'test.fypp:L113 (real128)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_positive_normal    ) ) ) error stop 'test.fypp:L114 (real128)'
        if ( .not. is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_positive_inf       ) ) ) error stop 'test.fypp:L115 (real128)'


        print '(A)', 'OK: test_is_ieee_positive_inf'

    end subroutine test_is_ieee_positive_inf



    subroutine test_is_ieee_positive_zero()


        if (       is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_signaling_nan      ) ) ) error stop 'test.fypp:L131 (real32)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_quiet_nan          ) ) ) error stop 'test.fypp:L132 (real32)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_negative_inf       ) ) ) error stop 'test.fypp:L133 (real32)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_negative_normal    ) ) ) error stop 'test.fypp:L134 (real32)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_negative_denormal  ) ) ) error stop 'test.fypp:L135 (real32)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_negative_subnormal ) ) ) error stop 'test.fypp:L136 (real32)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_negative_zero      ) ) ) error stop 'test.fypp:L137 (real32)'
        if ( .not. is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_positive_zero      ) ) ) error stop 'test.fypp:L138 (real32)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_positive_subnormal ) ) ) error stop 'test.fypp:L139 (real32)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_positive_denormal  ) ) ) error stop 'test.fypp:L140 (real32)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_positive_normal    ) ) ) error stop 'test.fypp:L141 (real32)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_positive_inf       ) ) ) error stop 'test.fypp:L142 (real32)'


        if (       is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_signaling_nan      ) ) ) error stop 'test.fypp:L131 (real64)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_quiet_nan          ) ) ) error stop 'test.fypp:L132 (real64)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_negative_inf       ) ) ) error stop 'test.fypp:L133 (real64)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_negative_normal    ) ) ) error stop 'test.fypp:L134 (real64)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_negative_denormal  ) ) ) error stop 'test.fypp:L135 (real64)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_negative_subnormal ) ) ) error stop 'test.fypp:L136 (real64)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_negative_zero      ) ) ) error stop 'test.fypp:L137 (real64)'
        if ( .not. is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_positive_zero      ) ) ) error stop 'test.fypp:L138 (real64)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_positive_subnormal ) ) ) error stop 'test.fypp:L139 (real64)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_positive_denormal  ) ) ) error stop 'test.fypp:L140 (real64)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_positive_normal    ) ) ) error stop 'test.fypp:L141 (real64)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_positive_inf       ) ) ) error stop 'test.fypp:L142 (real64)'


        if (       is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_signaling_nan      ) ) ) error stop 'test.fypp:L131 (real128)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_quiet_nan          ) ) ) error stop 'test.fypp:L132 (real128)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_negative_inf       ) ) ) error stop 'test.fypp:L133 (real128)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_negative_normal    ) ) ) error stop 'test.fypp:L134 (real128)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_negative_denormal  ) ) ) error stop 'test.fypp:L135 (real128)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_negative_subnormal ) ) ) error stop 'test.fypp:L136 (real128)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_negative_zero      ) ) ) error stop 'test.fypp:L137 (real128)'
        if ( .not. is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_positive_zero      ) ) ) error stop 'test.fypp:L138 (real128)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_positive_subnormal ) ) ) error stop 'test.fypp:L139 (real128)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_positive_denormal  ) ) ) error stop 'test.fypp:L140 (real128)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_positive_normal    ) ) ) error stop 'test.fypp:L141 (real128)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_positive_inf       ) ) ) error stop 'test.fypp:L142 (real128)'


        print '(A)', 'OK: test_is_ieee_positive_zero'

    end subroutine test_is_ieee_positive_zero

end program check
