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


        if (       is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_signaling_nan      ) ) ) error stop 'test.fypp:L58 (real32)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_quiet_nan          ) ) ) error stop 'test.fypp:L59 (real32)'
        if ( .not. is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_negative_inf       ) ) ) error stop 'test.fypp:L60 (real32)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_negative_normal    ) ) ) error stop 'test.fypp:L61 (real32)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_negative_denormal  ) ) ) error stop 'test.fypp:L62 (real32)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_negative_subnormal ) ) ) error stop 'test.fypp:L63 (real32)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_negative_zero      ) ) ) error stop 'test.fypp:L64 (real32)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_positive_zero      ) ) ) error stop 'test.fypp:L65 (real32)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_positive_subnormal ) ) ) error stop 'test.fypp:L66 (real32)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_positive_denormal  ) ) ) error stop 'test.fypp:L67 (real32)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_positive_normal    ) ) ) error stop 'test.fypp:L68 (real32)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real32 , ieee_positive_inf       ) ) ) error stop 'test.fypp:L69 (real32)'


        if (       is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_signaling_nan      ) ) ) error stop 'test.fypp:L58 (real64)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_quiet_nan          ) ) ) error stop 'test.fypp:L59 (real64)'
        if ( .not. is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_negative_inf       ) ) ) error stop 'test.fypp:L60 (real64)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_negative_normal    ) ) ) error stop 'test.fypp:L61 (real64)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_negative_denormal  ) ) ) error stop 'test.fypp:L62 (real64)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_negative_subnormal ) ) ) error stop 'test.fypp:L63 (real64)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_negative_zero      ) ) ) error stop 'test.fypp:L64 (real64)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_positive_zero      ) ) ) error stop 'test.fypp:L65 (real64)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_positive_subnormal ) ) ) error stop 'test.fypp:L66 (real64)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_positive_denormal  ) ) ) error stop 'test.fypp:L67 (real64)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_positive_normal    ) ) ) error stop 'test.fypp:L68 (real64)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real64 , ieee_positive_inf       ) ) ) error stop 'test.fypp:L69 (real64)'


        if (       is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_signaling_nan      ) ) ) error stop 'test.fypp:L58 (real128)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_quiet_nan          ) ) ) error stop 'test.fypp:L59 (real128)'
        if ( .not. is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_negative_inf       ) ) ) error stop 'test.fypp:L60 (real128)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_negative_normal    ) ) ) error stop 'test.fypp:L61 (real128)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_negative_denormal  ) ) ) error stop 'test.fypp:L62 (real128)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_negative_subnormal ) ) ) error stop 'test.fypp:L63 (real128)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_negative_zero      ) ) ) error stop 'test.fypp:L64 (real128)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_positive_zero      ) ) ) error stop 'test.fypp:L65 (real128)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_positive_subnormal ) ) ) error stop 'test.fypp:L66 (real128)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_positive_denormal  ) ) ) error stop 'test.fypp:L67 (real128)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_positive_normal    ) ) ) error stop 'test.fypp:L68 (real128)'
        if (       is_ieee_negative_inf( ieee_value( 0.0_real128 , ieee_positive_inf       ) ) ) error stop 'test.fypp:L69 (real128)'


        print '(A)', 'OK: test_is_ieee_negative_inf'

    end subroutine test_is_ieee_negative_inf



    subroutine test_is_ieee_negative_zero()


        if (       is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_signaling_nan      ) ) ) error stop 'test.fypp:L85 (real32)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_quiet_nan          ) ) ) error stop 'test.fypp:L86 (real32)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_negative_inf       ) ) ) error stop 'test.fypp:L87 (real32)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_negative_normal    ) ) ) error stop 'test.fypp:L88 (real32)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_negative_denormal  ) ) ) error stop 'test.fypp:L89 (real32)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_negative_subnormal ) ) ) error stop 'test.fypp:L90 (real32)'
        if ( .not. is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_negative_zero      ) ) ) error stop 'test.fypp:L91 (real32)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_positive_zero      ) ) ) error stop 'test.fypp:L92 (real32)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_positive_subnormal ) ) ) error stop 'test.fypp:L93 (real32)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_positive_denormal  ) ) ) error stop 'test.fypp:L94 (real32)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_positive_normal    ) ) ) error stop 'test.fypp:L95 (real32)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real32 , ieee_positive_inf       ) ) ) error stop 'test.fypp:L96 (real32)'


        if (       is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_signaling_nan      ) ) ) error stop 'test.fypp:L85 (real64)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_quiet_nan          ) ) ) error stop 'test.fypp:L86 (real64)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_negative_inf       ) ) ) error stop 'test.fypp:L87 (real64)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_negative_normal    ) ) ) error stop 'test.fypp:L88 (real64)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_negative_denormal  ) ) ) error stop 'test.fypp:L89 (real64)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_negative_subnormal ) ) ) error stop 'test.fypp:L90 (real64)'
        if ( .not. is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_negative_zero      ) ) ) error stop 'test.fypp:L91 (real64)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_positive_zero      ) ) ) error stop 'test.fypp:L92 (real64)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_positive_subnormal ) ) ) error stop 'test.fypp:L93 (real64)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_positive_denormal  ) ) ) error stop 'test.fypp:L94 (real64)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_positive_normal    ) ) ) error stop 'test.fypp:L95 (real64)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real64 , ieee_positive_inf       ) ) ) error stop 'test.fypp:L96 (real64)'


        if (       is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_signaling_nan      ) ) ) error stop 'test.fypp:L85 (real128)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_quiet_nan          ) ) ) error stop 'test.fypp:L86 (real128)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_negative_inf       ) ) ) error stop 'test.fypp:L87 (real128)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_negative_normal    ) ) ) error stop 'test.fypp:L88 (real128)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_negative_denormal  ) ) ) error stop 'test.fypp:L89 (real128)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_negative_subnormal ) ) ) error stop 'test.fypp:L90 (real128)'
        if ( .not. is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_negative_zero      ) ) ) error stop 'test.fypp:L91 (real128)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_positive_zero      ) ) ) error stop 'test.fypp:L92 (real128)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_positive_subnormal ) ) ) error stop 'test.fypp:L93 (real128)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_positive_denormal  ) ) ) error stop 'test.fypp:L94 (real128)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_positive_normal    ) ) ) error stop 'test.fypp:L95 (real128)'
        if (       is_ieee_negative_zero( ieee_value( 0.0_real128 , ieee_positive_inf       ) ) ) error stop 'test.fypp:L96 (real128)'


        print '(A)', 'OK: test_is_ieee_negative_zero'

    end subroutine test_is_ieee_negative_zero



    subroutine test_is_ieee_positive_inf()


        if (       is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_signaling_nan      ) ) ) error stop 'test.fypp:L112 (real32)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_quiet_nan          ) ) ) error stop 'test.fypp:L113 (real32)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_negative_inf       ) ) ) error stop 'test.fypp:L114 (real32)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_negative_normal    ) ) ) error stop 'test.fypp:L115 (real32)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_negative_denormal  ) ) ) error stop 'test.fypp:L116 (real32)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_negative_subnormal ) ) ) error stop 'test.fypp:L117 (real32)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_negative_zero      ) ) ) error stop 'test.fypp:L118 (real32)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_positive_zero      ) ) ) error stop 'test.fypp:L119 (real32)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_positive_subnormal ) ) ) error stop 'test.fypp:L120 (real32)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_positive_denormal  ) ) ) error stop 'test.fypp:L121 (real32)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_positive_normal    ) ) ) error stop 'test.fypp:L122 (real32)'
        if ( .not. is_ieee_positive_inf( ieee_value( 0.0_real32 , ieee_positive_inf       ) ) ) error stop 'test.fypp:L123 (real32)'


        if (       is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_signaling_nan      ) ) ) error stop 'test.fypp:L112 (real64)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_quiet_nan          ) ) ) error stop 'test.fypp:L113 (real64)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_negative_inf       ) ) ) error stop 'test.fypp:L114 (real64)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_negative_normal    ) ) ) error stop 'test.fypp:L115 (real64)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_negative_denormal  ) ) ) error stop 'test.fypp:L116 (real64)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_negative_subnormal ) ) ) error stop 'test.fypp:L117 (real64)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_negative_zero      ) ) ) error stop 'test.fypp:L118 (real64)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_positive_zero      ) ) ) error stop 'test.fypp:L119 (real64)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_positive_subnormal ) ) ) error stop 'test.fypp:L120 (real64)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_positive_denormal  ) ) ) error stop 'test.fypp:L121 (real64)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_positive_normal    ) ) ) error stop 'test.fypp:L122 (real64)'
        if ( .not. is_ieee_positive_inf( ieee_value( 0.0_real64 , ieee_positive_inf       ) ) ) error stop 'test.fypp:L123 (real64)'


        if (       is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_signaling_nan      ) ) ) error stop 'test.fypp:L112 (real128)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_quiet_nan          ) ) ) error stop 'test.fypp:L113 (real128)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_negative_inf       ) ) ) error stop 'test.fypp:L114 (real128)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_negative_normal    ) ) ) error stop 'test.fypp:L115 (real128)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_negative_denormal  ) ) ) error stop 'test.fypp:L116 (real128)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_negative_subnormal ) ) ) error stop 'test.fypp:L117 (real128)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_negative_zero      ) ) ) error stop 'test.fypp:L118 (real128)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_positive_zero      ) ) ) error stop 'test.fypp:L119 (real128)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_positive_subnormal ) ) ) error stop 'test.fypp:L120 (real128)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_positive_denormal  ) ) ) error stop 'test.fypp:L121 (real128)'
        if (       is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_positive_normal    ) ) ) error stop 'test.fypp:L122 (real128)'
        if ( .not. is_ieee_positive_inf( ieee_value( 0.0_real128 , ieee_positive_inf       ) ) ) error stop 'test.fypp:L123 (real128)'


        print '(A)', 'OK: test_is_ieee_positive_inf'

    end subroutine test_is_ieee_positive_inf



    subroutine test_is_ieee_positive_zero()


        if (       is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_signaling_nan      ) ) ) error stop 'test.fypp:L139 (real32)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_quiet_nan          ) ) ) error stop 'test.fypp:L140 (real32)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_negative_inf       ) ) ) error stop 'test.fypp:L141 (real32)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_negative_normal    ) ) ) error stop 'test.fypp:L142 (real32)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_negative_denormal  ) ) ) error stop 'test.fypp:L143 (real32)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_negative_subnormal ) ) ) error stop 'test.fypp:L144 (real32)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_negative_zero      ) ) ) error stop 'test.fypp:L145 (real32)'
        if ( .not. is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_positive_zero      ) ) ) error stop 'test.fypp:L146 (real32)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_positive_subnormal ) ) ) error stop 'test.fypp:L147 (real32)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_positive_denormal  ) ) ) error stop 'test.fypp:L148 (real32)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_positive_normal    ) ) ) error stop 'test.fypp:L149 (real32)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real32 , ieee_positive_inf       ) ) ) error stop 'test.fypp:L150 (real32)'


        if (       is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_signaling_nan      ) ) ) error stop 'test.fypp:L139 (real64)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_quiet_nan          ) ) ) error stop 'test.fypp:L140 (real64)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_negative_inf       ) ) ) error stop 'test.fypp:L141 (real64)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_negative_normal    ) ) ) error stop 'test.fypp:L142 (real64)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_negative_denormal  ) ) ) error stop 'test.fypp:L143 (real64)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_negative_subnormal ) ) ) error stop 'test.fypp:L144 (real64)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_negative_zero      ) ) ) error stop 'test.fypp:L145 (real64)'
        if ( .not. is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_positive_zero      ) ) ) error stop 'test.fypp:L146 (real64)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_positive_subnormal ) ) ) error stop 'test.fypp:L147 (real64)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_positive_denormal  ) ) ) error stop 'test.fypp:L148 (real64)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_positive_normal    ) ) ) error stop 'test.fypp:L149 (real64)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real64 , ieee_positive_inf       ) ) ) error stop 'test.fypp:L150 (real64)'


        if (       is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_signaling_nan      ) ) ) error stop 'test.fypp:L139 (real128)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_quiet_nan          ) ) ) error stop 'test.fypp:L140 (real128)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_negative_inf       ) ) ) error stop 'test.fypp:L141 (real128)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_negative_normal    ) ) ) error stop 'test.fypp:L142 (real128)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_negative_denormal  ) ) ) error stop 'test.fypp:L143 (real128)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_negative_subnormal ) ) ) error stop 'test.fypp:L144 (real128)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_negative_zero      ) ) ) error stop 'test.fypp:L145 (real128)'
        if ( .not. is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_positive_zero      ) ) ) error stop 'test.fypp:L146 (real128)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_positive_subnormal ) ) ) error stop 'test.fypp:L147 (real128)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_positive_denormal  ) ) ) error stop 'test.fypp:L148 (real128)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_positive_normal    ) ) ) error stop 'test.fypp:L149 (real128)'
        if (       is_ieee_positive_zero( ieee_value( 0.0_real128 , ieee_positive_inf       ) ) ) error stop 'test.fypp:L150 (real128)'


        print '(A)', 'OK: test_is_ieee_positive_zero'

    end subroutine test_is_ieee_positive_zero

end program check
