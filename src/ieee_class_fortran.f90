module ieee_class_fortran


    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128


    use, intrinsic :: ieee_arithmetic, only: operator(.eq.)
    use, intrinsic :: ieee_arithmetic, only: ieee_class
    use, intrinsic :: ieee_arithmetic, only: ieee_negative_inf
    use, intrinsic :: ieee_arithmetic, only: ieee_negative_zero
    use, intrinsic :: ieee_arithmetic, only: ieee_positive_inf
    use, intrinsic :: ieee_arithmetic, only: ieee_positive_zero
    use, intrinsic :: ieee_arithmetic, only: ieee_quiet_nan
    use, intrinsic :: ieee_arithmetic, only: ieee_signaling_nan
    use, intrinsic :: ieee_arithmetic, only: ieee_value


    implicit none


    private
    public  :: is_ieee_either_zero
    public  :: is_ieee_negative_inf
    public  :: is_ieee_negative_zero
    public  :: is_ieee_positive_inf
    public  :: is_ieee_positive_zero
    public  :: is_ieee_quiet_nan
    public  :: is_ieee_signaling_nan
    public  :: set_ieee_negative_inf
    public  :: set_ieee_negative_zero
    public  :: set_ieee_positive_inf
    public  :: set_ieee_positive_zero
    public  :: set_ieee_quiet_nan
    public  :: set_ieee_signaling_nan


    !> Checks whether `x` is either `ieee_negative_zero` or `ieee_positive_zero`
    interface is_ieee_either_zero
        module procedure :: is_ieee_either_zero_real32
        module procedure :: is_ieee_either_zero_real64
        module procedure :: is_ieee_either_zero_real128
    end interface


    !> Checks whether `x` is `ieee_negative_inf`
    interface is_ieee_negative_inf
        module procedure :: is_ieee_negative_inf_real32
        module procedure :: is_ieee_negative_inf_real64
        module procedure :: is_ieee_negative_inf_real128
    end interface


    !> Checks whether `x` is `ieee_negative_zero`
    interface is_ieee_negative_zero
        module procedure :: is_ieee_negative_zero_real32
        module procedure :: is_ieee_negative_zero_real64
        module procedure :: is_ieee_negative_zero_real128
    end interface


    !> Checks whether `x` is `ieee_positive_inf`
    interface is_ieee_positive_inf
        module procedure :: is_ieee_positive_inf_real32
        module procedure :: is_ieee_positive_inf_real64
        module procedure :: is_ieee_positive_inf_real128
    end interface


    !> Checks whether `x` is `ieee_positive_zero`
    interface is_ieee_positive_zero
        module procedure :: is_ieee_positive_zero_real32
        module procedure :: is_ieee_positive_zero_real64
        module procedure :: is_ieee_positive_zero_real128
    end interface


    !> Checks whether `x` is `ieee_quiet_nan`
    interface is_ieee_quiet_nan
        module procedure :: is_ieee_quiet_nan_real32
        module procedure :: is_ieee_quiet_nan_real64
        module procedure :: is_ieee_quiet_nan_real128
    end interface


    !> Checks whether `x` is `ieee_signaling_nan`
    interface is_ieee_signaling_nan
        module procedure :: is_ieee_signaling_nan_real32
        module procedure :: is_ieee_signaling_nan_real64
        module procedure :: is_ieee_signaling_nan_real128
    end interface


    !> Substitute `ieee_negative_inf` to `x` using `ieee_value`
    interface set_ieee_negative_inf
        module procedure :: set_ieee_negative_inf_real32
        module procedure :: set_ieee_negative_inf_real64
        module procedure :: set_ieee_negative_inf_real128
    end interface


    !> Substitute `ieee_negative_zero` to `x` using `ieee_value`
    interface set_ieee_negative_zero
        module procedure :: set_ieee_negative_zero_real32
        module procedure :: set_ieee_negative_zero_real64
        module procedure :: set_ieee_negative_zero_real128
    end interface


    !> Substitute `ieee_positive_inf` to `x` using `ieee_value`
    interface set_ieee_positive_inf
        module procedure :: set_ieee_positive_inf_real32
        module procedure :: set_ieee_positive_inf_real64
        module procedure :: set_ieee_positive_inf_real128
    end interface


    !> Substitute `ieee_positive_zero` to `x` using `ieee_value`
    interface set_ieee_positive_zero
        module procedure :: set_ieee_positive_zero_real32
        module procedure :: set_ieee_positive_zero_real64
        module procedure :: set_ieee_positive_zero_real128
    end interface


    !> Substitute `ieee_quiet_nan` to `x` using `ieee_value`
    interface set_ieee_quiet_nan
        module procedure :: set_ieee_quiet_nan_real32
        module procedure :: set_ieee_quiet_nan_real64
        module procedure :: set_ieee_quiet_nan_real128
    end interface


    !> Substitute `ieee_signaling_nan` to `x` using `ieee_value`
    interface set_ieee_signaling_nan
        module procedure :: set_ieee_signaling_nan_real32
        module procedure :: set_ieee_signaling_nan_real64
        module procedure :: set_ieee_signaling_nan_real128
    end interface


    contains


    !> Checks whether `x` is either `ieee_negative_zero` or `ieee_positive_zero`
    logical pure elemental function is_ieee_either_zero_real32(x) result(status)

        real(real32), intent(in) :: x

        associate( ieee_class_x => ieee_class(x) )
            status =      (ieee_class_x .eq. ieee_negative_zero) &!
            &        .or. (ieee_class_x .eq. ieee_positive_zero)
        end associate

    end function


    !> Checks whether `x` is either `ieee_negative_zero` or `ieee_positive_zero`
    logical pure elemental function is_ieee_either_zero_real64(x) result(status)

        real(real64), intent(in) :: x

        associate( ieee_class_x => ieee_class(x) )
            status =      (ieee_class_x .eq. ieee_negative_zero) &!
            &        .or. (ieee_class_x .eq. ieee_positive_zero)
        end associate

    end function


    !> Checks whether `x` is either `ieee_negative_zero` or `ieee_positive_zero`
    logical pure elemental function is_ieee_either_zero_real128(x) result(status)

        real(real128), intent(in) :: x

        associate( ieee_class_x => ieee_class(x) )
            status =      (ieee_class_x .eq. ieee_negative_zero) &!
            &        .or. (ieee_class_x .eq. ieee_positive_zero)
        end associate

    end function



    !> Checks whether `x` is `ieee_negative_inf`
    logical pure elemental function is_ieee_negative_inf_real32(x) result(status)

        real(real32), intent(in) :: x

        status = ieee_class(x) .eq. ieee_negative_inf

    end function


    !> Checks whether `x` is `ieee_negative_inf`
    logical pure elemental function is_ieee_negative_inf_real64(x) result(status)

        real(real64), intent(in) :: x

        status = ieee_class(x) .eq. ieee_negative_inf

    end function


    !> Checks whether `x` is `ieee_negative_inf`
    logical pure elemental function is_ieee_negative_inf_real128(x) result(status)

        real(real128), intent(in) :: x

        status = ieee_class(x) .eq. ieee_negative_inf

    end function




    !> Checks whether `x` is `ieee_negative_zero`
    logical pure elemental function is_ieee_negative_zero_real32(x) result(status)

        real(real32), intent(in) :: x

        status = ieee_class(x) .eq. ieee_negative_zero

    end function


    !> Checks whether `x` is `ieee_negative_zero`
    logical pure elemental function is_ieee_negative_zero_real64(x) result(status)

        real(real64), intent(in) :: x

        status = ieee_class(x) .eq. ieee_negative_zero

    end function


    !> Checks whether `x` is `ieee_negative_zero`
    logical pure elemental function is_ieee_negative_zero_real128(x) result(status)

        real(real128), intent(in) :: x

        status = ieee_class(x) .eq. ieee_negative_zero

    end function




    !> Checks whether `x` is `ieee_positive_inf`
    logical pure elemental function is_ieee_positive_inf_real32(x) result(status)

        real(real32), intent(in) :: x

        status = ieee_class(x) .eq. ieee_positive_inf

    end function


    !> Checks whether `x` is `ieee_positive_inf`
    logical pure elemental function is_ieee_positive_inf_real64(x) result(status)

        real(real64), intent(in) :: x

        status = ieee_class(x) .eq. ieee_positive_inf

    end function


    !> Checks whether `x` is `ieee_positive_inf`
    logical pure elemental function is_ieee_positive_inf_real128(x) result(status)

        real(real128), intent(in) :: x

        status = ieee_class(x) .eq. ieee_positive_inf

    end function




    !> Checks whether `x` is `ieee_positive_zero`
    logical pure elemental function is_ieee_positive_zero_real32(x) result(status)

        real(real32), intent(in) :: x

        status = ieee_class(x) .eq. ieee_positive_zero

    end function


    !> Checks whether `x` is `ieee_positive_zero`
    logical pure elemental function is_ieee_positive_zero_real64(x) result(status)

        real(real64), intent(in) :: x

        status = ieee_class(x) .eq. ieee_positive_zero

    end function


    !> Checks whether `x` is `ieee_positive_zero`
    logical pure elemental function is_ieee_positive_zero_real128(x) result(status)

        real(real128), intent(in) :: x

        status = ieee_class(x) .eq. ieee_positive_zero

    end function




    !> Checks whether `x` is `ieee_quiet_nan`
    logical pure elemental function is_ieee_quiet_nan_real32(x) result(status)

        real(real32), intent(in) :: x

        status = ieee_class(x) .eq. ieee_quiet_nan

    end function


    !> Checks whether `x` is `ieee_quiet_nan`
    logical pure elemental function is_ieee_quiet_nan_real64(x) result(status)

        real(real64), intent(in) :: x

        status = ieee_class(x) .eq. ieee_quiet_nan

    end function


    !> Checks whether `x` is `ieee_quiet_nan`
    logical pure elemental function is_ieee_quiet_nan_real128(x) result(status)

        real(real128), intent(in) :: x

        status = ieee_class(x) .eq. ieee_quiet_nan

    end function




    !> Checks whether `x` is `ieee_signaling_nan`
    logical pure elemental function is_ieee_signaling_nan_real32(x) result(status)

        real(real32), intent(in) :: x

        status = ieee_class(x) .eq. ieee_signaling_nan

    end function


    !> Checks whether `x` is `ieee_signaling_nan`
    logical pure elemental function is_ieee_signaling_nan_real64(x) result(status)

        real(real64), intent(in) :: x

        status = ieee_class(x) .eq. ieee_signaling_nan

    end function


    !> Checks whether `x` is `ieee_signaling_nan`
    logical pure elemental function is_ieee_signaling_nan_real128(x) result(status)

        real(real128), intent(in) :: x

        status = ieee_class(x) .eq. ieee_signaling_nan

    end function




    !> Substitute `ieee_negative_inf` to `x` using `ieee_value`
    pure elemental subroutine set_ieee_negative_inf_real32(x)

        real(real32), intent(inout) :: x

        x = ieee_value( x = x, class = ieee_negative_inf )

    end subroutine


    !> Substitute `ieee_negative_inf` to `x` using `ieee_value`
    pure elemental subroutine set_ieee_negative_inf_real64(x)

        real(real64), intent(inout) :: x

        x = ieee_value( x = x, class = ieee_negative_inf )

    end subroutine


    !> Substitute `ieee_negative_inf` to `x` using `ieee_value`
    pure elemental subroutine set_ieee_negative_inf_real128(x)

        real(real128), intent(inout) :: x

        x = ieee_value( x = x, class = ieee_negative_inf )

    end subroutine




    !> Substitute `ieee_negative_zero` to `x` using `ieee_value`
    pure elemental subroutine set_ieee_negative_zero_real32(x)

        real(real32), intent(inout) :: x

        x = ieee_value( x = x, class = ieee_negative_zero )

    end subroutine


    !> Substitute `ieee_negative_zero` to `x` using `ieee_value`
    pure elemental subroutine set_ieee_negative_zero_real64(x)

        real(real64), intent(inout) :: x

        x = ieee_value( x = x, class = ieee_negative_zero )

    end subroutine


    !> Substitute `ieee_negative_zero` to `x` using `ieee_value`
    pure elemental subroutine set_ieee_negative_zero_real128(x)

        real(real128), intent(inout) :: x

        x = ieee_value( x = x, class = ieee_negative_zero )

    end subroutine




    !> Substitute `ieee_positive_inf` to `x` using `ieee_value`
    pure elemental subroutine set_ieee_positive_inf_real32(x)

        real(real32), intent(inout) :: x

        x = ieee_value( x = x, class = ieee_positive_inf )

    end subroutine


    !> Substitute `ieee_positive_inf` to `x` using `ieee_value`
    pure elemental subroutine set_ieee_positive_inf_real64(x)

        real(real64), intent(inout) :: x

        x = ieee_value( x = x, class = ieee_positive_inf )

    end subroutine


    !> Substitute `ieee_positive_inf` to `x` using `ieee_value`
    pure elemental subroutine set_ieee_positive_inf_real128(x)

        real(real128), intent(inout) :: x

        x = ieee_value( x = x, class = ieee_positive_inf )

    end subroutine




    !> Substitute `ieee_positive_zero` to `x` using `ieee_value`
    pure elemental subroutine set_ieee_positive_zero_real32(x)

        real(real32), intent(inout) :: x

        x = ieee_value( x = x, class = ieee_positive_zero )

    end subroutine


    !> Substitute `ieee_positive_zero` to `x` using `ieee_value`
    pure elemental subroutine set_ieee_positive_zero_real64(x)

        real(real64), intent(inout) :: x

        x = ieee_value( x = x, class = ieee_positive_zero )

    end subroutine


    !> Substitute `ieee_positive_zero` to `x` using `ieee_value`
    pure elemental subroutine set_ieee_positive_zero_real128(x)

        real(real128), intent(inout) :: x

        x = ieee_value( x = x, class = ieee_positive_zero )

    end subroutine




    !> Substitute `ieee_quiet_nan` to `x` using `ieee_value`
    pure elemental subroutine set_ieee_quiet_nan_real32(x)

        real(real32), intent(inout) :: x

        x = ieee_value( x = x, class = ieee_quiet_nan )

    end subroutine


    !> Substitute `ieee_quiet_nan` to `x` using `ieee_value`
    pure elemental subroutine set_ieee_quiet_nan_real64(x)

        real(real64), intent(inout) :: x

        x = ieee_value( x = x, class = ieee_quiet_nan )

    end subroutine


    !> Substitute `ieee_quiet_nan` to `x` using `ieee_value`
    pure elemental subroutine set_ieee_quiet_nan_real128(x)

        real(real128), intent(inout) :: x

        x = ieee_value( x = x, class = ieee_quiet_nan )

    end subroutine




    !> Substitute `ieee_signaling_nan` to `x` using `ieee_value`
    pure elemental subroutine set_ieee_signaling_nan_real32(x)

        real(real32), intent(inout) :: x

        x = ieee_value( x = x, class = ieee_signaling_nan )

    end subroutine


    !> Substitute `ieee_signaling_nan` to `x` using `ieee_value`
    pure elemental subroutine set_ieee_signaling_nan_real64(x)

        real(real64), intent(inout) :: x

        x = ieee_value( x = x, class = ieee_signaling_nan )

    end subroutine


    !> Substitute `ieee_signaling_nan` to `x` using `ieee_value`
    pure elemental subroutine set_ieee_signaling_nan_real128(x)

        real(real128), intent(inout) :: x

        x = ieee_value( x = x, class = ieee_signaling_nan )

    end subroutine


end module ieee_class_fortran
