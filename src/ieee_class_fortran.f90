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


    implicit none


    private
    public  :: is_ieee_negative_inf
    public  :: is_ieee_negative_zero
    public  :: is_ieee_positive_inf
    public  :: is_ieee_positive_zero


    interface is_ieee_negative_inf
        module procedure :: is_ieee_negative_inf_real32
        module procedure :: is_ieee_negative_inf_real64
        module procedure :: is_ieee_negative_inf_real128
    end interface


    interface is_ieee_negative_zero
        module procedure :: is_ieee_negative_zero_real32
        module procedure :: is_ieee_negative_zero_real64
        module procedure :: is_ieee_negative_zero_real128
    end interface


    interface is_ieee_positive_inf
        module procedure :: is_ieee_positive_inf_real32
        module procedure :: is_ieee_positive_inf_real64
        module procedure :: is_ieee_positive_inf_real128
    end interface


    interface is_ieee_positive_zero
        module procedure :: is_ieee_positive_zero_real32
        module procedure :: is_ieee_positive_zero_real64
        module procedure :: is_ieee_positive_zero_real128
    end interface


    contains


    logical pure elemental function is_ieee_negative_inf_real32(x) result(status)

        real(real32), intent(in) :: x

        status = ieee_class(x) .eq. ieee_negative_inf

    end function


    logical pure elemental function is_ieee_negative_inf_real64(x) result(status)

        real(real64), intent(in) :: x

        status = ieee_class(x) .eq. ieee_negative_inf

    end function


    logical pure elemental function is_ieee_negative_inf_real128(x) result(status)

        real(real128), intent(in) :: x

        status = ieee_class(x) .eq. ieee_negative_inf

    end function




    logical pure elemental function is_ieee_negative_zero_real32(x) result(status)

        real(real32), intent(in) :: x

        status = ieee_class(x) .eq. ieee_negative_zero

    end function


    logical pure elemental function is_ieee_negative_zero_real64(x) result(status)

        real(real64), intent(in) :: x

        status = ieee_class(x) .eq. ieee_negative_zero

    end function


    logical pure elemental function is_ieee_negative_zero_real128(x) result(status)

        real(real128), intent(in) :: x

        status = ieee_class(x) .eq. ieee_negative_zero

    end function




    logical pure elemental function is_ieee_positive_inf_real32(x) result(status)

        real(real32), intent(in) :: x

        status = ieee_class(x) .eq. ieee_positive_inf

    end function


    logical pure elemental function is_ieee_positive_inf_real64(x) result(status)

        real(real64), intent(in) :: x

        status = ieee_class(x) .eq. ieee_positive_inf

    end function


    logical pure elemental function is_ieee_positive_inf_real128(x) result(status)

        real(real128), intent(in) :: x

        status = ieee_class(x) .eq. ieee_positive_inf

    end function




    logical pure elemental function is_ieee_positive_zero_real32(x) result(status)

        real(real32), intent(in) :: x

        status = ieee_class(x) .eq. ieee_positive_zero

    end function


    logical pure elemental function is_ieee_positive_zero_real64(x) result(status)

        real(real64), intent(in) :: x

        status = ieee_class(x) .eq. ieee_positive_zero

    end function


    logical pure elemental function is_ieee_positive_zero_real128(x) result(status)

        real(real128), intent(in) :: x

        status = ieee_class(x) .eq. ieee_positive_zero

    end function


end module ieee_class_fortran
