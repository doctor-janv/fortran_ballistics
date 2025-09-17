module fb_MathMod

type :: Vec3
  real(8), dimension(3) :: values = (/0.0, 0.0, 0.0/)
end type

contains

!====================================================================
subroutine YPlusAX(Y, A, X)
  implicit none
  type(Vec3), intent(inout) :: Y
  real(8), intent(in) :: A
  type(Vec3), intent(in) :: X

  integer i

  do i=1,3
    Y%values(i) = Y%values(i) + A * X%values(i)
  end do
end subroutine

!====================================================================
subroutine YNorm(Y, norm_value)
  implicit none
  type(Vec3), intent(in) :: Y
  real(8), intent(out) :: norm_value

  integer i

  norm_value = 0.0
  do i=1,3
    norm_value = norm_value + Y%values(i) * Y%values(i)
  end do
  norm_value = sqrt(norm_value)

end subroutine YNorm

!====================================================================
real(8) function Interpolate(data_x, data_y, N, x)

  use fb_UtilitiesMod, only : fb_WarningColor

  implicit none

  real(8), dimension(*), intent(in) :: data_x
  real(8), dimension(*), intent(in) :: data_y
  real(8), intent(in) :: x
  integer :: N

  integer i

  Interpolate = data_y(1)
  if (x < data_x(1)) then
    return
  elseif (x >= data_x(N)) then
    Interpolate = data_y(N)
    return
  else
    do i=1,N-1
      if (x >= data_x(i) .and. x <= data_x(i+1)) then
        Interpolate = data_y(i) + (x - data_x(i)) / (data_x(i+1) - data_x(i)) * &
                      (data_y(i+1) - data_y(i))
        return
      end if
    end do
  end if

end function Interpolate

end module fb_MathMod
