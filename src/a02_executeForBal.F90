subroutine fb_executeForBal()

  use fb_ParametersMod
  use fb_MathMod

  implicit none

  type(Vec3) :: bullet_position
  type(Vec3) :: bullet_velocity
  type(Vec3) :: gravity

  real(8) :: time = 0.0

  !============================================== Initialization
  bullet_position = Vec3((/0.0,0.0,0.0/))
  bullet_velocity = Vec3((/ &
    muzzle_velocity * cos(muzzle_theta) * cos(muzzle_varphi), &
    muzzle_velocity * cos(muzzle_theta) * sin(muzzle_varphi), &
    muzzle_velocity * sin(muzzle_theta)/))
  gravity = Vec3((/gravity_x, gravity_y, gravity_z/))

  write(*,'(A, T20, F10.3)') "target_x: ", target_x
  write(*, '(A, T20, 3E15.6)') "Muzzle velocity:", bullet_velocity

  !============================================== Solve
  write(*,'(8A15)') "time", "x", "y", "z", "vx", "vy", "vz", "v"
  call printState

  do while (bullet_position%values(1) < 1.05*target_x)

    call YPlusAX(bullet_position, solver_dt, bullet_velocity)
    call YPlusAX(bullet_velocity, solver_dt, gravity)



    time = time + solver_dt

    call printState
  end do

contains

!====================================================================
subroutine printState()
  implicit none
  real(8) v
  call YNorm(bullet_velocity, v)
  2000 format(8E15.6)
  write(*, 2000) time, bullet_position, bullet_velocity, v
end subroutine printState

!====================================================================
real(8) function computeAirMachNumber(v)
  implicit none

  real(8), intent(in) :: v
  real(8) :: c
  real(8), parameter :: gamma = 1.4
  real(8), parameter :: R = 8.31446262e3 ! m2 g s^-2 K^-1 mol^-1
  real(8), parameter :: M = 28.97        ! g mol^-1

  c = sqrt(gamma * R * air_temperature / M)

  computeAirMachNumber = c
  return
end function computeAirMachNumber

!====================================================================
real(8) function computeG1DragForceMagnitude(mach_number)
  implicit none


end function

end subroutine fb_executeForBal
