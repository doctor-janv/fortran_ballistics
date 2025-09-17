subroutine fb_initializeForBal()

  use fb_UtilitiesMod, only : fb_quit, fb_warn

  implicit none

  integer :: num_args

  num_args = command_argument_count()

  if (num_args == 0) then
    call fb_warn("No commandline arguments supplied")
  end if ! (num_args)

end subroutine fb_initializeForBal
