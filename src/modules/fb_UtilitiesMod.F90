module fb_UtilitiesMod

character(len=*), parameter :: fb_ErrorColor = achar(27)//"[31mERROR: "
character(len=*), parameter :: fb_WarningColor = achar(27)//"[33mERROR: "
character(len=*), parameter :: fb_ResetColor = achar(27)//"[0m"

contains

!====================================================================
subroutine fb_quit(exit_code, message)

  implicit none

  integer, intent(in) :: exit_code
  character(len=*), intent(in) :: message


  write(*,'(A,A,A)') fb_WarningColor, message, fb_ResetColor

  call exit(exit_code)

end subroutine fb_quit

!====================================================================
subroutine fb_warn(message)

  implicit none

  character(len=*), intent(in) :: message

  write(*,'(A,A,A)') fb_WarningColor, message, fb_ResetColor

end subroutine fb_warn

end module fb_UtilitiesMod
