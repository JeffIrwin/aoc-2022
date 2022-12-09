
!===============================================================================

module utils

	use iso_fortran_env

	implicit none

contains

!===============================================================================

logical function isalpha(c)

	character, intent(in) :: c

	isalpha = ('a' <= c .and. c <= 'z') .or. ('A' <= c .and. c <= 'Z')

end function isalpha

!===============================================================================

logical function isnum(c)

	character, intent(in) :: c

	isnum = '0' <= c .and. c <= '9'

end function isnum

!===============================================================================

integer function readint(s, is)

	character(len = *), intent(in) :: s

	integer, intent(inout) :: is

	!********

	integer :: is0

	do while (.not. isnum(s(is: is)) .and. is <= len_trim(s))
		is = is + 1
	end do

	is0 = is
	do while (isnum(s(is: is)) .and. is <= len_trim(s))
		is = is + 1
	end do

	!print *, 'is0, is = ', is0, is

	read(s(is0: is - 1), *) readint

	!print *, 'int = ', readint

end function readint

!===============================================================================

end module utils

