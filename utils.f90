
!===============================================================================

module utils

	use iso_fortran_env

	implicit none

	character, parameter :: &
		tab = char(  9), & ! tab
		nl  = char( 10), & ! newline
		vt  = char( 11), & ! vertical tab
		cr  = char( 13)    ! carriage return

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

logical function isws(c)

	! Is character c whitespace?

	character, intent(in) :: c

	isws = any(c == [tab, nl, vt, cr, ' '])

end function isws

!===============================================================================

function readword(s, is) result(word)

	! Read words (consisting of any non-whitespace characters) delimited by whitespace

	character(len = *), intent(in) :: s

	integer, intent(inout) :: is

	!********

	character(len = :), allocatable :: word

	integer :: is0

	do while (isws(s(is: is)) .and. is <= len_trim(s))
		is = is + 1
	end do

	is0 = is
	do while (.not. isws(s(is: is)) .and. is <= len_trim(s))
		is = is + 1
	end do

	!print *, 'is0, is = ', is0, is

	!read(s(is0: is - 1), *) readint
	word = s(is0: is - 1)

	!print *, 'word = ', word

end function readword

!===============================================================================

integer function readint(s, is)

	character(len = *), intent(in) :: s

	integer, intent(inout) :: is

	!********

	integer :: is0

	! Negative numbers are also allowed.  Assume they are well-formed, e.g. this
	! will not parse something like -2-4 (which I think might actually be
	! present in one of the earlier days?)
	do while (.not. (isnum(s(is:is)) .or. s(is:is) == '-') &
			.and. is <= len_trim(s))
		is = is + 1
	end do

	is0 = is
	do while ((isnum(s(is:is)) .or. s(is:is) == '-') .and. is <= len_trim(s))
		is = is + 1
	end do

	!print *, 'is0, is = ', is0, is

	read(s(is0: is - 1), *) readint

	!print *, 'int = ', readint

end function readint

!===============================================================================

end module utils

!===============================================================================

