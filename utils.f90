
!===============================================================================

module utils

	use iso_fortran_env

	implicit none

	character, parameter :: &
		tab = char(  9), & ! tab
		nl  = char( 10), & ! newline
		vt  = char( 11), & ! vertical tab
		cr  = char( 13)    ! carriage return


	type string
		character(len = :), allocatable :: s
	end type

contains

!===============================================================================

integer function wrap(i, n, n1)

	! Wrap i to range [1, n], or [n, n1] with optional arg

	integer :: i, n!, tmp
	integer, optional :: n1

	if (present(n1)) then

		if (n1 < n) then
			write(*,*) 'Error in wrap: range has negative width'
			stop
		end if

		!tmp = i - n + 1
		!tmp = modulo(tmp - 1, n1 - n + 1) + 1
		!tmp = tmp + n - 1
		!wrap = tmp

		wrap = modulo(i - n, n1 - n + 1) + n

	else
		wrap = modulo(i - 1, n) + 1
	end if

end function wrap

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

function readcsv(s, is) result(word)

	! Read a single comma-separated value (not an entire file)
	!
	! TODO: Make general readdlm function

	character(len = *), intent(in) :: s

	integer, intent(inout) :: is

	!********

	character(len = :), allocatable :: word

	integer :: is0

	!do while (isws(s(is: is)) .and. is <= len_trim(s))
	!	is = is + 1
	!end do

	is0 = is
	do while (s(is: is) /= ',' .and. is <= len_trim(s))
		is = is + 1
	end do

	!print *, 'is0, is = ', is0, is

	!read(s(is0: is - 1), *) readint
	word = s(is0: is - 1)

	! Skip comma
	is = is + 1

	!print *, 'word = ', word

end function readcsv

!===============================================================================

integer function readint(s, is)

	! TODO: error handling

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
		if (is > len_trim(s)) exit
	end do

	is0 = is
	do while ((isnum(s(is:is)) .or. s(is:is) == '-'))
		is = is + 1
		if (is > len_trim(s)) exit
	end do

	!print *, 'is0, is = ', is0, is

	read(s(is0: is - 1), *) readint

	!print *, 'int = ', readint

end function readint

!===============================================================================

integer function readuint(s, is)

	! TODO: error handling

	character(len = *), intent(in) :: s

	integer, intent(inout) :: is

	!********

	integer :: is0

	do while (.not. (isnum(s(is:is))) &
			.and. is <= len_trim(s))
		is = is + 1
		if (is > len_trim(s)) exit
	end do

	is0 = is
	do while (isnum(s(is:is)))
		is = is + 1
		if (is > len_trim(s)) exit
	end do

	!print *, 'is0, is = ', is0, is

	read(s(is0: is - 1), *) readuint

	!print *, 'int = ', readuint

end function readuint

!===============================================================================

integer(kind = 8) function readint8(s, is)

	! TODO: error handling

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
		if (is > len_trim(s)) exit
	end do

	is0 = is
	do while ((isnum(s(is:is)) .or. s(is:is) == '-'))
		is = is + 1
		if (is > len_trim(s)) exit
	end do

	!print *, 'is0, is = ', is0, is

	read(s(is0: is - 1), *) readint8

	!print *, 'int = ', readint8

end function readint8

!===============================================================================

integer function countlines(f) result(n)

	! Count number of lines n in a text file named f

	character(len = *), intent(in) :: f

	character :: c

	integer :: io, iu

	open(file = f, newunit = iu, status = 'old')

	n = 0

	do
		read(iu, '(a)', iostat = io) c
		if (io == iostat_end) exit
		n = n + 1
	end do

	close(iu)

end function countlines

!===============================================================================

function readline(iu) result(s)

	! Read a whole line (any length) as string s from file unit iu

	character(len = :), allocatable :: s

	integer :: io, iu, ns

	ns = countchars(iu)
	!print *, 'ns = ', ns

	allocate(character(len = ns) :: s)

	!read(iu, '(a)', iostat = io, advance = 'no') s
	read(iu, '(a)', iostat = io) s
	!print *, 's = ', s

end function readline

!===============================================================================

integer function countchars(iu) result(n)

	! Count number of characters n in the next line of a file unit iu

	character :: c

	integer :: io, iu

	n = 0

	do
		read(iu, '(a)', advance = 'no', iostat = io) c
		if (io == iostat_end) exit
		if (io == iostat_eor) exit
		n = n + 1
	end do

	backspace(iu)

end function countchars

!===============================================================================

logical function next_perm(array) result(next)

	integer, allocatable :: array(:)
	integer :: i, j, n

	n = size(array)

	i = n
	do while (i > 1)
		if (.not. array(i - 1) >= array(i)) exit
		i = i - 1
	end do

	if (i <= 1) then
		! This is the last permutation
		next = .false.
		return
	end if

	j = n
	do while (array(j) <= array(i - 1))
		j = j - 1
	end do

	array([i-1, j]) = array([j, i-1])

	j = n
	do while (i < j)
		array([i, j]) = array([j, i])
		i = i + 1
		j = j - 1
	end do

	next = .true.

end function next_perm

!===============================================================================

end module utils

!===============================================================================

