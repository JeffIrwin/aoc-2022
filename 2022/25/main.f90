
!===============================================================================

module m

	use utils

	implicit none

#if 0
	character(len = *), parameter :: finput = 'test-input.txt'
#else
	character(len = *), parameter :: finput = 'input.txt'
#endif

	! "=" is "-2", "-" is "-1", other characters have the usual interpretation
	character(len = *), parameter :: snafu_digs = '=-012'

	integer, parameter :: ik = 1

	integer(kind = ik), parameter :: snafu_base = 5, &
			snafu_offset = snafu_base / 2 + 1

	type snafu

		! Base-5 number with digits d in {-2, -1, 0, 1, 2} and number of digits
		! ndig.  Endianness is d(1) is ones place, d(2) is fives place, etc.

		integer :: ndig
		integer(kind = ik), allocatable :: d(:)

	end type snafu

	interface operator (+)
		module procedure snafu_add
	end interface

contains

!===============================================================================

function write_snafu(x) result(str)

	! Format a snafu number to str

	character(len = :), allocatable :: str

	type(snafu), intent(in) :: x

	integer :: i, j, d

	str = repeat(' ', x%ndig)

	j = 0
	do i = x%ndig, 1, -1
		j = j + 1
		d = x%d(j) + snafu_offset
		str(i:i) = snafu_digs(d:d)
	end do

end function write_snafu

!===============================================================================

function read_snafu(str) result(ans)

	! Read a snafu number from string str

	character :: c
	character(len = *) :: str
	character(len = :), allocatable :: s

	integer :: i, j
	integer(kind = ik) :: d

	type(snafu) :: ans

	s = trim(adjustl(str))
	ans%ndig = len(s)

	allocate(ans%d( ans%ndig ))

	! Loop from end for proper endianness
	j = 0
	do i = len(s), 1, -1
		j = j + 1

		c = s(i:i)

		d = index(snafu_digs, c, .false., ik)
		if (d <= 0) then
			write(*,*) 'Error in read_snafu: invalid character "', c, '"'
			stop
		end if

		ans%d(j) = d - snafu_offset

	end do

end function read_snafu

!===============================================================================

function snafu_add(a, b) result(c)

	! There's no need to convert SNAFU numbers to decimal or vice versa.  We can
	! just add up the SNAFU numbers in base-5 directly

	type(snafu), intent(in) :: a, b
	type(snafu) :: c

	integer :: i, nd
	integer(kind = ik) :: da, db, dc, carry

	!print *, 'starting snafu_add()'
	!print *, 'a digits = ', a%d( a%ndig: 1: -1 )  ! forward
	!print *, 'b digits = ', b%d( b%ndig: 1: -1 )  ! forward

	! Carrying can make an extra digit.  Trim at end if needed
	nd = max(a%ndig, b%ndig) + 1
	c%ndig = nd

	allocate(c%d(nd))
	c%d = 0

	carry = 0
	do i = 1, nd

		da = 0
		db = 0
		if (i <= a%ndig) da = a%d(i)
		if (i <= b%ndig) db = b%d(i)

		dc = da + db + carry

		carry = 0
		if (dc > snafu_offset - 1) then
			carry = 1
			dc = dc - snafu_base
		else if (dc < -(snafu_offset - 1)) then
			carry = -1
			dc = dc + snafu_base
		end if

		c%d(i) = dc

	end do

	! Trim unused carry zeros
	do while (c%d(nd) == 0)
		nd = nd - 1
	end do
	c%ndig = nd
	c%d = c%d(1: nd)

end function snafu_add

!===============================================================================

subroutine part1()

	character :: s*256

	integer :: iu, io, i

	type(snafu) :: x, ssum

	open(file = finput, newunit = iu, status = 'old')

	i = 0
	do
		i = i + 1
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		x = read_snafu(trim(s))

		!print *, 's = ', trim(s)
		!print *, 'x = ', write_snafu(x)

		if (i == 1) then
			! I'm lazy and don't want to initialize sum to literal snafu zero outside
			! of loop
			ssum = x
		else
			ssum = ssum + x
		end if

	end do

	close(iu)

	!print *, 'ndig = ', ssum%ndig
	!print *, 'ssum digits = ', ssum%d( ssum%ndig: 1: -1 )  ! forward

	write(*,*) 'part1 = ', write_snafu(ssum)
	write(*,*) ''

end subroutine part1

!===============================================================================

end module m

!===============================================================================

program main

	use m

	write(*,*) 'Starting AOC main'
	write(*,*) 'Input file = ', finput
	write(*,*) ''

	call part1()

	write(*,*) 'Ending AOC main'
	write(*,*) ''

end program main

!===============================================================================

