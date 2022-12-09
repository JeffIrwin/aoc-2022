
!===============================================================================

module m

	use iso_fortran_env

	implicit none

	character(len = :), allocatable :: finput

contains

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

logical function contain(al, ah, bl, bh)

	! Does the range [al, ah] contain the range [bl, bh]?

	integer, intent(in) :: al, ah, bl, bh

	contain = al <= bl .and. ah >= bh

end function contain

!===============================================================================

subroutine part1()

	character :: s*256

	integer :: iu, io, isum, al, ah, bl, bh, is

	open(file = finput, newunit = iu, status = 'old')

	isum = 0

	do
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		!print *, 's = ', trim(s)

		is = 1

		al = readint(s, is)
		ah = readint(s, is)
		bl = readint(s, is)
		bh = readint(s, is)

		!print *, 'ranges = ', al, '-', ah, ', ', bl, '-', bh
		!print *, 'is = ', is
		!print *, 'ranges = ', al

		if (contain(al, ah, bl, bh)) then
			isum = isum + 1
		else if (contain(bl, bh, al, ah)) then
			isum = isum + 1
		end if

	end do

	close(iu)

	print *, 'part 1 = ', isum
	print *, ''

end subroutine part1

!===============================================================================

logical function overlap(al, ah, bl, bh)

	! Does the range [al, ah] overlap the range [bl, bh]?

	integer, intent(in) :: al, ah, bl, bh

	overlap = .not. (ah < bl .or. bh < al)

end function overlap

!===============================================================================

subroutine part2()

	character :: s*256

	integer :: iu, io, isum, al, ah, bl, bh, is

	open(file = finput, newunit = iu, status = 'old')

	isum = 0

	do
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		!print *, 's = ', trim(s)

		is = 1

		al = readint(s, is)
		ah = readint(s, is)
		bl = readint(s, is)
		bh = readint(s, is)

		!print *, 'ranges = ', al, '-', ah, ', ', bl, '-', bh
		!print *, 'is = ', is
		!print *, 'ranges = ', al

		if (overlap(al, ah, bl, bh)) isum = isum + 1

	end do

	close(iu)

	print *, 'part 2 = ', isum
	print *, ''

end subroutine part2

!===============================================================================

end module m

!===============================================================================

program main

	use m

	print *, 'Starting AOC main'
	print *, ''

	finput = 'input.txt'
	!finput = 'test-input.txt'

	call part1()
	call part2()

	print *, 'Ending AOC main'
	print *, ''

end program main

!===============================================================================

