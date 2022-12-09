
!===============================================================================

module m

	use iso_fortran_env

	implicit none

	character(len = :), allocatable :: finput

contains

!===============================================================================

subroutine part1

	character :: s*256

	logical :: eof

	integer :: i, iu, io, isum, imax

	!print *, 'finput = ', finput
	!print *, ''

	open(file = finput, newunit = iu, status = 'old')

	isum = 0
	imax = 0

	eof = .false.

	do while (.not. eof)

		read(iu, '(a)', iostat = io) s
		eof = io == iostat_end
		if (eof) exit

		!print *, 's = ', trim(s)

		if (len_trim(s) == 0) then
			imax = max(imax, isum)
			isum = 0
		else
			read(s, *) i
			isum = isum + i
		end if

	end do

	print *, 'part 1 = ', imax
	print *, ''

	close(iu)

end subroutine part1

!===============================================================================

subroutine part2

	character :: s*256

	logical :: eof

	integer :: i, iu, io, isum, imax1, imax2, imax3

	!print *, 'finput = ', finput
	!print *, ''

	open(file = finput, newunit = iu, status = 'old')

	isum = 0
	imax1 = 0
	imax2 = 0
	imax3 = 0

	eof = .false.

	do while (.not. eof)

		read(iu, '(a)', iostat = io) s
		eof = io == iostat_end
		if (eof) exit

		!print *, 's = ', trim(s)

		if (len_trim(s) == 0) then

		! Lazy maxk() implementation.  Should implement growable arrays and
		! a sorting algorithm.

			!imax = max(imax, isum)

			! imax1 >= imax2 >= imax3
			if (isum > imax1) then
				imax3 = imax2
				imax2 = imax1
				imax1 = isum
			else if (isum > imax2) then
				imax3 = imax2
				imax2 = isum
			else if (isum > imax3) then
				imax3 = isum
			end if

			isum = 0

		else
			read(s, *) i
			isum = isum + i
		end if

	end do

	!print *, 'imax1 = ', imax1
	!print *, 'imax2 = ', imax2
	!print *, 'imax3 = ', imax3
	!print *, ''

	print *, 'part 2 = ', imax1 + imax2 + imax3
	print *, ''

	close(iu)

end subroutine part2

!===============================================================================

end module m

!===============================================================================

program main

	use m

	print *, ''
	print *, 'Starting AOC main'
	print *, ''

	finput = 'test-input.txt'
	finput = 'input.txt'

	call part1()
	call part2()

	print *, 'Ending AOC main'
	print *, ''

end program main

!===============================================================================

