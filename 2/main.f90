
!===============================================================================

module m

	use iso_fortran_env

	implicit none

	character(len = :), allocatable :: finput

contains

!===============================================================================

subroutine part1

	character :: s*256, opp, res

	logical :: eof

	integer :: iu, io, isum

	!print *, 'finput = ', finput
	!print *, ''

	open(file = finput, newunit = iu, status = 'old')

	isum = 0

	eof = .false.

	do while (.not. eof)

		read(iu, '(a)', iostat = io) s
		eof = io == iostat_end
		if (eof) exit

		!print *, 's = ', trim(s)

		! Opponent and response
		opp = s(1:1)
		res = s(3:3)

		if (res == 'X') then

			! Shape score
			isum = isum + 1

			! Outcome score
			if (opp == 'A') then
				isum = isum + 3
			else if (opp == 'B') then
				isum = isum + 0
			else if (opp == 'C') then
				isum = isum + 6
			end if

		else if (res == 'Y') then
			isum = isum + 2

			if (opp == 'B') then
				isum = isum + 3
			else if (opp == 'C') then
				isum = isum + 0
			else if (opp == 'A') then
				isum = isum + 6
			end if

		else if (res == 'Z') then
			isum = isum + 3

			if (opp == 'C') then
				isum = isum + 3
			else if (opp == 'A') then
				isum = isum + 0
			else if (opp == 'B') then
				isum = isum + 6
			end if

		end if

	end do

	print *, 'part 1 = ', isum
	print *, ''

	close(iu)

end subroutine part1

!===============================================================================

subroutine part2

	character :: s*256, opp, res, outcome

	logical :: eof

	integer :: iu, io, isum

	!print *, 'finput = ', finput
	!print *, ''

	open(file = finput, newunit = iu, status = 'old')

	isum = 0

	eof = .false.

	do while (.not. eof)

		read(iu, '(a)', iostat = io) s
		eof = io == iostat_end
		if (eof) exit

		!print *, 's = ', trim(s)

		! Opponent and outcome (instead of response)
		opp = s(1:1)
		outcome = s(3:3)

		! Determine required response to yield outcome
		if (opp == 'A') then
			if (outcome == 'X') res = 'Z'
			if (outcome == 'Y') res = 'X'
			if (outcome == 'Z') res = 'Y'
		else if (opp == 'B') then
			if (outcome == 'X') res = 'X'
			if (outcome == 'Y') res = 'Y'
			if (outcome == 'Z') res = 'Z'
		else if (opp == 'C') then
			if (outcome == 'X') res = 'Y'
			if (outcome == 'Y') res = 'Z'
			if (outcome == 'Z') res = 'X'
		end if

		if (res == 'X') then

			! Shape score
			isum = isum + 1

			! Outcome score
			if (opp == 'A') then
				isum = isum + 3
			else if (opp == 'B') then
				isum = isum + 0
			else if (opp == 'C') then
				isum = isum + 6
			end if

		else if (res == 'Y') then
			isum = isum + 2

			if (opp == 'B') then
				isum = isum + 3
			else if (opp == 'C') then
				isum = isum + 0
			else if (opp == 'A') then
				isum = isum + 6
			end if

		else if (res == 'Z') then
			isum = isum + 3

			if (opp == 'C') then
				isum = isum + 3
			else if (opp == 'A') then
				isum = isum + 0
			else if (opp == 'B') then
				isum = isum + 6
			end if

		end if

	end do

	print *, 'part 2 = ', isum
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

