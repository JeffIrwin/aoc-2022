
!===============================================================================

module m

	use utils

	implicit none

#if 1
	character(len = *), parameter :: finput = 'test-input.txt'
#else
	character(len = *), parameter :: finput = 'input.txt'
#endif

contains

!===============================================================================

subroutine part1()

	character :: s*256

	integer :: iu, io, isum

	open(file = finput, newunit = iu, status = 'old')

	isum = 0

	do
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		print *, 's = ', trim(s)

	end do

	close(iu)

	print *, 'part 1 = ', isum
	print *, ''

end subroutine part1

!===============================================================================

end module m

!===============================================================================

program main

	use m

	print *, 'Starting AOC main'
	print *, ''

	call part1()
	!call part2()

	print *, 'Ending AOC main'
	print *, ''

end program main

!===============================================================================

