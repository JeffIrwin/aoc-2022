
!===============================================================================

module m

	use utils

	implicit none

#if 0
	character(len = *), parameter :: finput = 'test-input.txt'
#else
	character(len = *), parameter :: finput = 'input.txt'
#endif

contains

!===============================================================================

subroutine part1()

	character :: s*256, letter*1
	character(len = :), allocatable :: password

	integer :: i, iu, io, isum, nmin, nmax, is, n

	open(file = finput, newunit = iu, status = 'old')

	isum = 0

	do
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		print *, 's = ', trim(s)

		is = 1
		nmin = readuint(s, is)
		print *, 'nmin = ', nmin

		! Skip delimiter `-`
		is = is + 1
		nmax = readuint(s, is)
		print *, 'nmax = ', nmax

		! Skip delimiter ` `
		is = is + 1
		letter = s(is: is)
		print *, 'letter = ', letter

		! Skip delimiters `: `
		is = is + 2
		password = readword(s, is)
		print *, 'password = ', password

		! Count occurences of letter in password
		n = 0
		do i = 1, len(password)
			if (password(i:i) == letter) n = n + 1
		end do
		print *, 'n = ', n

		if (nmin <= n .and. n <= nmax) isum = isum + 1

	end do

	close(iu)

	write(*,*) 'part1 = ', isum
	write(*,*) ''

end subroutine part1

!===============================================================================

subroutine part2()

	character :: s*256, letter*1
	character(len = :), allocatable :: password

	integer :: i, iu, io, isum, nmin, nmax, is, n

	open(file = finput, newunit = iu, status = 'old')

	isum = 0

	do
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		print *, 's = ', trim(s)

		is = 1
		nmin = readuint(s, is)
		print *, 'nmin = ', nmin

		! Skip delimiter `-`
		is = is + 1
		nmax = readuint(s, is)
		print *, 'nmax = ', nmax

		! Skip delimiter ` `
		is = is + 1
		letter = s(is: is)
		print *, 'letter = ', letter

		! Skip delimiters `: `
		is = is + 2
		password = readword(s, is)
		print *, 'password = ', password

		! Different password policy from part 1
		if (password(nmin:nmin) == letter .neqv. &
		    password(nmax:nmax) == letter) isum = isum + 1

		!! Count occurences of letter in password
		!n = 0
		!do i = 1, len(password)
		!	if (password(i:i) == letter) n = n + 1
		!end do
		!print *, 'n = ', n

		!if (nmin <= n .and. n <= nmax) isum = isum + 1

	end do

	close(iu)

	write(*,*) 'part2 = ', isum
	write(*,*) ''

end subroutine part2

!===============================================================================

end module m

!===============================================================================

program main

	use m

	write(*,*) 'Starting AOC main'
	write(*,*) 'Input file = ', finput
	write(*,*) ''

	call part1()
	call part2()

	write(*,*) 'Ending AOC main'
	write(*,*) ''

end program main

!===============================================================================

