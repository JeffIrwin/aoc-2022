
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

	character :: s*256

	integer :: i, j, iu, io, isum, n, is
	integer, allocatable :: vec(:)

	n = countlines(finput)
	!print *, 'n = ', n

	allocate(vec(n))

	open(file = finput, newunit = iu, status = 'old')

	isum = 0

	i = 0
	do
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		!print *, 's = ', trim(s)

		i = i + 1
		is = 1
		vec(i) = readint(s, is)

	end do

	close(iu)

	!print *, 'vec = ', vec

	do i = 2, n
	do j = 1, i - 1
		if (vec(i) + vec(j) == 2020) then
			isum = vec(i) * vec(j)
		end if
	end do
	end do

	write(*,*) 'part1 = ', isum
	write(*,*) ''

end subroutine part1

!===============================================================================

subroutine part2()

	character :: s*256

	integer :: i, j, k, iu, io, isum, n, is
	integer, allocatable :: vec(:)

	n = countlines(finput)
	!print *, 'n = ', n

	allocate(vec(n))

	open(file = finput, newunit = iu, status = 'old')

	isum = 0

	i = 0
	do
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		!print *, 's = ', trim(s)

		i = i + 1
		is = 1
		vec(i) = readint(s, is)

	end do

	close(iu)

	!print *, 'vec = ', vec

	do i = 3, n
	do j = 2, i - 1
	do k = 1, j - 1
		if (sum(vec([i,j,k])) == 2020) then
			isum = product(vec([i,j,k]))
		end if
	end do
	end do
	end do

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

