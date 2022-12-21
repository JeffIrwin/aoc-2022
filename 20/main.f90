
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

subroutine part2()

	integer(kind = 8), parameter :: key = 811589153, nround = 10

	integer(kind = 8) :: i, i0, j, j1, j2, iu, isum, n, dir, npos, i00(1), &
			ifin, iround

	integer(kind = 8), allocatable :: vect(:)
	integer(kind = 8), allocatable :: perm(:), invperm(:)

	isum = 0

	n = countlines(finput)
	allocate(vect(n), perm(n), invperm(n))

	open(file = finput, newunit = iu, status = 'old')

	do i = 1, n
		read(iu, *) vect(i)
	end do

	close(iu)

	! Permutation vector and inverse.  Initially identity
	perm = [(i, i = 1, n)]
	invperm = perm

	vect = vect * key

	!print *, 'vect  = ', vect
	!print *, 'perm = ', perm

	do iround = 1, nround

	!print *, 'round = ', iround

	do i = 1, n

		! Initial index of the number to move
		i0 = invperm(i)

		! Number of positions to move (signed)
		npos = vect(i0) !abs(vect(i0))

		!print *, 'i0, npos = ', i0, npos

		! Final position of number
		ifin = i0 + npos

		!do while (ifin <= 1)
		!	ifin = ifin + n - 1
		!end do
		!do while (ifin >= n)
		!	ifin = ifin - n + 1
		!end do

		ifin = modulo(ifin - 1, n - 1) + 1
		if (ifin == 1) ifin = n

		!print *, 'i0, ifin = ', i0, ifin

		if (i0 < ifin) then
			vect = [vect(1:i0-1), vect(i0+1:ifin), vect(i0), vect(ifin+1: n)]
			perm = [perm(1:i0-1), perm(i0+1:ifin), perm(i0), perm(ifin+1: n)]
		else if (ifin < i0) then
			vect = [vect(1:ifin-1), vect(i0), vect(ifin:i0-1), vect(i0+1: n)]
			perm = [perm(1:ifin-1), perm(i0), perm(ifin:i0-1), perm(i0+1: n)]
		end if

		!if (ifin == 1) then
		!	vect = cshift(vect, 1)
		!	perm = cshift(perm, 1)
		!end if

		! Invert the permutation
		do j = 1, n
			invperm(perm(j)) = j
		end do

		!print *, 'vect  = ', vect
		!print *, 'perm = ', perm
		!print *, 'invp = ', invperm
		!print *, ''

	end do
	end do

	!print *, 'vect  = ', vect

	! Find the index of 0
	i00 = minloc(abs(vect - 0))
	i0 = i00(1)
	do i = i0 + 1000, i0 + 3000, 1000
		!print *, 'adding ', vect(modulo(i-1, n) + 1)
		isum = isum + vect(modulo(i-1, n) + 1)
	end do

	write(*,*) 'part2 = ', isum
	write(*,*) ''

end subroutine part2

!===============================================================================

subroutine part1()

	integer :: i, i0, ifin, j, j1, j2, iu, isum, n, dir, npos, i00(1)
	integer, allocatable :: vect(:), perm(:), invperm(:)

	isum = 0

	n = countlines(finput)
	allocate(vect(n), perm(n), invperm(n))

	open(file = finput, newunit = iu, status = 'old')

	do i = 1, n
		read(iu, *) vect(i)
	end do

	close(iu)

	! Permutation vector and inverse.  Initially identity
	perm = [(i, i = 1, n)]
	invperm = perm

	!print *, 'vect  = ', vect
	!print *, 'perm = ', perm

	do i = 1, n

		! Initial index of the number to move
		i0 = invperm(i)

		! Number of positions to move (signed)
		npos = vect(i0) !abs(vect(i0))

		!print *, 'i0, npos = ', i0, npos

		! Final position of number
		ifin = i0 + npos

		! TODO: is this just ifin modulo (n-1)?
		do while (ifin <= 1)
			ifin = ifin + n - 1
		end do
		do while (ifin >= n)
			ifin = ifin - n + 1
		end do

		!print *, 'i0, ifin = ', i0, ifin

		if (i0 < ifin) then
			vect = [vect(1:i0-1), vect(i0+1:ifin), vect(i0), vect(ifin+1: n)]
			perm = [perm(1:i0-1), perm(i0+1:ifin), perm(i0), perm(ifin+1: n)]
		else if (ifin < i0) then
			vect = [vect(1:ifin-1), vect(i0), vect(ifin:i0-1), vect(i0+1: n)]
			perm = [perm(1:ifin-1), perm(i0), perm(ifin:i0-1), perm(i0+1: n)]
		end if

		!if (ifin == 1) then
		!	vect = cshift(vect, 1)
		!	perm = cshift(perm, 1)
		!end if

		! Invert the permutation
		do j = 1, n
			invperm(perm(j)) = j
		end do

		!print *, 'vect  = ', vect
		!print *, 'perm = ', perm
		!print *, 'invp = ', invperm
		!print *, ''

	end do

	!print *, 'vect  = ', vect

	! Find the index of 0
	i00 = minloc(abs(vect - 0))
	i0 = i00(1)
	do i = i0 + 1000, i0 + 3000, 1000
		!print *, 'adding ', vect(modulo(i-1, n) + 1)
		isum = isum + vect(modulo(i-1, n) + 1)
	end do

	write(*,*) 'part1 = ', isum
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
	call part2()

	write(*,*) 'Ending AOC main'
	write(*,*) ''

end program main

!===============================================================================

