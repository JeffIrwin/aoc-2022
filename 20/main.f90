
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

subroutine part2()

	integer, parameter :: key = 811589153

	integer :: i, i0, j, j1, j2, iu, isum, n, dir, npos, shift, i00(1)
	integer(kind = 8), allocatable :: vec(:)
	integer, allocatable :: perm(:), invperm(:)

	isum = 0

	n = countlines(finput)
	allocate(vec(n), perm(n), invperm(n))

	open(file = finput, newunit = iu, status = 'old')

	do i = 1, n
		read(iu, *) vec(i)
	end do

	close(iu)

	! Permutation vector and inverse.  Initially identity
	perm = [(i, i = 1, n)]
	invperm = perm

	vec = vec * key

	print *, 'vec  = ', vec
	print *, 'perm = ', perm

	do i = 1, n

		! Initial index of the number to move
		!i0 = perm(i)
		i0 = invperm(i)

		! Number of positions to move (signed)
		npos = vec(i0) !abs(vec(i0))

		! Direction to move (+1 or -1)
		dir = sign(1, npos)

		!! Convert negative motions to equivalent positive motions
		!npos = modulo(npos, n)
		!dir = 1

		print *, 'i0, dir, npos = ', i0, dir, npos

		shift = 0

		! Apply the movement to vec and perm
		do j = i0 + dir, i0 + npos, dir

			! Indices of 2 numbers to be swapped

			!j1 = abs(mod(j-1    , n)) + 1
			!j2 = abs(mod(j-dir-1, n)) + 1

			j1 = modulo(j-1    , n) + 1
			j2 = modulo(j-dir-1, n) + 1

			!print *, 'j, j1, j2 = ', j, j1, j2

			vec ([j1, j2]) = vec ([j2, j1])
			perm([j1, j2]) = perm([j2, j1])

			! If we swap elements on the ends, then we have to shift the whole
			! array by one to correct.  But, don't do this until we're done
			! bubbling i0 to it's mixed position
			if      (j1 == 1 .and. j2 == n) then
				shift = shift - 1
				!vec = cshift(vec, -1)
			else if (j2 == 1 .and. j1 == n) then
				shift = shift + 1
				!vec = cshift(vec,  1)
			end if

		end do

		! This case also counts as a circular shift
		if (i0 + npos == 1 .and. dir < 0) shift = shift + 1

		vec  = cshift(vec , shift)
		perm = cshift(perm, shift)

		! Invert the permutation
		do j = 1, n
			!invperm(perm(j)) = j
			invperm(perm(j)) = j
		end do

		!print *, 'vec  = ', vec
		!print *, 'perm = ', perm
		!print *, 'invp = ', invperm
		!print *, ''

	end do

	print *, 'vec  = ', vec

	! Find the index of 0
	i00 = minloc(abs(vec - 0))
	i0 = i00(1)
	do i = i0 + 1000, i0 + 3000, 1000
		print *, 'adding ', vec(modulo(i-1, n) + 1)
		isum = isum + vec(modulo(i-1, n) + 1)
	end do

	write(*,*) 'part2 = ', isum
	write(*,*) ''

end subroutine part2

!===============================================================================

subroutine part1()

	integer :: i, i0, ifin, j, j1, j2, iu, isum, n, dir, npos, shift, i00(1)
	integer, allocatable :: vec(:), perm(:), invperm(:)

	isum = 0

	n = countlines(finput)
	allocate(vec(n), perm(n), invperm(n))

	open(file = finput, newunit = iu, status = 'old')

	do i = 1, n
		read(iu, *) vec(i)
	end do

	close(iu)

	! Permutation vector and inverse.  Initially identity
	perm = [(i, i = 1, n)]
	invperm = perm

	!print *, 'vec  = ', vec
	!print *, 'perm = ', perm

	do i = 1, n

		! Initial index of the number to move
		i0 = invperm(i)

		! Number of positions to move (signed)
		npos = vec(i0) !abs(vec(i0))

		! Direction to move (+1 or -1)
		dir = sign(1, npos)

		!print *, 'i0, dir, npos = ', i0, dir, npos

		shift = 0

		! Apply the movement to vec and perm
		do j = i0 + dir, i0 + npos, dir

			! Indices of 2 numbers to be swapped
			j1 = modulo(j-1    , n) + 1
			j2 = modulo(j-dir-1, n) + 1

			!print *, 'j, j1, j2 = ', j, j1, j2

			vec ([j1, j2]) = vec ([j2, j1])
			perm([j1, j2]) = perm([j2, j1])

			! If we swap elements on the ends, then we have to shift the whole
			! array by one to correct.  But, don't do this until we're done
			! bubbling i0 to it's mixed position
			if      (j1 == 1 .and. j2 == n) then
				shift = shift - 1
			else if (j2 == 1 .and. j1 == n) then
				shift = shift + 1
			end if

		end do

		! This case also counts as a circular shift
		if (i0 + npos == 1 .and. dir < 0) shift = shift + 1

		vec  = cshift(vec , shift)
		perm = cshift(perm, shift)

		! Invert the permutation
		do j = 1, n
			invperm(perm(j)) = j
		end do

		!print *, 'vec  = ', vec
		!print *, 'perm = ', perm
		!print *, 'invp = ', invperm
		!print *, ''

	end do

	!print *, 'vec  = ', vec

	! Find the index of 0
	i00 = minloc(abs(vec - 0))
	i0 = i00(1)
	do i = i0 + 1000, i0 + 3000, 1000
		!print *, 'adding ', vec(modulo(i-1, n) + 1)
		isum = isum + vec(modulo(i-1, n) + 1)
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

