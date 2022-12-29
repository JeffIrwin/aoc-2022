
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

integer function count_visible(a)

	integer, allocatable :: a(:,:)

	integer :: nx, ny, ix, iy, t

	nx = size(a, 1)
	ny = size(a, 2)

	!print *, 'nx, ny = ', nx, ny

	count_visible = 0

	do iy = 1, ny
		do ix = 1, nx

			! Tree height at ix, iy
			t = a(ix, iy)

			! Visible from west?
			if (all(a(1: ix-1, iy) < t)) then
				count_visible = count_visible + 1
				cycle
			end if

			! Visible from east?
			if (all(a(ix+1: nx, iy) < t)) then
				count_visible = count_visible + 1
				cycle
			end if

			! Visible from north?  Array a is indexed from north to south
			if (all(a(ix, 1: iy-1) < t)) then
				count_visible = count_visible + 1
				cycle
			end if

			! Visible from south?
			if (all(a(ix, iy+1: ny) < t)) then
				count_visible = count_visible + 1
				cycle
			end if

		end do
	end do

end function count_visible

!===============================================================================

integer function max_scenic_score(a)

	integer, allocatable :: a(:,:)

	integer :: nx, ny, ix, iy, t, sw, se, sn, ss, s

	nx = size(a, 1)
	ny = size(a, 2)

	!print *, 'nx, ny = ', nx, ny

	max_scenic_score = -1

	do iy = 2, ny - 1
		do ix = 2, nx - 1

			! Tree height at ix, iy
			t = a(ix, iy)

			! Score from west
			sw = 1
			do while (a(ix-sw, iy) < t)
				if (ix-sw <= 1) exit
				sw = sw + 1
			end do

			! Score from east
			se = 1
			do while (a(ix+se, iy) < t)
				if (ix+se >= nx) exit
				se = se + 1
			end do

			! Score from north.  Array a is indexed from north to south
			sn = 1
			do while (a(ix, iy-sn) < t)
				if (iy-sn <= 1) exit
				sn = sn + 1
			end do

			! Score from south
			ss = 1
			do while (a(ix, iy+ss) < t)
				if (iy+ss >= ny) exit
				ss = ss + 1
			end do

			!print '(6i4)', ix, iy, sw, se, sn, ss

			s = sw * se * sn * ss
			max_scenic_score = max(max_scenic_score, s)

		end do
	end do

end function max_scenic_score

!===============================================================================

subroutine part2()

	character :: s*256

	integer :: iu, io, isum, ix, iy
	integer :: nx, ny
	integer, allocatable :: a(:,:)

	open(file = finput, newunit = iu, status = 'old')

	isum = 0

	ix = 0
	iy = 0

	do
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		!print *, 's = ', trim(s)

		iy = iy + 1

		if (iy == 1) then
			nx = len_trim(s)

			! Assume the grid is square
			ny = nx

			allocate(a(nx, ny))
			a = -1

		end if

		do ix = 1, nx
			read(s(ix: ix), *) a(ix,iy)
		end do

	end do

	close(iu)

	!! Print tree height grid
	!do iy = 1, ny
	!	do ix = 1, nx
	!		write(*, '(i1)', advance = 'no') a(ix,iy)
	!	end do
	!	write(*,*)
	!end do

	isum = max_scenic_score(a)

	print *, 'part 2 = ', isum
	print *, ''

end subroutine part2

!===============================================================================

subroutine part1()

	character :: s*256

	integer :: iu, io, isum, ix, iy
	integer :: nx, ny
	integer, allocatable :: a(:,:)

	open(file = finput, newunit = iu, status = 'old')

	isum = 0

	ix = 0
	iy = 0

	do
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		!print *, 's = ', trim(s)

		iy = iy + 1

		if (iy == 1) then
			nx = len_trim(s)

			! Assume the grid is square
			ny = nx

			allocate(a(nx, ny))
			a = -1

		end if

		do ix = 1, nx
			read(s(ix: ix), *) a(ix,iy)
		end do

	end do

	close(iu)

	!! Print tree height grid
	!do iy = 1, ny
	!	do ix = 1, nx
	!		write(*, '(i1)', advance = 'no') a(ix,iy)
	!	end do
	!	write(*,*)
	!end do

	isum = count_visible(a)

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
	call part2()

	print *, 'Ending AOC main'
	print *, ''

end program main

!===============================================================================

