
!===============================================================================

module m

	use utils

	implicit none

#if 0
	character(len = *), parameter :: finput = "test-input.txt"
#else
	character(len = *), parameter :: finput = "input.txt"
#endif

	character, parameter :: &
			ESC             = char(27)

	! ANSI escape codes.  c.f.
	! https://github.com/JeffIrwin/cali/blob/main/src/cali.f90
	character(len = *), parameter :: &
			RED         = ESC//"[91;1m", &
			MAGENTA     = ESC//"[95;1m", &
			GREEN       = ESC//"[92m", &
			RESET_COLOR = ESC//"[0m"

contains

!===============================================================================

subroutine part1()

	character :: s*256

	integer :: iu, io, isum, nx, ny, ix, iy, dx, dy

	logical(kind = 1), allocatable :: grid(:,:)

	ny = countlines(finput)
	!print *, 'ny = ', ny

	open(file = finput, newunit = iu, status = 'old')

	isum = 0

	! Read first line to get nx size then rewind
	read(iu, '(a)', iostat = io) s
	nx = len(trim(s))
	!print *, 'nx = ', nx
	rewind(iu)

	! Might not actually need to save the whole grid but who knows what's coming
	! in part 2
	allocate(grid(nx, ny))
	grid = .false.

	iy = 0
	do
		iy = iy + 1
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		!print *, 's = ', trim(s)

		do ix = 1, nx
			grid(ix, iy) = s(ix:ix) == '#'
		end do

	end do

	close(iu)

	!print *, 'grid = '
	!do iy = 1, ny
	!	print *, grid(:,iy)
	!end do

	! Start at top-left
	ix = 1
	iy = 1

	! Travel right 3, down 1 (down is positive in my convention)
	dx = 3
	dy = 1

	! Iterate in steps of dx, dy.  Don't count start position (it's empty in
	! both inputs anyway)
	do while (iy < ny)
		ix = ix + dx
		iy = iy + dy

		! Grid repeats in x dir
		ix = wrap(ix, nx)
		!print *, 'ix, iy = ', ix, iy

		if (grid(ix,iy)) isum = isum + 1

	end do

	write(*,*) 'part1 = ', isum
	write(*,*) ''

end subroutine part1

!===============================================================================

subroutine part2()

	character :: s*256

	integer, parameter :: nslopes = 5

	integer :: iu, io, isum, nx, ny, ix, iy, dx, dy, &
		slopes(2, nslopes), islope
	integer(kind = 8) :: iprod

	logical(kind = 1), allocatable :: grid(:,:)

	ny = countlines(finput)
	!print *, 'ny = ', ny

	open(file = finput, newunit = iu, status = 'old')

	! Read first line to get nx size then rewind
	read(iu, '(a)', iostat = io) s
	nx = len(trim(s))
	!print *, 'nx = ', nx
	rewind(iu)

	! Might not actually need to save the whole grid but who knows what's coming
	! in part 2
	allocate(grid(nx, ny))
	grid = .false.

	iy = 0
	do
		iy = iy + 1
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		!print *, 's = ', trim(s)

		do ix = 1, nx
			grid(ix, iy) = s(ix:ix) == '#'
		end do

	end do

	close(iu)

	!print *, 'grid = '
	!do iy = 1, ny
	!	print *, grid(:,iy)
	!end do

	! Check all of these x/y slope pairs
	slopes(:,1) = [1, 1]
	slopes(:,2) = [3, 1]
	slopes(:,3) = [5, 1]
	slopes(:,4) = [7, 1]
	slopes(:,5) = [1, 2]

	iprod = 1
	do islope = 1, nslopes

		isum = 0

		! Start at top-left
		ix = 1
		iy = 1

		! Travel right 3, down 1 (down is positive in my convention)
		dx = slopes(1,islope)
		dy = slopes(2,islope)

		! Iterate in steps of dx, dy.  Don't count start position (it's empty in
		! both inputs anyway)
		do while (iy < ny)
			ix = ix + dx
			iy = iy + dy

			! Grid repeats in x dir
			ix = wrap(ix, nx)
			!print *, 'ix, iy = ', ix, iy

			if (grid(ix,iy)) isum = isum + 1

		end do
		!print *, 'isum = ', isum

		iprod = iprod * isum

	end do

	write(*,*) 'part2 = ', iprod
	write(*,*) ''

end subroutine part2

!===============================================================================

end module m

!===============================================================================

program main

	use m

	write(*,*) MAGENTA//"Starting AOC main"//RESET_COLOR
	write(*,*) "Input file = ", finput
	write(*,*)

	call part1()
	call part2()

	write(*,*) GREEN  //"Success!"//RESET_COLOR
	write(*,*) MAGENTA//"Ending AOC main"//RESET_COLOR
	write(*,*)

end program main

!===============================================================================

