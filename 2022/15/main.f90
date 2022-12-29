
!===============================================================================

module m

	use utils

	implicit none

#if 0
	character(len = *), parameter :: finput = 'test-input.txt'
	integer, parameter :: ry = 10  ! Row of interest
	integer, parameter :: bmin = 0, bmax = 20  ! Given search bounds
#else
	character(len = *), parameter :: finput = 'input.txt'
	integer, parameter :: ry = 2000000  ! Row of interest
	integer, parameter :: bmin = 0, bmax = 4000000  ! Given search bounds
#endif

contains

!===============================================================================

subroutine readinput(s, b)

	character :: str*256

	integer :: i, is, iu, io, n

	! Sensors and beacons
	integer, allocatable :: s(:,:), b(:,:)

	n = countlines(finput)
	!print *, 'n = ', n

	if (allocated(s)) deallocate(s)
	if (allocated(b)) deallocate(b)

	allocate(s(2,n), b(2,n))

	open(file = finput, newunit = iu, status = 'old')

	do i = 1, n
		read(iu, '(a)', iostat = io) str

		!print *, 'str = ', trim(str)

		is = 1
		s(1,i) = readint(str, is)
		s(2,i) = readint(str, is)
		b(1,i) = readint(str, is)
		b(2,i) = readint(str, is)

		!print *, 's, b = ', s(:,i), b(:,i)

	end do

	close(iu)

end subroutine readinput

!===============================================================================

integer function norm1(x)

	integer :: x(:)

	norm1 = sum(abs(x))

end function norm1

!===============================================================================

subroutine part2()

	integer :: i, iy, n, norm, xmin, xmax, xl, xh, half, ix
	integer(kind = 8) :: isum

	! Sensors and beacons
	integer, allocatable :: s(:,:), b(:,:), norms(:)

	logical, allocatable :: nobeacons(:)

	isum = 0

	call readinput(s, b)
	n = size(s, 2)

	! Square bounds for search space are given for part2
	xmin = bmin
	xmax = bmax

	! If true, a beacon cannot possibly exist there in row iy
	allocate(nobeacons(xmin: xmax))

	! Precompute norms to save time
	allocate(norms(n))
	do i = 1, n
		norms(i) = norm1(s(:,i) - b(:,i))
	end do

	! There's probably some optimization that I'm missing here.  Maybe sort the
	! beacon/sensor pairs by which rows they block, then iterate only through
	! those rows instead of relying on the half >= 0 condition.  Or maybe find
	! the intersection of the nobeacons sets w/o using an actual logical array.
	! Anyway idc, this gets the answer in less than a day and I've got other
	! work to do

	! For part2 we are interested in all rows in the search space
	!$OMP parallel private(i,norm,half,xl,xh,iy,nobeacons,ix)
	!$OMP do

	do iy = bmin, bmax
	!do iy = 2639657 - 3000, 2639657 + 3000

		nobeacons = .false.
		if (mod(iy, 1000) == 0) print *, 'iy = ', iy

		!print *, 'x in ', xmin, xmax

		! Mark places where the beacons can't be
		do i = 1, n
			!print *, 's, b = ', s(:,i), b(:,i)

			norm = norms(i)
			!print *, 'norm = ', norm

			! Half-width of blocked row of interest
			half = norm - abs(s(2,i) - iy)
			!print *, 'half = ', half

			if (half >= 0) then

				! For sensor i, a beacon cannot exist from xl to xh (unless
				! beacon i exists exactly in row iy, accounted for in the next
				! loop)
				xl = s(1,i) - half
				xh = s(1,i) + half

				! Clamp to search space
				xl = max(xl, xmin)
				xh = min(xh, xmax)

				!print *, 'xl, xh = ', xl, xh
				!print *, ''

				nobeacons(xl: xh) = .true.

			end if

		end do

		if (.not. all(nobeacons)) then
			!print *, 'FOUND A POSSIBLE LOCATION'
			!print *, 'nobeacons = ', nobeacons

			!ix = findloc(nobeacons, .false.)

			ix = xmin
			do while (nobeacons(ix))
				ix = ix + 1
			end do

			print *, 'x,y = ', ix, iy

			! Could exit outer loop early here, but you should also check that
			! there's ONLY one possible location

			isum = int(4000000, 8) * ix + iy
			print *, 'isum = ', isum

		end if

	end do
	!$OMP end do
	!$OMP end parallel

	write(*,*) 'part2 = ', isum
	write(*,*) ''

end subroutine part2

!===============================================================================

subroutine part1()

	integer :: i, isum, n, norm, xmin, xmax, xl, xh, half

	! Sensors and beacons
	integer, allocatable :: s(:,:), b(:,:)

	logical, allocatable :: nobeacons(:)

	isum = 0

	call readinput(s, b)
	n = size(s, 2)

	! First pass: get bounds of row of interest

	! TODO: may need to pad by max norm s-b
	xmin = min(minval(s(1,:)), minval(b(1,:)))
	xmax = max(maxval(s(1,:)), maxval(b(1,:)))

	!print *, 'x in ', xmin, xmax

	! Second pass: mark places where the beacons can't be
	do i = 1, n
		!print *, 's, b = ', s(:,i), b(:,i)

		norm = norm1(s(:,i) - b(:,i))
		!print *, 'norm = ', norm

		! Half-width of blocked row of interest
		half = norm - abs(s(2,i) - ry)
		!half = max(norm - abs(s(2,i) - ry), 0)
		!half = max(abs(s(2,i) - ry), 0)
		!print *, 'half = ', half

		! For sensor i, a beacon cannot exist from xl to xh (unless beacon
		! i exists exactly in row ry, accounted for in the next loop)
		xl = s(1,i) - half
		xh = s(1,i) + half

		!print *, 'xl, xh = ', xl, xh
		!print *, ''

		xmin = min(xmin, xl)
		xmax = max(xmax, xh)

		!nobeacons(xl: xh) = .true.

		!if (i == 7) exit

	end do

	! If true, a beacon cannot possibly exist there in row ry
	allocate(nobeacons(xmin: xmax))
	nobeacons = .false.

	!print *, 'x in ', xmin, xmax

	! Second pass: mark places where the beacons can't be
	do i = 1, n

		norm = norm1(s(:,i) - b(:,i))

		! Half-width of blocked row of interest
		half = norm - abs(s(2,i) - ry)

		! For sensor i, a beacon cannot exist from xl to xh (unless beacon
		! i exists exactly in row ry, accounted for in the next loop)
		xl = s(1,i) - half
		xh = s(1,i) + half

		nobeacons(xl: xh) = .true.

	end do

	! Don't count places where there are known beacons!
	do i = 1, n
		if (b(2,i) == ry) then
			nobeacons(b(1,i)) = .false.
		end if
	end do

	write(*,*) 'part1 = ', count(nobeacons)
	write(*,*) ''

end subroutine part1

!===============================================================================

end module m

!===============================================================================

program main

	use m

	write(*,*) 'Starting AOC main'
	write(*,*) ''

	call part1()
	call part2()

	write(*,*) 'Ending AOC main'
	write(*,*) ''

end program main

!===============================================================================

