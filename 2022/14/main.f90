
!===============================================================================

module m

	use utils

	implicit none

#if 0
	character(len = *), parameter :: finput = 'test-input.txt'
#else
	character(len = *), parameter :: finput = 'input.txt'
#endif

	integer(kind = 1), parameter :: empty = 0, rock = 1, sand = 2

contains

!===============================================================================

subroutine printmap(a)

	integer(kind = 1), allocatable :: a(:,:)

	integer :: ix, iy, ixl, ixh, iyl, iyh

	! Get bounds.  This will crash on a totally empty map

	ixl = lbound(a,1)
	do while (all(a(ixl,:) == empty))
		ixl = ixl + 1
	end do

	ixh = ubound(a,1)
	do while (all(a(ixh,:) == empty))
		ixh = ixh - 1
	end do

	iyl = lbound(a,2)
	do while (all(a(:,iyl) == empty))
		iyl = iyl + 1
	end do

	iyh = ubound(a,2)
	do while (all(a(:,iyh) == empty))
		iyh = iyh - 1
	end do

	! part2
	iyh = iyh + 2

	!print *, 'ixl = ', ixl
	!print *, 'ixh = ', ixh
	print *, 'iyl = ', iyl
	!print *, 'iyh = ', iyh
	!!return

	do iy = iyl, iyh
		do ix = ixl, ixh
			if      (a(ix,iy) == empty) then
				write(*, '(a)', advance = 'no') '.'
			else if (a(ix,iy) == rock) then
				write(*, '(a)', advance = 'no') '#'
			else if (a(ix,iy) == sand) then
				write(*, '(a)', advance = 'no') 'o'
			end if
		end do
		write(*,*)
	end do
	write(*,*)

end subroutine printmap

!===============================================================================

subroutine swap(a, b)

	! TODO: move to utils

	integer :: a, b, tmp

	tmp = b
	b = a
	a = tmp

end subroutine swap

!===============================================================================

subroutine part2()

	! Sand source point
	integer, parameter :: sx = 500, sy = 0

	integer :: i, ix, iy, iyh, floory

	! Map array
	integer(kind = 1), allocatable :: a(:,:)

	logical :: rest, blocked

	a = readmap()
	!call printmap(a)

	! This could use DRYing up w/ bounds from printmap()
	iyh = ubound(a,2)
	do while (all(a(:,iyh) == empty))
		iyh = iyh - 1
	end do

	!! Add floor for part2
	!a(:, iyh+2) = rock
	floory = iyh + 2

	!print *, 'floory = ', floory

	blocked = .false.

	i = 0
	do while (.not. blocked)
		i = i + 1

		!print *, 'i = ', i

		! Create sand at source
		ix = sx
		iy = sy
		rest = .false.

		do while (.not. (rest .or. blocked))

			if (ix <= lbound(a,1) + 1) then
				write(*,*) 'Error: x underflow'
				stop
			else if (ix >= ubound(a,1) - 1) then
				write(*,*) 'Error: x overflow'
				stop
			end if

			if (iy+1 >= floory) then
				!print *, 'floor'
				a(ix,iy) = sand
				rest = .true.

			else if (a(ix, iy+1) == empty) then
				iy = iy+1

			else if (a(ix-1, iy+1) == empty) then
				iy = iy+1
				ix = ix-1

			else if (a(ix+1, iy+1) == empty) then
				iy = iy+1
				ix = ix+1

			else if (iy == sy) then
				blocked = .true.

			else
				a(ix,iy) = sand
				rest = .true.

			end if

		end do

		!call printmap(a)

	end do

	!call printmap(a)

	write(*,*) 'part2 = ', i
	write(*,*) ''

end subroutine part2

!===============================================================================

subroutine part1()

	! Sand source point
	integer, parameter :: sx = 500, sy = 0

	integer :: i, ix, iy, iyh

	! Map array
	integer(kind = 1), allocatable :: a(:,:)

	logical :: rest, freefall

	a = readmap()
	!call printmap(a)

	! This could use DRYing up w/ bounds from printmap()
	iyh = ubound(a,2)
	do while (all(a(:,iyh) == empty))
		iyh = iyh - 1
	end do

	freefall = .false.

	i = 0
	do while (.not. freefall)
		i = i + 1

		! Create sand at source
		ix = sx
		iy = sy
		rest = .false.

		do while (.not. (rest .or. freefall))

			if (a(ix, iy+1) == empty) then
				iy = iy+1
			else if (a(ix-1, iy+1) == empty) then
				iy = iy+1
				ix = ix-1
			else if (a(ix+1, iy+1) == empty) then
				iy = iy+1
				ix = ix+1
			else
				a(ix,iy) = sand
				rest = .true.
			end if

			if (iy > iyh) then
				freefall = .true.
			end if

			!! This does NOT draw the falling unit of sand.  It isn't drawn
			!! until it comes to rest.
			!call printmap(a)

		end do

		!call printmap(a)

	end do
	i = i - 1

	write(*,*) 'part1 = ', i
	write(*,*) ''

end subroutine part1

!===============================================================================

function readmap() result(a)

	character :: s*512

	integer, parameter :: nmax = 1023

	integer :: iu, io, is, ix, iy, ix0, iy0, ixa, ixb, iya, iyb

	! Map array
	integer(kind = 1), allocatable :: a(:,:)

	open(file = finput, newunit = iu, status = 'old')

	! Initialize empty map.  Source point is at y = 0 so I'm indexing from
	! there
	allocate(a(1: nmax, 1: nmax))
	!allocate(a(0: nmax, 0: nmax))
	a = empty

	do
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		!print *, 's = ', trim(s)

		! First point
		is = 1
		ix0 = readint(s, is)
		iy0 = readint(s, is)
		!print *, 'ixy0 = ', ix0, iy0

		! The rest of the points
		do while (is < len_trim(s))

			! Skip " -> " delim
			is = is + 4

			ix = readint(s, is)
			iy = readint(s, is)
			!print *, 'ixy  = ', ix, iy

			if (.not. (ix == ix0 .or. iy == iy0)) then
				write(*,*) 'Error: diagonal line'
				stop
			end if

			if (ix0 < lbound(a,1) .or. ix0 > ubound(a,1) .or. &
			    ix  < lbound(a,1) .or. ix  > ubound(a,1) .or. &
			    iy0 < lbound(a,2) .or. iy0 > ubound(a,2) .or. &
			    iy  < lbound(a,2) .or. iy  > ubound(a,2)) then
				write(*,*) 'Error: map overflow'
				stop
			end if

			ixa = ix0
			ixb = ix
			if (ixa > ixb) call swap(ixa, ixb)

			iya = iy0
			iyb = iy
			if (iya > iyb) call swap(iya, iyb)

			a(ixa:ixb, iya:iyb) = rock

			ix0 = ix
			iy0 = iy
		end do
		!print *, ''

	end do

	close(iu)

	!call printmap(a)

end function readmap

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

