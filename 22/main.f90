
!===============================================================================

module m

	use utils

	implicit none

#if 0
	character(len = *), parameter :: finput = 'test-input.txt'
	integer, parameter :: nc = 4
#else
	character(len = *), parameter :: finput = 'input.txt'
	integer, parameter :: nc = 50
#endif

	! Open tiles ".", solid walls "#", and off-map " "
	integer, parameter :: open = 1, solid = 2, off = 3

	! Path turn encodings
	integer, parameter :: right = -1, left = -2, invalid = -3

	! Cardinal directions.  c.f. day 12.  These are in CCW order: right,
	! up, left, down.  Note y is positive down!
	integer, parameter :: nd = 2, ndirs = 4
	integer, parameter :: dirs(nd,ndirs) = &
		reshape([  &
			 +1,  0, &
			  0, -1, &
			 -1,  0, &
			  0, +1  &
		], [nd,ndirs])

	! Heading encodings.  Same ordering convention as dirs
	integer, parameter :: east = 1, north = 2, west = 3, south = 4

contains

!===============================================================================

subroutine readinput(map, path)

	character :: s*(1024*8)

	integer :: i, is, iu, io, nx, ny, ix, iy

	integer, allocatable :: map(:,:), path(:)

	! After the map is a blank line and the path line
	ny = countlines(finput) - 2

	open(file = finput, newunit = iu, status = 'old')

	! First pass: get x width (first row may not be full width)
	nx = 0
	do i = 1, ny
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		!print *, 's = ', trim(s)
		nx = max(nx, len_trim(s))

	end do
	rewind(iu)

	!print *, 'nx, ny = ', nx, ny

	! Second pass: save map
	allocate(map(nx,ny))
	map = off
	do iy = 1, ny
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		!print *, 's = ', trim(s)
		!print *, 'iy = ', iy

		do ix = 1, len_trim(s)
			if      (s(ix:ix) == '.') then
				map(ix,iy) = open
			else if (s(ix:ix) == '#') then
				map(ix,iy) = solid
			end if
		end do

	end do

	!print *, 'map = '
	!do iy = 1, ny
	!	do ix = 1, nx
	!		if      (map(ix,iy) == open) then
	!			write(*, '(a)', advance = 'no') '.'
	!		else if (map(ix,iy) == solid) then
	!			write(*, '(a)', advance = 'no') '#'
	!		else if (map(ix,iy) == off) then
	!			write(*, '(a)', advance = 'no') ' '
	!		end if
	!	end do
	!	write(*,*)
	!end do

	! Skip blank line
	read(iu, '(a)', iostat = io) s

	! Read path.  Move and turn instructions are alternated in the encoded
	! path, the same way that numbers and letters are alternated in the
	! input.  There are never two moves or two turns in a row.
	read(iu, '(a)', iostat = io) s

	! Actual path may be shorter
	allocate(path(len_trim(s)))
	path = invalid

	is = 1
	i = 0
	do while (is < len_trim(s))
		i = i + 1

		! Read a number
		path(i) = readint(s, is)

		! Read a letter (if not end)
		if (is < len_trim(s)) then
			i = i + 1
			if (s(is:is) == 'R') then
				path(i) = right
			else
				path(i) = left
			end if
		end if

	end do

	close(iu)

	! Trim
	path = path(1:i)

	!print *, 'path = ', path

end subroutine readinput

!===============================================================================

integer function wrap(i,n)
	! Wrap i to range [1, n]
	integer :: i, n
	wrap = modulo(i - 1, n) + 1
end function wrap

!===============================================================================

subroutine part2()

	character(len = :), allocatable :: case

	integer :: i, j, isum, ix, iy, nx, ny, heading, dx, dy, nmove, row, &
			column, facing, ix0, iy0, heading0

	integer, allocatable :: map(:,:), path(:)

	isum = 0

	call readinput(map, path)
	nx = size(map, 1)
	ny = size(map, 2)

	!print *, 'nc = ', nc

	! input.txt 2D projection wrapping is different than
	! test-input.txt!  Convert the input to the test wrapping
	! convention

	! test-input.txt, our standard convention, is like this:
	!
	!   +-x>    +---+
	!   y       |o  |
	!   v       | U |
	!           |   |
	!   +---+---+---+
	!   |o  |o  |o  |
	!   | B | L | F |
	!   |   |   |   |
	!   +---+---+---+---+
	!           |o  |o  |
	!           | D | R |
	!           |   |   |
	!           +---+---+
	!
	! The global coordinate axes are shown above, and "o" marks each face's
	! original corner.
	!
	! input.txt, is like this:
	!
	!      +-x> +---+---+
	!      y    |o  |   |
	!      v    | U | R |
	!           |   |  o|
	!           +---+---+
	!           |o  |
	!           | F |
	!           |   |
	!       +---+---+
	!       |   |o  |
	!       | L | D |
	!       |o  |   |
	!       +---+---+
	!       |   |
	!       | B |
	!       |o  |
	!       +---+
	!

	! Find starting position: leftmost open tile of the top row
	iy = 1
	ix = 1
	do while (map(ix,iy) /= open)
		ix = ix + 1
	end do

	! Initial heading is right (dirs(:,1))
	heading = 1

	!print *, 'start = ', ix, iy

	! Loop through path instructions
	do i = 1, size(path)

		nmove = 0
		if      (path(i) == left) then
			! Left = CCW = positive
			heading = heading + 1
		else if (path(i) == right) then
			! Right = CW = negative
			heading = heading - 1
		else
			nmove = path(i)
		end if

		if (nmove < 0) then
			write(*,*) 'Error: invalid path instruction'
			stop
		end if

		heading = wrap(heading, ndirs)

		do j = 1, nmove

			ix0 = ix
			iy0 = iy
			heading0 = heading

			dx = dirs(1,heading)
			dy = dirs(2,heading)

			!print '(a,3i4)', 'ix, iy, hd = ', ix, iy, heading

			! Advance 1 move
			!
			! There are 5 normal edges that don't require any additional
			! wrapping logic.  The other 7 edges are special, and we need
			! logic for crossing them in both directions, yielding 2*7 = 14
			! cases
			!
			! First we have the "forward" cases, following the forward
			! directions of the arrows in my diagram, labelled 1-7.  Later we
			! will have the "reverse" cases in the opposite directions
			!
			! To make matters exponentially worse, the small test-input.txt and have
			! the real data have totally different wrapped mapping conventions which
			! I didn't notice until I already implemented these cases for the test.
			! So we can't share any code across both data sets :(
			!
			! I would not have implemented it like this if I looked at the data
			! earlier.  I guess I would have tried converting the 2D map to a fully 3D
			! cube and moving around in 3D instead :shrug:

			if (finput == 'input.txt') then

			if      (ix == nc + 1 .and. (iy-1)/nc == 1 .and. heading == west) then

				! Case 1: from F to L
				case = 'case 1'
				iy = 2 * nc + 1
				ix = iy0 - nc
				heading = south

			else if (iy == 2 * nc + 1 .and. (ix-1)/nc == 0 .and. heading == north) then

				! Case 1r: from L to F
				case = 'case 1r'
				ix = nc + 1
				iy = nc + (ix0 - 0)
				heading = east

			else if (ix == 2 * nc .and. (iy-1)/nc == 1 .and. heading == east) then

				! Case 2: from F to R
				case = 'case 2'
				iy = nc
				ix = 2 * nc + iy0 - nc
				heading = north

			else if (iy == nc .and. (ix-1)/nc == 2 .and. heading == south) then

				! Case 2r: from R to F
				case = 'case 2r'
				ix = 2 * nc
				iy = nc + (ix0 - 2 * nc)
				heading = west

			else if (iy == 3 * nc .and. (ix-1)/nc == 1 .and. heading == south) then

				! Case 3: from D to B
				case = 'case 3'
				ix = nc
				iy = 3 * nc + ix0 - nc
				heading = west

			else if (ix == nc .and. (iy-1)/nc == 3 .and. heading == east) then

				! Case 3r: from B to D
				case = 'case 3r'
				iy = 3 * nc
				ix = nc + (iy0 - 3 * nc)
				heading = north

			else if (ix == nc + 1 .and. (iy-1)/nc == 0 .and. heading == west) then

				! Case 4: from U to L
				case = 'case 4'
				ix = 1
				iy = 3 * nc + 1 - (iy0 - 0)
				heading = east

			else if (ix == 1 .and. (iy-1)/nc == 2 .and. heading == west) then

				! Case 4r: from L to U
				case = 'case 4r'
				ix = nc + 1
				iy = nc + 1 - (iy0 - 2 * nc)
				heading = east

			else if (ix == 3 * nc .and. (iy-1)/nc == 0 .and. heading == east) then

				! Case 5: from R to D
				case = 'case 5'
				ix = 2 * nc
				iy = 3 * nc + 1 - (iy0 - 0)
				heading = west

			else if (ix == 2 * nc .and. (iy-1)/nc == 2 .and. heading == east) then

				! Case 5r: from D to R
				case = 'case 5r'
				ix = 3 * nc
				iy = nc + 1 - (iy0 - 2 * nc)
				heading = west

			else if (iy == 1 .and. (ix-1)/nc == 2 .and. heading == north) then

				! Case 6: from R to B
				case = 'case 6'
				iy = 4 * nc
				ix = 0 + (ix0 - 2 * nc)
				heading = north

			else if (iy == 4 * nc .and. (ix-1)/nc == 0 .and. heading == south) then

				! Case 6r: from B to R
				case = 'case 6r'
				iy = 1
				ix = 2 * nc + (ix0 - 0)
				heading = south

			else if (iy == 1 .and. (ix-1)/nc == 1 .and. heading == north) then

				! Case 7: from U to B
				case = 'case 7'
				ix = 1
				iy = 3 * nc + (ix0 - nc)
				heading = east

			else if (ix == 1 .and. (iy-1)/nc == 3 .and. heading == west) then

				! Case 7r: from B to U
				case = 'case 7r'
				iy = 1
				ix = nc + (iy0 - 3 * nc)
				heading = south

			else

				! Default case: advance 1 move with no special logic
				case = 'case default'
				ix = ix + dx
				iy = iy + dy

			end if

      !*******************************
			else ! finput = 'test-input.txt'
      !*******************************

			if      (ix == 2 * nc + 1 .and. heading == west .and. iy <= 1 * nc) then

				! Case 1: from U to L
				case = 'case 1'
				iy = 1 * nc + 1
				ix = 1 * nc + iy0
				heading = south

			else if (iy == 2 * nc .and. heading == south &
					.and. (ix-1) / nc == 1) then

				! Case 2: from L to D
				case = 'case 2'
				ix = 2 * nc + 1
				iy = 3 * nc + 1 - (ix0 - nc)
				heading = east

			else if (ix == 3 * nc .and. (iy-1) / nc == 1 .and. heading == east) then

				! Case 3: from F to R.  This is basically the only special case covered
				! in the small example
				case = 'case 3'
				iy = 2 * nc + 1
				ix = 4 * nc + 1 - (iy0 - nc)
				heading = south

			else if (iy == 1 .and. heading == north) then

				! Case 4: from U to B
				case = 'case 4'
				iy = nc + 1
				ix = 3 * nc + 1 - ix0
				heading = south

			!else if (iy == 3 * nc .and. ix <= 3 * nc .and. heading == south) then
			else if (iy == 3 * nc .and. (ix-1) / nc == 2 .and. heading == south) then

				! Case 5: from D to B
				case = 'case 5'
				iy = 2 * nc
				ix = 3 * nc + 1 - ix0
				heading = north

			!else if (ix == 3 * nc .and. iy <= nc .and. heading == east) then
			else if (ix == 3 * nc .and. (iy-1) / nc == 0 .and. heading == east) then

				! Case 6: from U to R
				case = 'case 6'
				ix = 4 * nc
				!iy = 3 * nc + 1 - (nc - iy0)
				iy = 3 * nc + 1 - iy0
				heading = west

			else if (ix == 1 .and. heading == west) then

				! Case 7: from B to R
				case = 'case 7'
				iy = 3 * nc
				ix = 4 * nc + 1 - (iy0 - nc)
				heading = north

			! Reverse cases (r)
			else if (iy == nc + 1 .and. (ix-1)/nc == 1 .and. heading == north) then

				! Case 1r: from L to U
				case = 'case 1r'
				ix = 2 * nc + 1
				iy = ix0 - nc
				heading = east

			else if (ix == 2*nc + 1 .and. (iy-1) / nc == 2 .and. heading == west) then

				! Case 2r: from D to L
				case = 'case 2r'
				iy = 2 * nc
				ix = 2 * nc + 1 - (iy0 - 3 * nc)
				heading = north

			else if (iy == 2 * nc + 1 .and. (ix-1) / nc == 3 .and. heading == north) then

				! Case 3r: from R to F
				case = 'case 3r'
				ix = 3 * nc
				iy = 2 * nc + 1 - (ix0 - 3 * nc)
				heading = west

			else if (iy == nc + 1 .and. (ix-1) / nc == 0 .and. heading == north) then

				! Case 4r: from B to U
				case = 'case 4r'
				iy = 1
				ix = 3 * nc + 1 - ix0
				heading = south

			else if (iy == 2 * nc .and. (ix-1) / nc == 0 .and. heading == south) then

				! Case 5r: from B to D
				case = 'case 5r'
				iy = 3 * nc
				ix = 3 * nc + 1 - ix0
				heading = north

			else if (ix == 4 * nc .and. (iy-1) / nc == 2 .and. heading == east) then

				! Case 6r: from R to U
				case = 'case 6r'
				ix = 3 * nc
				iy = nc + 1 - (iy0 - 2 * nc)
				heading = west

			else if (iy == 3 * nc .and. (ix-1) / nc == 3 .and. heading == south) then

				! Case 7r: from R to B
				case = 'case 7r'
				ix = 1
				iy = 2 * nc + 1 - (ix0 - 3 * nc)
				heading = east

			else

				! Default case: advance 1 move with no special logic
				case = 'case default'
				ix = ix + dx
				iy = iy + dy

			end if

			end if ! input.txt / test-input.txt

			!print *, case
			!print *, ''

			if (map(ix,iy) == off) then
				write(*,*) 'Error: off map'
				write(*,*) 'ix, iy = ', ix, iy
				stop
			end if

			! Stop just before a solid wall.  It's important to check for this
			! *after* the wrap-around logic above
			if (map(ix,iy) == solid) then
				ix = ix0
				iy = iy0
				heading = heading0
				exit
			end if

		end do

		!print *, 'heading = ', heading
		!print *, 'ix, iy = ', ix, iy

	end do

	row = iy
	column = ix

	! I should've just used this convention to begin with
	if (heading == 1) then
		facing = 0
	else if (heading == 2) then
		facing = 3
	else if (heading == 3) then
		facing = 2
	else if (heading == 4) then
		facing = 1
	end if

	!print *, 'col    = ', column
	!print *, 'row    = ', row
	!print *, 'facing = ', facing

	isum = 1000 * row + 4 * column + facing

	write(*,*) 'part2 = ', isum
	write(*,*) ''

end subroutine part2

!===============================================================================

subroutine part1()

	integer :: i, j, isum, ix, iy, nx, ny, heading, dx, dy, nmove, row, &
			column, facing, ix0, iy0

	integer, allocatable :: map(:,:), path(:)

	isum = 0

	call readinput(map, path)
	nx = size(map, 1)
	ny = size(map, 2)

	! Find starting position: leftmost open tile of the top row
	iy = 1
	ix = 1
	do while (map(ix,iy) /= open)
		ix = ix + 1
	end do

	! Initial heading is right (dirs(:,1))
	heading = 1

	!print *, 'start = ', ix, iy

	! Loop through path instructions
	do i = 1, size(path)

		nmove = 0
		if      (path(i) == left) then
			! Left = CCW = positive
			heading = heading + 1
		else if (path(i) == right) then
			! Right = CW = negative
			heading = heading - 1
		else
			nmove = path(i)
		end if

		if (nmove < 0) then
			write(*,*) 'Error: invalid path instruction'
			stop
		end if

		!heading = modulo(heading - 1, ndirs) + 1
		heading = wrap(heading, ndirs)

		dx = dirs(1,heading)
		dy = dirs(2,heading)

		do j = 1, nmove

			ix0 = ix
			iy0 = iy

			! Wrap around at the edge of the map
			do while (map(wrap(ix+dx,nx), wrap(iy+dy,ny)) == off)
				ix = wrap(ix + dx, nx)
				iy = wrap(iy + dy, ny)
			end do

			! Stop just before a solid wall.  It's important to check for this
			! *after* the wrap-around logic above
			if (map(wrap(ix+dx,nx), wrap(iy+dy,ny)) == solid) then
				ix = ix0
				iy = iy0
				exit
			end if

			! Advance 1 move
			ix = wrap(ix + dx, nx)
			iy = wrap(iy + dy, ny)

		end do

		!print *, 'heading = ', heading
		!print *, 'ix, iy = ', ix, iy

	end do

	row = iy
	column = ix

	! I should've just used this convention to begin with
	if (heading == 1) then
		facing = 0
	else if (heading == 2) then
		facing = 3
	else if (heading == 3) then
		facing = 2
	else if (heading == 4) then
		facing = 1
	end if

	isum = 1000 * row + 4 * column + facing

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

