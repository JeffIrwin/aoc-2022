
!===============================================================================

module m

	use utils

	implicit none

#if 0
	character(len = *), parameter :: finput = 'test-input.txt'
#else
	character(len = *), parameter :: finput = 'input.txt'
#endif
	
	integer, parameter :: np = 5, nbmax = 5

	integer, parameter :: nbp(np) = &
		[      &
			4, &
			5, &
			5, &
			4, &
			4  &
		]

	integer, parameter :: p(2, nbmax, np) = reshape( &
		[                            &
			1,1, 2,1, 3,1, 4,1, 0,0, &  ! horizontal line
			2,1, 1,2, 2,2, 3,2, 2,3, &  ! plus sign
			1,1, 2,1, 3,1, 3,2, 3,3, &  ! reverse L
			1,1, 1,2, 1,3, 1,4, 0,0, &  ! vertical line
			1,1, 2,1, 1,2, 2,2, 0,0  &  ! square
		], [2, nbmax, np])

	! Piece IDs (or empty)
	integer, parameter :: &
		empty  = 0, &
		hor    = 1, &
		plus   = 2, &
		revl   = 3, &
		ver    = 4, &
		square = 5

contains

!===============================================================================

subroutine part2()

	character :: c, s*(1024*16)

	integer(kind = 8) :: i, j, ii, it, iu, io, ns, ix, iy, ymaxrest, ix0, iy0, &
			dx, dy, ip, x, y, xmin, xmax, ymin, ymax, ymaxmax, ybuf, ybuf0

	integer(kind = 8), parameter ::&
			!n = 2022
			n     = 1000000000000_8     ! number of pieces to simulate

	integer, parameter :: &
			nx    = 7                 ! width

	! Size of rock buffer array and swap shift amount
	!integer, parameter :: nbuf = 16*1024*256, nbuf2 = nbuf / 2
	integer, parameter :: nbuf = 1024*1024*256, nbuf2 = nbuf / 2

	integer(kind=1), allocatable :: rocks(:,:), tmp(:,:)

	logical :: rest, collide

	open(file = finput, newunit = iu, status = 'old')

	do
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		!print *, 's = ', trim(s)

	end do

	close(iu)

	!print *, 'p = ', p

	ns = len_trim(s)

	ymaxrest = 0
	ymaxmax = 0

	! Time index
	it = 0

	! State of rested pieces
	allocate(rocks(nx, nbuf))
	rocks = empty

	ybuf = 1

	! Loop through pieces
	do i = 1, n
		if (mod(i, 10000000) == 0) print *, 'i = ', i

		! Initial coordinates of min corner of piece.  Note that the piece does
		! not necessarily have a block in this position, e.g. the plus sign
		ix = 2
		iy = ymaxmax + 3
		!iy = ymaxrest + 3

		!if (i == 1) iy = iy - 1

		ip = mod(i-1, np) + 1
		!print *, 'ip = ', ip
		!print *, 'nbp = ', nbp(ip)

		! Loop until the piece comes to rest
		rest = .false.
		do while (.not. rest)

			! Previous position
			ix0 = ix
			iy0 = iy

			! Movement
			dx = 0
			dy = 0

			it = it + 1
			if (mod(it, 2) == 1) then
				! Horizontal motion

				! Input string index
				ii = mod((it / 2), ns) + 1

				c = s(ii:ii)

				!print *, 'ii = ', ii
				!print *, 'c = ', c

				if (c == '>') then
					!print *, 'right'
					dx =  1
				else if (c == '<') then
					!print *, 'left'
					dx = -1
				else
					write(*,*) 'Error: unkown input char "'//c//'"'
					write(*,*) 'ii = ', ii
					write(*,*) 'it = ', it
					stop
				end if

			else
				! Vertical motion
				!print *, 'down'
				dy = -1

			end if

			ix = ix0 + dx
			iy = iy0 + dy

			! Check for collisions
			collide = .false.

			! Wall collisions.  This could be optimized by getting each piece's
			! bounding box ahead of time
			xmin = ix! + p(1, 1, ip)
			xmax = ix + p(1, 1, ip)
			ymin = iy! + p(2, 1, ip)
			ymax = iy + p(2, 1, ip)
			do j = 2, nbp(ip)
				!xmin = min(xmin, ix + p(1, j, ip))
				xmax = max(xmax, ix + p(1, j, ip))
				!ymin = min(ymin, iy + p(2, j, ip))
				ymax = max(ymax, iy + p(2, j, ip))
			end do

			!print *, 'x in ', xmin, xmax
			!print *, 'y in ', ymin, ymax

			collide = collide .or. (xmin < 1 .or. xmax > nx)

			! Floor collisions
			collide = collide .or. (ymin < 1)

			! Check for collisions with an old piece which has come to rest
			if (.not. collide) then
				do j = 1, nbp(ip)
					x = ix + p(1, j, ip)
					y = iy + p(2, j, ip)
					collide = collide .or. (rocks(x,y-ybuf+1) /= empty)
				end do
			end if

			! Reset to previous position
			if (collide) then
				ix = ix0
				iy = iy0
				ymax = ymax - dy
			end if

			! Only downward motion can bring a piece to rest
			!rest = rest .or. (collide .and. dy < 0)
			if (collide .and. dy < 0) then

				rest = .true.

				! Top of last piece
				ymaxrest = ymax

				! Top of highest piece
				ymaxmax = max(ymaxmax, ymaxrest)

				!print *, 'ymaxrest = ', ymaxrest
				!print *, 'ymaxmax = ', ymaxmax

				do j = 1, nbp(ip)
					x = ix + p(1, j, ip)
					y = iy + p(2, j, ip)
					rocks(x,y-ybuf+1) = ip
				end do

				!call printrocks(rocks)
				!print *, 'ymaxrest, ymaxmax = ', ymaxrest, ymaxmax

				if (ymaxmax + 8 >= ybuf + nbuf) then
					print *, 'swapping'

					! Shift the buffer up and swap to a new array
					ybuf0 = ybuf

					ybuf = ybuf + nbuf2

					allocate(tmp(nx, nbuf))
					do j = 1, nbuf - nbuf2
						tmp(:,j) = rocks(:, j+nbuf2)
					end do
					do j = nbuf - nbuf2 + 1, nbuf
						tmp(:,j) = empty
					end do

					call move_alloc(tmp, rocks)

				end if

			end if

			!if (it == 100) return

		end do

		!! Benchmarking
		!if (i == 100000000) exit

	end do
	!call printrocks(rocks)

	print *, 'ymaxrest, ymaxmax = ', ymaxrest, ymaxmax

	write(*,*) 'part2 = ', ymaxmax
	write(*,*) ''

end subroutine part2

!===============================================================================

subroutine part1()

	character :: c, s*(1024*16)

	integer :: i, j, ii, it, iu, io, ns, ix, iy, ymaxrest, ix0, iy0, &
			dx, dy, ip, x, y, xmin, xmax, ymin, ymax, ymaxmax

	integer, parameter ::    &
			n     = 2022,    &  ! number of pieces to simulate
			nx    = 7,       &  ! width
			nymax = 2048 * 8    ! height capacity

	integer, allocatable :: rocks(:,:)

	logical :: rest, collide

	open(file = finput, newunit = iu, status = 'old')

	do
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		!print *, 's = ', trim(s)

	end do

	close(iu)

	!print *, 'p = ', p

	ns = len_trim(s)

	ymaxrest = 0
	ymaxmax = 0

	! Time index
	it = 0

	! State of rested pieces
	allocate(rocks(nx, nymax))
	!allocate(rocks(0:nx+1, 0:nymax))  
	!rocks(0:nx+1, 0:nymax) = empty
	rocks = empty

	! Loop through pieces
	do i = 1, n
		!print *, 'i = ', i

		! Initial coordinates of min corner of piece.  Note that the piece does
		! not necessarily have a block in this position, e.g. the plus sign
		ix = 2
		iy = ymaxmax + 3
		!iy = ymaxrest + 3

		!if (i == 1) iy = iy - 1

		ip = mod(i-1, np) + 1
		!print *, 'ip = ', ip
		!print *, 'nbp = ', nbp(ip)

		! Loop until the piece comes to rest
		rest = .false.
		do while (.not. rest)

			! Previous position
			ix0 = ix
			iy0 = iy

			! Movement
			dx = 0
			dy = 0

			it = it + 1
			if (mod(it, 2) == 1) then
				! Horizontal motion

				! Input string index
				ii = mod((it / 2), ns) + 1

				c = s(ii:ii)

				!print *, 'ii = ', ii
				!print *, 'c = ', c

				if (c == '>') then
					!print *, 'right'
					dx =  1
					dy =  0
				else if (c == '<') then
					!print *, 'left'
					dx = -1
					dy =  0
				else
					write(*,*) 'Error: unkown input char "'//c//'"'
					write(*,*) 'ii = ', ii
					write(*,*) 'it = ', it
					stop
				end if

			else
				! Vertical motion
				!print *, 'down'
				dx =  0
				dy = -1

			end if

			ix = ix0 + dx
			iy = iy0 + dy

			! Check for collisions
			collide = .false.

			! Wall collisions
			xmin = ix + p(1, 1, ip)
			xmax = ix + p(1, 1, ip)
			ymin = iy + p(2, 1, ip)
			ymax = iy + p(2, 1, ip)
			do j = 2, nbp(ip)
				xmin = min(xmin, ix + p(1, j, ip))
				xmax = max(xmax, ix + p(1, j, ip))
				ymin = min(ymin, iy + p(2, j, ip))
				ymax = max(ymax, iy + p(2, j, ip))
			end do

			!print *, 'x in ', xmin, xmax
			!print *, 'y in ', ymin, ymax

			collide = collide .or. (xmin < 1 .or. xmax > nx)

			! Floor collisions
			collide = collide .or. (ymin < 1)

			! Check for collisions with an old piece which has come to rest
			if (.not. collide) then
				do j = 1, nbp(ip)
					x = ix + p(1, j, ip)
					y = iy + p(2, j, ip)
					collide = collide .or. (rocks(x,y) /= empty)
				end do
			end if

			! Reset to previous position
			if (collide) then
				ix = ix0
				iy = iy0
				ymax = ymax - dy
			end if

			! Only downward motion can bring a piece to rest
			!rest = rest .or. (collide .and. dy < 0)
			if (collide .and. dy < 0) then

				rest = .true.

				! Top of last piece
				ymaxrest = ymax

				! Top of highest piece
				ymaxmax = max(ymaxmax, ymaxrest)

				!print *, 'ymaxrest = ', ymaxrest
				!print *, 'ymaxmax = ', ymaxmax

				do j = 1, nbp(ip)
					x = ix + p(1, j, ip)
					y = iy + p(2, j, ip)
					rocks(x,y) = ip
				end do

				!call printrocks(rocks)

			end if

			!if (it == 100) return

		end do

		!if (i == 10) exit

	end do
	call printrocks(rocks)

	write(*,*) 'part1 = ', ymaxmax
	write(*,*) ''

end subroutine part1

!===============================================================================

subroutine printrocks(r)

	integer, allocatable :: r(:,:)

	integer :: ix, iy, iy0

	! Skip empty part
	iy = ubound(r,2)
	do while (all(r(:,iy) == empty))
		iy = iy - 1
	end do
	iy0 = iy

	!do while (iy > lbound(r,2))
	do iy = iy0, lbound(r,2), -1
		!do ix = lbound(r,1) + 1, ubound(r,1) - 1
		write(*, '(a)', advance = 'no') '|'
		do ix = lbound(r,1), ubound(r,1)
			if (r(ix,iy) == empty) then
				write(*, '(a)', advance = 'no') '.'
			else
				write(*, '(a)', advance = 'no') '#'
			end if
		end do
		write(*, '(a)', advance = 'no') '|'
		write(*,*)
	end do
	write(*,'(a)') '+-------+'
	write(*,*)

end subroutine printrocks

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

