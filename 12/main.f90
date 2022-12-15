
!===============================================================================

module m

	use utils

	implicit none

	! Debug on a flat height map with start at one corner and end at
	! opposite corner
	logical, parameter :: debug = .false.

#if 0
	character(len = *), parameter :: finput = 'test-input.txt'
#else
	character(len = *), parameter :: finput = 'input.txt'
#endif

	integer :: startx, starty, endx, endy, nstepsmin = -1
	logical, allocatable :: v0(:,:)

contains

!===============================================================================

function readinput() result(height)

	character :: s*256, c

	integer :: iu, io, nx, ny, ix, iy
	integer, allocatable :: height(:,:)

	if (debug) then

		nx = 7
		ny = 6
		allocate(height(nx,ny))
		allocate(v0(nx,ny))
		v0 = .false.
		height = 0
		startx = 1
		starty = 1
		endx = nx
		endy = ny

		return
	end if

	open(file = finput, newunit = iu, status = 'old')

	! Count lines
	ny = 0
	do
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		ny = ny + 1
		!print *, 's = ', trim(s)

	end do

	nx = len_trim(s)

	!print *, 'nx, ny = ', nx, ny

	if (allocated(height)) deallocate(height)
	if (allocated(v0)) deallocate(v0)

	allocate(height(nx, ny))
	allocate(v0(nx,ny))
	v0 = .false.

	! Re-read, convert letters to numerical heights, and save
	rewind(iu)
	do iy = 1, ny
		read(iu, '(a)', iostat = io) s

		do ix = 1, nx
			c = s(ix:ix)

			if      (c == 'S') then
				c = 'a'
				startx = ix
				starty = iy
			else if (c == 'E') then
				c = 'z'
				endx = ix
				endy = iy
			end if

			height(ix,iy) = iachar(c) - iachar('a')

		end do

	end do

	close(iu)

	!call printheight(height)

	!print *, 'start = ', startx, starty
	!print *, 'end   = ',   endx,   endy
	!print *, ''

end function readinput

!===============================================================================

subroutine printheight(height)

	integer, allocatable :: height(:,:)

	integer :: ix, iy

	print *, 'height = '
	do iy = 1, size(height, 2)
		do ix = 1, size(height, 1)
			write(*, '(i3)', advance = 'no') height(ix,iy)
		end do
		write(*,*)
	end do
	write(*,*)

end subroutine printheight

!===============================================================================

recursive subroutine dfs(h, v, ix0, iy0, nsteps)

	integer, allocatable :: h(:,:)
	logical, allocatable :: v(:,:), vl(:,:)

	integer, intent(in) :: ix0, iy0
	integer :: nsteps

	integer :: nsl, i, ix, iy

	! Cardinal directions
	integer, parameter :: d(2,4) = &
		reshape([    &
			 -1,  0, &
			 +1,  0, &
			  0, -1, &
			  0, +1  &
		], [2,4])

	!nsl = 0
	!if (present(nsteps0)) nsl = nsteps0
	!nsteps = nsl + 1

	!print *, 'ix0, iy0 = ', ix0, iy0

	! Destination reached
	if (ix0 == endx .and. iy0 == endy) then
		print *, 'FOUND DESTINATION'
		print *, 'nsteps = ', nsteps
		print *, ''

		if (nstepsmin < 0 .or. nsteps < nstepsmin) then
			nstepsmin = nsteps
		end if
		return
	end if

	nsl = nsteps + 1

	! Find ALL paths, not just one.  Make a local copy of the visitation
	! markings.  If we use a shared global variable here, DFS returns
	! after it finds the first path.
	allocate(vl(size(v,1), size(v,2)))
	vl = v

	! Mark visited
	!h(ix0, iy0) = -1
	vl(ix0,iy0) = .true.

	!call printheight(h)
	!print *, 'size(h,:) = ', size(h,1), size(h,2)

	! Search neighbors
	do i = 1, 4

		!print *, 'd(:,i) = ', d(:,i)

		ix = ix0 + d(1,i)
		iy = iy0 + d(2,i)

		!print *, '  ix, iy = ', ix, iy

		! Check bounds
		if (.not. (1 <= ix .and. ix <= size(h,1) &
			  .and.  1 <= iy .and. iy <= size(h,2))) then
			cycle
		end if

		! Check if already visited
		if (v(ix,iy)) cycle
		!if (h(ix,iy) < 0) cycle

		! Check height does not increase by more than 1
		if (h(ix,iy) > h(ix0,iy0) + 1) cycle

		!! Mark visited
		!h(ix0, iy0) = -1

		call dfs(h, vl, ix, iy, nsl)

	end do

	!print *, 'd = '
	!print '(2i3)', d

end subroutine dfs

!===============================================================================

subroutine dijkstra(h, ix0, iy0)

	! Special-purpose Dijkstra's algorithm on 2D heightmap h from source
	! point [ix0, iy0]

	integer, allocatable :: h(:,:), dist(:,:), prev(:,:,:)

	integer, intent(in) :: ix0, iy0

	integer :: iv, ix, iy, nx, ny, u(2), v(2), alt

	logical, allocatable :: q(:,:)

	! Cardinal directions
	integer, parameter :: d(2,4) = &
		reshape([    &
			 -1,  0, &
			 +1,  0, &
			  0, -1, &
			  0, +1  &
		], [2,4])

	nx = size(h,1)
	ny = size(h,2)

	allocate(dist(nx, ny))
	allocate(prev(2, nx, ny))
	allocate(   q(nx, ny))

	prev = 0

	! Initialize distance to infinity
	dist = huge(dist)

	!print *, 'dist = ', dist

	! q is true if a vertex is in it.  Initialize with all vertices in q
	q = .true.

	! Initialize source vertex with 0 distance
	dist(ix0, iy0) = 0

	! While q is not empty
	do while (any(q))

		! Vertex in q with min dist
		u = minloc(dist, q)

		! Remove u from q
		q(u(1), u(2)) = .false.

		! For each neighbor v of u still in q

		! Search neighbors
		do iv = 1, 4

			v = u + d(:,iv)

			!print *, 'd(:,iv) = ', d(:,iv)
			!print *, '  v = ', v

			! Check bounds
			if (.not. (1 <= v(1) .and. v(1) <= size(h,1) &
			    .and.  1 <= v(2) .and. v(2) <= size(h,2))) then
				cycle
			end if

			if (.not. q(v(1), v(2))) cycle

			! Check height does not increase by more than 1
			if (h(v(1),v(2)) > h(u(1),u(2)) + 1) cycle

			! All graph edge distances are 1
			alt = dist(u(1), u(2)) + 1

			if (alt < dist(v(1), v(2))) then
				dist(v(1), v(2)) = alt

				! This is part of Dijkstra, but it's not strictly required to
				! solve the AOC problem
				prev(:, v(1), v(2)) = u

			end if

		end do

	end do

	!print *, 'dist(end) = ', dist(endx, endy)

	!print *, 'Shortest path (reversed) = '
	!u = [endx, endy]
	!do while (.not. all(u == [ix0, iy0]))
	!	print *, u
	!	u = prev(:, u(1), u(2))
	!end do
	!print *, u

	! This is the return value for AOC
	nstepsmin = dist(endx, endy)

end subroutine dijkstra

!===============================================================================

subroutine part1()

	integer :: isum
	integer, allocatable :: h(:,:)

	isum = 0

	h = readinput()

	!! I think this works, but it's exponentially slow!
	!call dfs(h, v0, startx, starty, isum)

	call dijkstra(h, startx, starty)

	write(*,*) 'part1 = ', nstepsmin
	write(*,*) ''

end subroutine part1

!===============================================================================

subroutine part2()

	integer :: nstepsminmin, ix, iy
	integer, allocatable :: h(:,:)

	h = readinput()
	nstepsminmin = huge(nstepsminmin)

	do ix = 1, size(h,1)
	do iy = 1, size(h,2)

		if (h(ix,iy) == 0) then
			call dijkstra(h, ix, iy)

			! For some starting points the destination is unreachable
			if (nstepsmin >= 0 .and. nstepsmin < huge(nstepsmin)) then
				nstepsminmin = min(nstepsminmin, nstepsmin)
			end if

		end if

	end do
	end do

	write(*,*) 'part2 = ', nstepsminmin
	write(*,*) ''

end subroutine part2

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

