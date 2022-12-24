
!===============================================================================

module m

	use utils

	implicit none

#if 0
	character(len = *), parameter :: finput = 'test-input.txt'
#else
	character(len = *), parameter :: finput = 'input.txt'
#endif

	! Max number of collocated blizzards
	integer, parameter :: nbmax = 4

	integer, parameter :: expedition = -2, wall = -1, empty = 0

	! Cardinal directions.  c.f. day 12.  Note y is positive down!
	integer, parameter :: nd = 2, ndirs = 4
	integer, parameter :: dirs(nd,ndirs) = &
		reshape([  &
			  0, -1, &
			  0, +1, &
			 -1,  0, &
			 +1,  0  &
		], [nd,ndirs])

	! Number of spacetime dimensions
	integer, parameter :: nst = nd + 1

	integer, parameter :: up = 1, down = 2, left = 3, right = 4

	! I'm making these global variables because I'm lazy
	integer :: start(nd), end(nd), levelmin = -1, levelmax = 30!1000
	integer, parameter :: tmax = 2048!512!256

contains

!===============================================================================

subroutine printmap(map, nmap, expe)

	integer :: map(:,:,:), nmap(:,:), expe(nd)

	integer :: x, y, nx, ny

	nx = size(nmap, 1)
	ny = size(nmap, 2)

	do y = 1, ny
		do x = 1, nx

			if (nmap(x,y) > 1) then
				write(*, '(i1)', advance = 'no') nmap(x,y)
				cycle
			end if

			if (all([x,y] == expe)) then
				write(*, '(a)', advance = 'no') 'E'

			!if      (map(1,x,y) == wall) then
			else if ((x == 1 .or. x == nx .or. y == 1 .or. y == ny) &
					.and. .not. (all([x,y] == start))              &
					.and. .not. (all([x,y] == end  ))) then
				write(*, '(a)', advance = 'no') '#'

			else if (map(1,x,y) == up) then
				write(*, '(a)', advance = 'no') '^'
			else if (map(1,x,y) == down) then
				write(*, '(a)', advance = 'no') 'v'
			else if (map(1,x,y) == left) then
				write(*, '(a)', advance = 'no') '<'
			else if (map(1,x,y) == right) then
				write(*, '(a)', advance = 'no') '>'
			else! if (map(1,x,y) == empty) then
				write(*, '(a)', advance = 'no') '.'
			end if

		end do
		write(*,*)
	end do
	write(*,*)

end subroutine printmap

!===============================================================================

subroutine move_blizzards(map, nmap)

	integer :: map(:,:,:), nmap(:,:), m0
	integer, allocatable :: map0(:,:,:), nmap0(:,:)

	integer :: i0, x0, y0, i, x, y, nx, ny

	nx = size(nmap, 1)
	ny = size(nmap, 2)

	! Backup current iteration before overwriting with next iteration
	nmap0 = nmap
	 map0 =  map

	! Clear
	nmap = 0
	map = empty

	!! Unit testing :)
	!print *, 'w0 = ', wrap(0, 2, 7)
	!print *, 'w1 = ', wrap(1, 2, 7)
	!print *, 'w2 = ', wrap(2, 2, 7)
	!print *, 'w3 = ', wrap(3, 2, 7)
	!print *, 'w4 = ', wrap(4, 2, 7)
	!print *, 'w5 = ', wrap(5, 2, 7)
	!print *, 'w6 = ', wrap(6, 2, 7)
	!print *, 'w7 = ', wrap(7, 2, 7)
	!print *, 'w8 = ', wrap(8, 2, 7)

	! Move each blizzard in its direction
	do y0 = 1, ny
		do x0 = 1, nx
			do i0 = 1, nmap0(x0,y0)

				m0 = map0(i0,x0,y0)

				!if (m0 <= 0) then
				!	! Expedition, wall, and empty stay in the same [x,y] location
				!	x = x0
				!	y = y0
				!	i = 1
				!else
				if (m0 > 0) then

					! Blizzard.  Assume walls at 1, nx, and ny, and no blizzards
					! ever go to start/end locations
					x = wrap(x0 + dirs(1,m0), 2, nx-1)
					y = wrap(y0 + dirs(2,m0), 2, ny-1)

					i = nmap(x,y) + 1
					nmap(x,y) = i
					map(i, x, y) = m0

				else if (m0 == wall) then
					!print *, 'copy wall'
					map(1, x0, y0) = m0
					nmap(x0,y0) = 1

				! No need to copy empty locations, already initialized above

				end if

				!i = nmap(x,y) + 1
				!nmap(x,y) = i
				!map(i, x, y) = m0

			end do
		end do
	end do

	! Expedition position is stored outside of map, so there's no need to
	! set it here

end subroutine move_blizzards

!===============================================================================

subroutine readinput(map, nmap)

	character :: s*256

	integer :: iu, io, nx, ny, x, y
	integer, allocatable :: map(:,:,:), nmap(:,:)

	ny = countlines(finput)

	open(file = finput, newunit = iu, status = 'old')

	read(iu, '(a)', iostat = io) s
	nx = len_trim(s)
	rewind(iu)

	!print *, 'nx, ny = ', nx, ny

	allocate(map(nbmax, nx, ny), nmap(nx, ny))
	map = empty

	! Number of collocated blizzards at each map location
	nmap = 1

	do y = 1, ny
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		!print *, 's = ', trim(s)

		do x = 1, nx

			if      (s(x:x) == '#') then
				map(1,x,y) = wall
			else if (s(x:x) == '.') then
				map(1,x,y) = empty
			else if (s(x:x) == '^') then
				map(1,x,y) = up
			else if (s(x:x) == 'v') then
				map(1,x,y) = down
			else if (s(x:x) == '<') then
				map(1,x,y) = left
			else if (s(x:x) == '>') then
				map(1,x,y) = right
			else
				write(*,*) 'Error parsing map'
				stop
			end if

		end do

	end do

	close(iu)

	! TODO: initialize expedition location

	! Hard coded because I'm lazy
	start = [   2,  1]
	end   = [nx-1, ny]

end subroutine readinput

!===============================================================================

function get_map3(map, nmap, tmax) result(map3)

	! Get a spacetime map of the blizzards' motion ahead of time

	integer, intent(in) :: map(:,:,:), nmap(:,:), tmax

	integer, allocatable :: mapl(:,:,:), nmapl(:,:)

	integer :: i, x, y, nx, ny

	! Could be 1-byte to save RAM if needed
	integer, allocatable :: map3(:,:,:)

	!print *, 'starting get_map3'

	allocate(map3(size(nmap, 1), size(nmap, 2), tmax))
	map3 = empty

	mapl = map
	nmapl = nmap

	!print *, 'mapl 1 = ', mapl(1,:,:)

	! Time loop
	do i = 1, tmax

		! We only need to save the first sheet of 2D map.  We don't care about
		! multiple blizzards being collocated
		map3(:,:,i) = mapl(1,:,:)
		call move_blizzards(mapl, nmapl)

	end do

	!! Print, debug only
	!nx = size(nmap, 1)
	!ny = size(nmap, 2)
	!do i = 1, 3
	!print *, 'mapl ', i, ' = ', mapl(1,:,:)
	!do y = 1, ny
	!	do x = 1, nx
	!		if      (map3(x,y,i) == wall) then
	!			write(*, '(a)', advance = 'no') '#'
	!		else if (map3(x,y,i) == up) then
	!			write(*, '(a)', advance = 'no') '^'
	!		else if (map3(x,y,i) == down) then
	!			write(*, '(a)', advance = 'no') 'v'
	!		else if (map3(x,y,i) == left) then
	!			write(*, '(a)', advance = 'no') '<'
	!		else if (map3(x,y,i) == right) then
	!			write(*, '(a)', advance = 'no') '>'
	!		else! if (map3(...) == empty) then
	!			write(*, '(a)', advance = 'no') '.'
	!		end if
	!	end do
	!	write(*,*)
	!end do
	!write(*,*)
	!end do
	!write(*,*)

end function get_map3

!===============================================================================

function dijkstra(map3, xyt0, xyend) result(dist)

	! c.f. day 12

	integer, intent(in) :: map3(:,:,:), xyt0(nst)

	!********

	integer :: nx, ny, nt, u(nst), v(nst), nlocs, alt, i, iv, x, y, &
			xyend(nd)
	integer, allocatable :: dist(:,:,:), prev(:,:,:,:), locs(:,:)

	logical, allocatable :: q(:,:,:)

	!print *, 'starting dijkstra'
	!print *, 'source xyt0 =', xyt0
	!print *, 'end xy      = ', xyend

	! Consider waiting in current location and search 4 neighbors
	nlocs = ndirs + 1
	allocate(locs(nd, nlocs))
	do i = 1, ndirs
		locs(:,i) = dirs(:,i)
	end do
	locs(:, nlocs) = [0,0]

	!print *, 'nlocs = ', nlocs
	!print *, 'locs = '
	!print '(2i3)', locs

	nx = size(map3, 1)
	ny = size(map3, 2)
	nt = size(map3, 3)

	!print *, 'nx, ny = ', nx, ny

	allocate(dist(nx, ny, nt))
	allocate(prev(nst, nx, ny, nt))
	allocate(q   (nx, ny, nt))

	prev = 0

	! Initialize distance to infinity.  Huge is causing numerical problems
	dist = 2 * nt !huge(dist)

	! q is true if a vertex (node) is in it.  Initialize with all vertices
	! in q
	q = .true.

	! Initialize source vertex with 0 distance
	dist(xyt0(1), xyt0(2), xyt0(3)) = 0

	! While q is not empty
	do while (any(q))

		! Vertex in q with min dist
		u = minloc(dist, q)

		! Early return.  Is this optimal though?  Works for part1 :shrug:
		if (all(u(1:nd) == xyend)) return

		! Remove u from q
		q(u(1), u(2), u(3)) = .false.

		! For each neighbor v of u still in q

		! Search spacetime neighbors
		do iv = 1, nlocs

			v(1:nd) = u(1:nd) + locs(:,iv) ! space location
			v(nst) = u(nst) + 1 ! time always increases by 1

			! Check space bounds
			if (v(1) < 1 .or. v(1) > nx .or. v(2) < 1 .or. v(2) > ny) cycle

			! Check somewhat arbitrary upper time bound
			if (v(nst) > nt) cycle

			! v still in q
			if (.not. q(v(1), v(2), v(3))) cycle

			! Check location not blocked by wall or blizzards
			if (map3(v(1), v(2), v(3)) /= empty) cycle

			! All graph edge distances are 1
			alt = dist(u(1), u(2), u(3)) + 1

			if (alt < dist(v(1), v(2), v(3))) then
				dist(v(1), v(2), v(3)) = alt

				prev(:, v(1), v(2), v(3)) = u

			end if

		end do
	end do

	!do i = 1, 20
	!	write(*,*) 'Time ', i
	!	do y = 1, ny
	!		do x = 1, nx
	!			write(*, '(i3)', advance = 'no') dist(x,y,i)
	!		end do
	!		write(*,*)
	!	end do
	!	write(*,*)
	!end do
	!print *, 'finished dijkstra'

end function dijkstra

!===============================================================================

integer function dist_end(dist, end, t0)

	integer :: i, end(nd), t0
	integer :: dist(:,:,:)

	! Find the first time index from t0 when the end location is reachable.  This
	! is actually off-by-one after the first time but idgaf

	i = t0
	do while (abs(dist(end(1), end(2), i)) >= tmax)
		i = i + 1
		if (i > tmax) then
			write(*,*) 'Error: end unreachable.  Increase maximum time'
			stop
		end if
	end do
	dist_end = dist(end(1), end(2), i)

	!print *, 'dist_end = ', dist_end

end function dist_end

!===============================================================================

subroutine part2()

	! Use Dijkstra in 3D with regular valley map x, y, and time as the 3rd
	! dimension.  Blizzards move deterministically, so this can be done

	integer :: i, isum, x, y, t, expe(nst)!, dist
	integer, allocatable :: map(:,:,:), nmap(:,:), map3(:,:,:), dist(:,:,:)

	isum = 0

	call readinput(map, nmap)

	! Initialize expedition location at start location and start time 1
	t = 1
	expe = [start, t]

	!print *, 'expe = ', expe
	!call printmap(map, nmap, expe)

	map3 = get_map3(map, nmap, tmax)

	!********

	! There
	dist = dijkstra(map3, expe, end)
	isum = isum + dist_end(dist, end, t)
	t = isum

	! Back
	expe = [end, isum]
	dist = dijkstra(map3, expe, start)
	isum = isum + dist_end(dist, start, t) - 1
	t = isum

	! There again
	expe = [start, isum]
	dist = dijkstra(map3, expe, end)
	isum = isum + dist_end(dist, end, t) - 1
	t = isum

	write(*,*) 'part2 = ', isum
	write(*,*) ''

end subroutine part2

!===============================================================================

subroutine part1()

	! Use Dijkstra in 3D with regular valley map x, y, and time as the 3rd
	! dimension.  Blizzards move deterministically, so this can be done

	integer :: i, isum, x, y, expe(nst)!, dist
	integer, allocatable :: map(:,:,:), nmap(:,:), map3(:,:,:), dist(:,:,:)

	integer, parameter :: tmax = 512!256

	isum = 0

	call readinput(map, nmap)

	! Initialize expedition location at start location and start time 1
	expe = [start, 1]

	!print *, 'expe = ', expe
	!call printmap(map, nmap, expe)

	map3 = get_map3(map, nmap, tmax)

	dist = dijkstra(map3, expe, end)

	!do i = 1, 20
	!	write(*,*) 'Time ', i
	!	do y = 1, size(nmap, 2)
	!		do x = 1, size(nmap, 1)
	!			write(*, '(i3)', advance = 'no') dist(x,y,i)
	!		end do
	!		write(*,*)
	!	end do
	!	write(*,*)
	!end do

	!print *, 'dist(end) = ', dist(end(1), end(2), 1:20)

	! Find the first time index when the end location is reachable
	i = 1
	do while (abs(dist(end(1), end(2), i)) >= tmax)
		i = i + 1
		if (i > tmax) then
			write(*,*) 'Error: end unreachable.  Increase maximum time'
			stop
		end if
	end do
	isum = dist(end(1), end(2), i)

	!print *, 'end, i = ', end, i

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

