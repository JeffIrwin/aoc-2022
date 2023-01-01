
!===============================================================================

module m

	use mintcode_v2
	use utils

	implicit none

#if 0
	character(len = *), parameter :: finput = 'test-input.txt'
#else
	character(len = *), parameter :: finput = 'input.txt'
#endif

	! Cardinal directions.  c.f. 2022 day 12.  y is positive north
	integer, parameter :: nd = 2, ndirs = 4
	integer, parameter :: dirs(nd,ndirs) = &
		reshape([  &
			  0, +1, &
			  0, -1, &
			 -1,  0, &
			 +1,  0  &
		], [nd,ndirs])
	integer, parameter :: north = 1, south = 2, west = 3, east = 4

	! Droid status codes
	integer, parameter :: wall = 0, moved = 1, found = 2

	integer, allocatable :: map(:,:)
	integer :: oxy_steps, oxy_x, oxy_y, stepsmax

	integer, parameter :: qmax = 1024, xmax = 512

contains

!===============================================================================

subroutine bfs1(prog0)

	! Breadth-first-search first pass:  the map is unknown.  Explore and chart the
	! map by querying the droid status

	integer(kind = ick) :: prog0(:)

	!********

	integer :: ql, qr, x, y, u(nd), v(nd), w(nd), iw, i, j, stat
	integer, allocatable :: q(:,:), parent(:,:), dist(:,:)
	integer(kind = ick), allocatable :: moves(:)

	logical, allocatable :: l(:,:)

	type(intcode) :: ic

	! Queue
	allocate(q(nd, qmax))

	! Visited locations
	allocate(l(-xmax: xmax, -xmax: xmax))
	l = .false.

	allocate(parent(-xmax: xmax, -xmax: xmax))
	parent = -1

	allocate(dist(-xmax: xmax, -xmax: xmax))
	dist = -1

	if (allocated(map)) deallocate(map)
	allocate(map(-xmax: xmax, -xmax: xmax))
	map = wall

	! Begin search at origin as root
	x = 0
	y = 0

	! Label root as explored.  Assume we don't start on a wall
	l(x,y) = .true.
	map(x,y) = moved
	dist(x,y) = 0

	! Initialize and enqueue the root (on the right end of the queue)
	ql = 1
	qr = 1
	q(:,qr) = [x,y]

	! While queue is not empty
	i = 0
	do while (ql <= qr)
		i = i + 1

		! Dequeue v from q (from left end)
		v = q(:,ql)
		ql = ql + 1

		! For all edges from v to w
		do iw = 1, ndirs

			w = v + dirs(:,iw)

			if (.not. l(w(1), w(2))) then
				! w is not explored yet

				! Retrace the steps to get to w from last move to first.  This is
				! a terrible way to push things to an array, it amortizes every time,
				! but idgaf.  It wouldn't be too hard to make another array that tracks
				! the distance of each point from start to help pre-allocate here
				u = v
				dist(w(1), w(2)) = dist(u(1), u(2)) + 1
				j = dist(w(1), w(2))
				allocate(moves(j))
				moves(j) = iw
				do while (parent(u(1), u(2)) > 0)
					j = j - 1
					moves(j) =      parent(u(1), u(2))
					u = u - dirs(:, parent(u(1),u(2)))
				end do

				!moves = [iw]
				!do while (parent(u(1),u(2)) > 0)
				!	moves = [int(parent(u(1),u(2)),ick), moves]
				!	u = u - dirs(:, parent(u(1),u(2)))
				!end do

				!print *, 'w = ', w
				!print *, 'moves = ', moves

				! Label w as explored (whether it's reachable or not)
				l(w(1), w(2)) = .true.

				! Restart the interpreter from the beginning for each location
				ic = new(prog0, moves)
				call ic%interpret()
				stat = ic%outputs(ic%io-1)

				deallocate(moves)

				!print *, 'stat = ', stat
				!print *, ''

				map(w(1), w(2)) = stat

				! Stop if we hit a wall on the last move.  We don't need to check
				! earlier moves because they wouldn't have been enqueued
				if (stat == wall) cycle

				! Return when oxygen found
				if (stat == found) then
					!write(*,*) 'part1 = ', size(moves)
					!print *, 'i = ', i
					!oxy_steps = size(moves)
					oxy_steps = dist(w(1), w(2))
					oxy_x = w(1)
					oxy_y = w(2)
					!return
				end if

				! Parent stores the move dir index to get from v to w
				parent(w(1), w(2)) = iw

				! Enqueue w
				qr = qr + 1
				q(:,qr) = [w(1), w(2)]

			end if

		end do

	end do

end subroutine bfs1

!===============================================================================

subroutine bfs2()

	! Breadth-first-search second pass:  the map is now known after being set by
	! bf1().  We can search from any starting point without having to interpret
	! Intcode.
	!
	! There's probably a cleaner way of doing this than having 2 copies of BFS,
	! but idgaf.

	integer :: ql, qr, x, y, u(nd), v(nd), w(nd), iw, i, stat, steps
	integer, allocatable :: q(:,:), parent(:,:)
	integer(kind = ick), allocatable :: moves(:)

	logical, allocatable :: l(:,:)

	type(intcode) :: ic

	!print *, 'starting bfs2()'
	!print *, 'oxy xy = ', oxy_x, oxy_y
	!print *, ''

	! Queue
	allocate(q(nd, qmax))

	! Visited locations
	allocate(l(-xmax: xmax, -xmax: xmax))
	l = .false.

	! Is this required for part2?
	allocate(parent(-xmax: xmax, -xmax: xmax))
	parent = -1

	! Begin search at oxy location found by bfs1()
	x = oxy_x
	y = oxy_y

	! Label root as explored 
	l(x,y) = .true.

	! Initialize and enqueue the root (on the right end of the queue)
	ql = 1
	qr = 1
	q(:,qr) = [x,y]

	stepsmax = -huge(stepsmax)

	! While queue is not empty
	i = 0
	do while (ql <= qr)
		i = i + 1

		! Dequeue v from q (from left end)
		v = q(:,ql)
		ql = ql + 1

		! For all edges from v to w
		do iw = 1, ndirs

			w = v + dirs(:,iw)

			if (.not. l(w(1), w(2)) .and. .not. map(w(1), w(2)) == wall) then
				! w is not explored yet

				! Retrace the steps to get to w from last move to first, just to count
				! the distance from oxy to current loc
				u = v
				steps = 1
				do while (parent(u(1),u(2)) > 0)
					steps = steps + 1
					u = u - dirs(:, parent(u(1),u(2)))
				end do
				stepsmax = max(stepsmax, steps)

				!print *, 'xy, steps = ', w, steps

				! Label w as explored
				l(w(1), w(2)) = .true.

				! Parent stores the move dir index to get from v to w
				parent(w(1), w(2)) = iw

				! Enqueue w
				qr = qr + 1
				q(:,qr) = [w(1), w(2)]

			end if

		end do

	end do

	!! This is not the answer
	!print *, 'i = ', i

end subroutine bfs2

!===============================================================================

subroutine part2()

	integer(kind = ick), allocatable :: prog(:)

	prog = readprog(finput)

	call bfs1(prog)
	call bfs2()

	write(*,*) 'part2 = ', stepsmax
	write(*,*) ''

end subroutine part2

!===============================================================================

subroutine part1()

	integer(kind = ick), allocatable :: prog(:)

	prog = readprog(finput)

	call bfs1(prog)

	write(*,*) 'part1 = ', oxy_steps
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

