
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

contains

!===============================================================================

subroutine bfs1(prog0)

	integer(kind = ick) :: prog0(:)

	!********

	integer, parameter :: qmax = 1024, xmax = 512

	integer :: ql, qr, x, y, u(nd), v(nd), w(nd), iw, i
	integer, allocatable :: q(:,:), parent(:,:)
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

	! Begin search at origin as root
	x = 0
	y = 0

	! Label root as explored 
	l(x,y) = .true.

	! Enqueue the root (on the right end of the queue)
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
				! but idgaf
				moves = [iw]
				u = v
				do while (parent(u(1),u(2)) > 0)
					moves = [int(parent(u(1),u(2)),ick), moves]
					u = u - dirs(:, parent(u(1),u(2)))
				end do

				!print *, 'w = ', w
				!print *, 'moves = ', moves

				! Label w as explored (whether it's reachable or not)
				l(w(1), w(2)) = .true.

				! Restart the interpreter from the beginning for each location
				ic = new(prog0, moves)
				call ic%interpret()
				!print *, 'out = ', ic%outputs(0: ic%io-1)
				!print *, ''

				! Stop if we hit a wall on the last move.  We don't need to check
				! earlier moves because they wouldn't have been enqueued
				if (ic%outputs(ic%io-1) == wall) cycle

				! Return when oxygen found
				if (ic%outputs(ic%io-1) == found) then
					write(*,*) 'part1 = ', size(moves)
					return
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

subroutine part1()

	integer(kind = ick), allocatable :: prog(:)

	prog = readprog(finput)

	call bfs1(prog)

	!! TODO: get a return val from bfs1
	!write(*,*) 'part1 = ', ic%outputs(0: ic%io-1)
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
	!call part2()

	write(*,*) 'Ending AOC main'
	write(*,*) ''

end program main

!===============================================================================

