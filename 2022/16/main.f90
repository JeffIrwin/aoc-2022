
!===============================================================================

module m

	use utils

	implicit none

#if 0
	character(len = *), parameter :: finput = 'test-input.txt'
#else
	character(len = *), parameter :: finput = 'input.txt'
#endif

	! Static graph capacity
	integer, parameter :: nnodemax = 32*32, nedgemax = 8

	! Time before volcano erupts
	integer, parameter :: tmax = 30

	type node
		character(len = :), allocatable :: name
		integer :: rate
	end type node

	type graph

		type(node) :: n(nnodemax)
		integer :: nnode

		! Edge destinations of directed graph and number of edges per node
		integer :: e(nedgemax, nnodemax)
		integer :: ne(nnodemax) = 0

	end type graph

	integer :: scoremax = 0

contains

!===============================================================================

function readinput() result(g)

	character :: s*256
	character(len = :), allocatable :: name

	integer :: i, j, iu, io, is, rate

	type(graph) :: g

	type(string), allocatable :: es(:,:)

	open(file = finput, newunit = iu, status = 'old')

	! Save edges as name strings initially, then make a second pass and
	! map names to int IDs
	allocate(es(nedgemax, nnodemax))

	i = 0
	do
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit
		i = i + 1

		!print *, 's = ', trim(s)

		if (i > nnodemax) then
			write(*,*) 'Error: nnodemax overflow'
			stop
		end if

		is = len('Valve ')
		name = readword(s, is)

		!print *, 'name = ', name

		is = is + len(' has flow rate=')

		rate = readint(s, is)

		!print *, 'rate = ', rate

		g%n(i)%name = name
		g%n(i)%rate = rate

		! Could actually be "valve" or "valves" but trim will take care of
		! the extra space
		is = is + len('; tunnels lead to valve ')

		j = 0
		do while (is < len_trim(s))
			j = j + 1

			name = trim(adjustl(readcsv(s, is)))
			!print *, 'name = ', name

			if (j > nedgemax) then
				write(*,*) 'Error: nedgemax overflow'
				stop
			end if

			es(j,i)%s = name

		end do
		g%ne(i) = j

		!print *, ''

	end do
	g%nnode = i

	close(iu)

	!print *, 'Edge names = '
	!do i = 1, g%nnode
	!	do j = 1, g%ne(i)
	!		write(*, '(a)', advance = 'no') es(j,i)%s//', '
	!	end do
	!	print *, ''
	!end do

	do i = 1, g%nnode
		do j = 1, g%ne(i)
			g%e(j,i) = get_index(g, es(j,i)%s)
		end do
	end do

	print *, 'nnode = ', g%nnode

	print *, 'Edge IDs = '
	do i = 1, g%nnode
		do j = 1, g%ne(i)
			write(*, '(i0,a)', advance = 'no') g%e(j,i), ', '
		end do
		print *, ''
	end do

end function readinput

!===============================================================================

integer function get_index(g, name) result(k)

	! Get the node index k in graph g by the node's name

	type(graph) :: g

	character(len = *) :: name

	k = 1
	do while (name /= g%n(k)%name)
		k = k + 1
		if (k > g%nnode) then
			print *, 'Error: edge "'//name//'" is not in list'
			stop
		end if
	end do

end function get_index

!===============================================================================

function dijkstra(g, src) result(dist)

	! General purpose Dijkstra's algorithm with all edges length 1

	integer :: src, u(1), iv, v, alt
	integer, allocatable :: dist(:), prev(:)

	logical, allocatable :: q(:)

	type(graph) :: g

	allocate(dist(g%nnode))
	allocate(prev(g%nnode))
	allocate(   q(g%nnode))

	! Initialize distance to infinity
	dist = huge(dist)

	prev = 0

	! q is true if a vertex (node) is in it.  Initialize with all vertices
	! (nodes) in q
	q = .true.

	! Initialize source vertex with 0 distance
	dist(src) = 0

	! While q is not empty
	do while (any(q))

		! Vertex in q with min dist
		u = minloc(dist, q)

		! Remove u from q
		q(u(1)) = .false.

		! For each neighbor v of u still in q

		! Search neighbors
		do iv = 1, g%ne(u(1))

			v = g%e(iv, u(1))
			if (.not. q(v)) cycle

			! All graph edge distances are 1
			alt = dist(u(1)) + 1

			if (alt < dist(v)) then
				dist(v) = alt
				prev(v) = u(1)
			end if

		end do

	end do

	!! prev array can be used to trace the reverse route to a given
	!! destination from src
	!print *, 'prev = ', prev

end function dijkstra

!===============================================================================

subroutine part1()

	! Brute-force by visiting positive-rate valves in every possible
	! permutation of orders

	integer :: isum, i, i0, i00, j, level, score, nopen, t, i1(1), nn, &
			plen, isummax
	integer(kind = 8) :: k

	integer, allocatable :: dist(:), scores(:), perm(:), dists(:,:)

	type(graph) :: g

	logical :: next

	! Valve states (opened or closed)
	logical, allocatable :: o(:)

	isummax = 0

	g = readinput()

	nn = g%nnode

	! Mark rate 0 valves as open, since they can't increase the score
	! anyway
	allocate(o(nn))
	where (g%n(1:nn)%rate == 0)
		o = .true.
	elsewhere
		o = .false.
	end where
	!o(g%n%rate == 0) = .true.

	!print *, 'o = ', o

	! Start at valve AA (not necessarily valve 1)
	i00 = get_index(g, 'AA')

	!print *, 'i00 = ', i00

	! Initial permutation and it's length
	plen = count(.not. o)
	allocate(perm(plen))

	j = 0
	do i = 1, nn
		if (.not. o(i)) then
			j = j + 1
			perm(j) = i
		end if
	end do

	!print *, 'perm = ', perm

	! Potential scores for each node, accounting for its flow rate and how
	! much time is left after travelling to that node
	allocate(scores(nn))

	! Get shortest distances between every pair of nodes ahead of time
	allocate(dists(nn, nn))
	do i = 1, nn
		! From source node i
		dists(:,i) = dijkstra(g, i)
	end do

	! Loop through permutations
	k = 0
	next = .true.
	do while (next)
		k = k + 1

		if (mod(k, 5000000) == 0) then
			print *, 'k = ', k
			!print *, 'perm = ', perm
		end if

		i0 = i00

		isum = 0

		! Time
		t = 0

		do i = 1, plen
			!print *, 'time t = ', t

			! Get distances
			dist = dists(:,i0)
			!dist = dijkstra(g, i0)

			!print *, 'dist = ', dist

			i1 = perm(i)

			! Travel to it
			i0 = i1(1)

			!scores = g%n(1:nn)%rate * (tmax - t - dist)
			!scores = g%n(1:nn)%rate * (tmax - t - dist - 1)
			score = g%n(i0)%rate * (tmax - t - dist(i0) - 1)

			! Add travelling distance time and opening time (1)
			t = t + dist(i0) + 1

			if (t >= tmax) exit

			! Add score
			isum = isum + score
			!isum = isum + scores(i0)

			!print *, ''

		end do

		isummax = max(isummax, isum)

		next = next_perm(perm)
	end do

	write(*,*) 'part1 = ', isummax
	write(*,*) ''

end subroutine part1

!===============================================================================

subroutine part1heu()

	integer :: isum, i, i0, level, score, nopen, t, i1(1), nn
	integer, allocatable :: dist(:), scores(:)

	type(graph) :: g

	! Valve states (opened or closed)
	logical, allocatable :: o(:)

	isum = 0

	g = readinput()

	nn = g%nnode

	! Mark rate 0 valves as open, since they can't increase the score
	! anyway
	allocate(o(nn))
	where (g%n(1:nn)%rate == 0)
		o = .true.
	elsewhere
		o = .false.
	end where
	!o(g%n%rate == 0) = .true.

	print *, 'o = ', o

	! Start at valve AA (not necessarily valve 1)
	i0 = get_index(g, 'AA')

	print *, 'i0 = ', i0

	!level = 0
	!score = 0
	!nopen = count(o(1: nn))
	!call dfs(g, o, nopen, i0, level, score)

	! Time
	t = 0

	! Potential scores for each node, accounting for its flow rate and how
	! much time is left after travelling to that node
	allocate(scores(nn))

	! At each time step, use Dijkstra's algorithm to get the distances
	! from the current valve to all other valves.  Pick the one that can
	! give us the maximal flow from the time that it takes to travel there
	! and open it until tmax.  This is a decent heuristic, but it's not
	! optimal, releasing 1595 pressure for the example instead of 1651.
	! For example, it might be better to go to a low rate valve at one step
	! if it can also get us closer to a high rate valve for the step
	! afterwards, but Dijkstra can't look ahead 2 steps like that.

	do while (t < tmax .and. .not. all(o))
		print *, 'time t = ', t

		! Get distances
		dist = dijkstra(g, i0)

		print *, 'dist = ', dist

		!scores = g%n(1:nn)%rate * (tmax - t - dist)
		scores = g%n(1:nn)%rate * (tmax - t - dist - 1)

		print *, 'scores = ', scores
		print *, 'o = ', o

		! Choose the unopened valve with the max possible score
		i1 = maxloc(scores, .not. o)

		! Travel to it
		i0 = i1(1)

		print *, 'MOVE TO VALVE ', g%n(i0)%name

		! Mark open
		o(i0) = .true.

		! Add travelling distance time and opening time (1)
		t = t + dist(i0) + 1

		! Add score
		isum = isum + scores(i0)

		print *, ''

	end do

	write(*,*) 'part1 (heuristic) = ', isum
	write(*,*) ''

end subroutine part1heu

!===============================================================================

recursive subroutine dfs(g, o, nopen, i0, level, score)

	type(graph) :: g

	integer :: i, i0, j, l, level, score, scorel, rate, add, nopen, nol

	logical :: o(nnodemax), ol(nnodemax)

	l = level + 1

	! Out of time
	if (l >= tmax) then
		scoremax = max(scoremax, score)
		return
	end if

	scorel = score
	i = i0
	nol = nopen

	! Each branch of the search has a separate copy of the open valve
	! state o
	ol = o

	!print *, 'o = ', o(1: g%nnode)
	!print *, 'i0 = ', i0
	!print *, 'ne = ', g%ne(i0)

	! We don't necessarily have to open every valve we visit, it takes an
	! extra minute.  Include this option in the search, unless the rate is
	! 0 in which case there's no point

	rate = g%n(i)%rate
	!if (.not. ol(i) .and. rate > 0) then
	if (.not. ol(i)) then

		!print *, 'opening ', g%n(i)%name

		ol(i) = .true.
		nol = nol + 1

		add = rate * (tmax - l)
		scorel = scorel + add
		!print *, 'adding ', add, ' at valve ', g%n(i)%name, ' at time ', l

		! Return early if all valves open
		if (nol >= g%nnode) then
		!if (all(ol(1: g%nnode))) then
			!print *, 'ALL OPEN RETURN'

			scoremax = max(scoremax, scorel)
			return

		end if

		call dfs(g, ol, nol, i, l, scorel)

		! Traverse after opening
		do j = 1, g%ne(i0)
			i = g%e(j,i0)
			call dfs(g, ol, nol, i, l, scorel)
		end do

	end if

	! Traverse without opening.  Search all edges, cycles are allowed
	do j = 1, g%ne(i0)
		i = g%e(j,i0)
		call dfs(g, ol, nol, i, l, scorel)
	end do

end subroutine dfs

!===============================================================================

subroutine part1dfs()

	integer :: isum, i0, level, score, nopen

	type(graph) :: g

	! Valve states (opened or closed)
	logical :: o(nnodemax) = .false.

	isum = 0

	g = readinput()

	! Mark rate 0 valves as open, since they can't increase the score
	! anyway

	where (g%n%rate == 0)
		o = .true.
	end where
	!o(g%n%rate == 0) = .true.

	print *, 'o = ', o(1: g%nnode)

	! Start at valve AA (not necessarily valve 1)
	i0 = get_index(g, 'AA')

	print *, 'i0 = ', i0

	level = 0
	score = 0
	nopen = count(o(1: g%nnode))

	! Use DFS to check all possible branches and get the optimal solution.
	! I think this brute force approach is correct, but it takes way too
	! long even for the example at 30 steps.

	call dfs(g, o, nopen, i0, level, score)

	write(*,*) 'part1 (dfs) = ', scoremax
	write(*,*) ''

end subroutine part1dfs

!===============================================================================

end module m

!===============================================================================

program main

	use m

	write(*,*) 'Starting AOC main'
	write(*,*) ''

	!call part1dfs()
	!call part1heu()
	call part1()
	!call part2()

	write(*,*) 'Ending AOC main'
	write(*,*) ''

end program main

!===============================================================================

