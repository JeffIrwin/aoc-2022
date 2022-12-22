
!===============================================================================

module m

	use utils

	implicit none

#if 0
	character(len = *), parameter :: finput = 'test-input.txt'
#else
	character(len = *), parameter :: finput = 'input.txt'
#endif

	type node
		character(len = :), allocatable :: name
		character :: op
		!type(node) :: left, right
		integer(kind = 8) :: left, right, parent = -1
		integer(kind = 8) :: num
		logical :: leaf = .false.
	end type node

contains

!===============================================================================

function readinput() result(nodes)

	character :: s*256, left*4, right*4, op

	integer :: i, j, iu, io, n

	type(node), allocatable :: nodes(:)

	n = countlines(finput)

	allocate(nodes(n))

	open(file = finput, newunit = iu, status = 'old')

	! First pass: get names
	do i = 1, n
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		!print *, 's = ', trim(s)

		nodes(i)%name = s(1:4)

	end do
	rewind(iu)

	! Second pass, save while mapping names ! to int indices
	do i = 1, n
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		!print *, 's = ', trim(s)

		!nodes(i)%name = s(1:4)

		if (isnum(s(7:7))) then
			nodes(i)%leaf = .true.
			read(s(7:), *) nodes(i)%num
		else

			! Avast ye matey!  Magic numbers be here
			left  = s( 7: 10)
			op    = s(12: 12)
			right = s(14: 17)

			nodes(i)%leaf = .false.
			nodes(i)%op = op

			! O(n^2) but idgaf yolo lmao rofl
			do j = 1, n
				if (nodes(j)%name == left ) nodes(i)%left  = j
				if (nodes(j)%name == right) nodes(i)%right = j
			end do

			nodes( nodes(i)%left  )%parent = i
			nodes( nodes(i)%right )%parent = i

		end if

	end do

	close(iu)

	!print *, 'Tree = '
	!do i = 1, n
	!	if (nodes(i)%leaf) then
	!		print '(a,a,a,i0)', ' ', nodes(i)%name, ': ', nodes(i)%num
	!	else
	!		print *, nodes(i)%name, ': ', nodes(nodes(i)%left )%name, &
	!		                 nodes(i)%op, nodes(nodes(i)%right)%name
	!	end if
	!end do
	!print *, ''

	!print *, 'Parents = '
	!do i = 1, n
	!	print *, nodes(i)%name, ': ', nodes(i)%parent
	!end do
	!print *, ''

end function readinput

!===============================================================================

recursive integer(kind = 8) function evaluate(nodes, i) result(num)

	type(node), allocatable :: nodes(:)

	integer(kind = 8), intent(in) :: i

	integer(kind = 8) :: left, right

	if (nodes(i)%leaf) then
		num = nodes(i)%num
		return
	end if

	left  = evaluate(nodes, nodes(i)%left )
	right = evaluate(nodes, nodes(i)%right)

	if      (nodes(i)%op == '+') then
		num = left + right
	else if (nodes(i)%op == '-') then
		num = left - right
	else if (nodes(i)%op == '*') then
		num = left * right
	else if (nodes(i)%op == '/') then
		num = left / right
	else
		write(*,*) 'Error: invalid operator in evaluate'
		stop
	end if

	!if (nodes(i)%name == 'root') then
	!	! You can plug your solution to part to into the input file(s) for
	!	! the "humn" value to check that the answer results in a match here
	!	print *, 'root left  = ', left
	!	print *, 'root right = ', right
	!end if

end function evaluate

!===============================================================================

recursive integer(kind = 8) function solve(nodes, match, unknown) result(num)

	! Solve the nodes' expression tree for unknown, given that the left
	! and right nodes of the match node are equal

	type(node), allocatable :: nodes(:)

	integer(kind = 8), intent(in) :: match, unknown

	character :: op

	integer(kind = 8) :: unknown_parent, bro_node, bro_val, par_val

	logical :: is_left

	! The parent of the unknown is also currently unknown
	unknown_parent = nodes(unknown)%parent

	if (unknown_parent < 0) then
		write(*,*) 'Error: invalid parent'
		stop
	end if

	! Bro, do you even have a sibling node?
	if (nodes(unknown_parent)%left == unknown) then

		! This unknown is left, bro is right
		is_left = .true.
		bro_node = nodes(unknown_parent)%right

	else
		is_left = .false.
		bro_node = nodes(unknown_parent)%left
	end if

	if (bro_node < 0) then
		write(*,*) 'Error: invalid sibling node'
		stop
	end if

	! Sibling value does not depend on any unknowns, so we can just
	! evaluate instead of solving
	bro_val = evaluate(nodes, bro_node)

	if (unknown_parent == match) then

		! Base case: current unknown matches sibling numeric value
		num = bro_val
		return

	end if

	! The time for al-jabr has come

	! Recursively solve for parent
	par_val = solve(nodes, match, unknown_parent)

	! Get the parent's operator
	op = nodes(unknown_parent)%op

	! Commutative operators are easier
	if      (op == '+') then
		num = par_val - bro_val
	else if (op == '*') then
		num = par_val / bro_val

	! Non-commutative operators depend on whether unknown is left or right
	else if (op == '-') then

		if (is_left) then
			! par = num - bro
			num = par_val + bro_val
		else
			! par = bro - num
			num = bro_val - par_val
		end if

	else if (op == '/') then

		if (is_left) then
			! par = num / bro
			num = par_val * bro_val
		else
			! par = bro / num
			num = bro_val / par_val
		end if

	else
		write(*,*) 'Error: invalid operator in solve'
		stop

	end if

end function solve

!===============================================================================

subroutine part2()

	integer(kind = 8) :: j, isum, root, humn

	type(node), allocatable :: nodes(:)

	isum = 0

	nodes = readinput()

	! Get root
	do j = 1, size(nodes)
		if (nodes(j)%name == 'root') root = j
	end do

	! Get "humn" (human).  This is getting WET
	do j = 1, size(nodes)
		if (nodes(j)%name == 'humn') humn = j
	end do

	!print *, 'root = ', root
	!print *, 'humn = ', humn

	isum = solve(nodes, root, humn)

	write(*,*) 'part2 = ', isum
	write(*,*) ''

end subroutine part2

!===============================================================================

subroutine part1()

	integer(kind = 8) :: j, isum, root

	type(node), allocatable :: nodes(:)

	isum = 0

	nodes = readinput()

	! Get root
	do j = 1, size(nodes)
		if (nodes(j)%name == 'root') root = j
	end do

	isum = evaluate(nodes, root)

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

