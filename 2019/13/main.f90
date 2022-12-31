
!===============================================================================

module m

	use mintcode_v2
	use utils

	implicit none

#if 0
	character(len = *), parameter :: finput = 'test-input.txt'
	!character(len = *), parameter :: finput = 'test-input2.txt'
	!character(len = *), parameter :: finput = 'test-input3.txt'
#else
	character(len = *), parameter :: finput = 'input.txt'
#endif

	! Every 3 outputs is x pos, y pos, and tile ID
	integer, parameter :: nout = 3

	! Tile IDs
	integer, parameter :: empty = 0, wall = 1, blockt = 2, paddle = 3, ball = 4

contains

!===============================================================================

subroutine part2()

	character :: move

	integer(kind = ick), allocatable :: prog(:), inputs(:)

	type(intcode) :: ic

	integer :: i, nblock, ntile, xmin, xmax, ymin, ymax, x, y, score, &
			xpaddle, xball, it
	integer, allocatable :: outr(:,:), tiles(:,:)
	integer(kind = ick) :: input

	prog = readprog(finput)

	! Memory address 0 represents the number of quarters that have been inserted;
	! set it to 2 to play for free
	prog(1) = 2

	allocate(inputs(0))

	ic = new(prog, inputs)
	ic%debug = 0

	!print *, 'stat = ', ic%stat
	call ic%interpret()

	xball = 0

	! Main game loop
	it = 0
	do while (ic%stat /= finish)
		it = it + 1

		call render()

		write(*,*) 'Enter move (a/o/e = left/neutral/right):'
		write(*,*)
		write(*,*)

		!! Pause for stdin and display
		!read(*,*) move

		! Automatic override: track the paddle's x position
		if (xball > xpaddle) then
			move = 'e'
		else if (xball < xpaddle) then
			move = 'a'
		else
			move = 'o'
		end if

		! Dvorak WASD get fucked qwerty normies
		if (move == 'a') then
			input = -1
		else if (move == 'e') then
			input = +1
		else
			input = 0
		end if

		call ic%set_inputs([ic%inputs, input])
		!print *, 'stat = ', ic%stat
		call ic%interpret()

	end do  ! main game loop

	!print *, 'stat = ', ic%stat

	! We need the final frame with the updated score
	call render()

!********

contains

!********

subroutine render()

	! This is a bit more than just rendering, the automatic paddle moving logic is
	! tied up here

	! It's faster without printing frames, sleep or not
	logical, parameter :: display = .false.

	ntile = ic%io/3
	outr = reshape(ic%outputs(0: ic%io-1), [nout, ntile])

	xmin = minval(outr(1,:))
	xmax = maxval(outr(1,:))
	ymin = minval(outr(2,:))
	ymax = maxval(outr(2,:))

	!print *, 'bounds = ', xmin, xmax, ymin, ymax

	allocate(tiles(xmin: xmax, ymin: ymax))
	tiles = empty

	score = 0
	do i = 1, ntile
		x = outr(1,i)
		y = outr(2,i)
		tiles(x,y) = outr(3,i)

		if (x == -1 .and. y == 0) score = outr(3,i)

		if      (tiles(x,y) == ball) then
			xball = x
		else if (tiles(x,y) == paddle) then
			xpaddle = x
		end if

	end do

	if (display) then

		do y = ymax, ymin, -1
		do x = xmin, xmax

			if      (tiles(x,y) == empty) then
				write(*, '(a)', advance = 'no') ' '
			else if (tiles(x,y) == wall) then
				write(*, '(a)', advance = 'no') '+'
			else if (tiles(x,y) == blockt) then
				write(*, '(a)', advance = 'no') 'x'
			else if (tiles(x,y) == paddle) then
				write(*, '(a)', advance = 'no') '_'
			else if (tiles(x,y) == ball) then
				write(*, '(a)', advance = 'no') 'o'
			end if

		end do
		write(*,*)
		end do
		write(*,*)

		call rsleep(0.04)

	end if

	deallocate(tiles)

	write(*,*) 'part2 score = ', score
	write(*,*) ''

end subroutine render

!********

end subroutine part2

!===============================================================================

subroutine rsleep(dt)

	! sleep() is not only a GNU extension, but it only takes int args!

	real :: dt, start, finish

	call cpu_time(start)
	call cpu_time(finish)
	do while (finish - start < dt)
		call cpu_time(finish)
	end do

end subroutine rsleep

!===============================================================================

subroutine part1()

	integer(kind = ick), allocatable :: prog(:), inputs(:)

	type(intcode) :: ic

	integer :: i, nblock, ntile
	integer, allocatable :: outr(:,:)

	prog = readprog(finput)

	!inputs = [0] ! TODO

	ic = new(prog, inputs)
	call ic%interpret()

	ntile = ic%io/3
	outr = reshape(ic%outputs(0: ic%io-1), [nout, ntile])

	!print *, 'sizes = ', ic%io, size(outr)

	nblock = 0

	!do i = 2, ic%io-1, 3
	!	if (ic%outputs(i) == blockt) nblock = nblock + 1
	!end do

	do i = 1, ntile
		if (outr(3,i) == blockt) nblock = nblock + 1
	end do

	write(*,*) 'part1 = ', nblock
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

