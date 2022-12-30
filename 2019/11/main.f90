
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

	! Panel colors
	integer, parameter :: black = 0, white = 1

	! Cardinal directions.  c.f. 2022 day 12.  y is positive up
	integer, parameter :: nd = 2, ndirs = 4
	integer, parameter :: dirs(nd,ndirs) = &
		reshape([  &
			  0, +1, &
			 +1,  0, &
			  0, -1, &
			 -1,  0  &
		], [nd,ndirs])
	integer, parameter :: up = 1, right = 2, down = 3, left = 4

contains

!===============================================================================

subroutine part2()

	integer(kind = ick), allocatable :: prog(:), inputs(:)

	type(intcode) :: ic

	! Grid half size
	integer, parameter :: nhalf = 512

	integer :: color  (-nhalf: nhalf, -nhalf: nhalf), i, x, y, dir, &
			xmin, xmax, ymin, ymax
	integer(kind = ick) :: input, out1, out2
	logical :: painted(-nhalf: nhalf, -nhalf: nhalf)

	prog = readprog(finput)

	x = 0
	y = 0
	dir = up

	! All of the panels are initially black except for 1
	color = black
	color(x,y) = white

	painted = .false.

	allocate(inputs(0))
	ic = new(prog, inputs)

	i = 0
	do while (ic%stat /= finish)
		i = i + 1

		input = color(x, y)
		call ic%set_inputs([ic%inputs, input])

		call ic%interpret()

		out1 = ic%outputs(ic%io - 2)
		out2 = ic%outputs(ic%io - 1)

		!print *, 'out1, out2 = ', out1, out2

		color(x,y) = out1
		painted(x,y) = .true.

		if (out2 == 0) then
			! Turn left
			dir = wrap(dir - 1, ndirs)
		else if (out2 == 1) then
			! Turn right
			dir = wrap(dir + 1, ndirs)
		else
			write(*,*) 'Error: unexpected turn output:', out2
			stop
		end if

		x = x + dirs(1,dir)
		y = y + dirs(2,dir)

		!if (i == 10) exit

	end do

	! Find bounds of white area

	xmin = -nhalf
	do while (all(color(xmin, :) == black))
		xmin = xmin + 1
	end do

	xmax = +nhalf
	do while (all(color(xmax, :) == black))
		xmax = xmax - 1
	end do

	ymin = -nhalf
	do while (all(color(:, ymin) == black))
		ymin = ymin + 1
	end do

	ymax = +nhalf
	do while (all(color(:, ymax) == black))
		ymax = ymax - 1
	end do

	!print *, 'bounds = ', xmin, xmax, ymin, ymax

	write(*,*) 'part2 = '

	do y = ymax, ymin, -1
		do x = xmin, xmax
			if (color(x,y) == black) then
				write(*, '(a)', advance = 'no') ' '
			else
				write(*, '(a)', advance = 'no') '#'
			end if
		end do
		write(*,*) ''
	end do
	write(*,*) ''

end subroutine part2

!===============================================================================

subroutine part1()

	integer(kind = ick), allocatable :: prog(:), inputs(:)

	type(intcode) :: ic

	! Grid half size
	integer, parameter :: nhalf = 512

	integer :: color  (-nhalf: nhalf, -nhalf: nhalf), i, x, y, dir
	integer(kind = ick) :: input, out1, out2
	logical :: painted(-nhalf: nhalf, -nhalf: nhalf)

	prog = readprog(finput)

	! All of the panels are initially black
	color = black
	painted = .false.

	x = 0
	y = 0
	dir = up

	allocate(inputs(0))
	ic = new(prog, inputs)

	i = 0
	do while (ic%stat /= finish)
		i = i + 1

		input = color(x, y)
		call ic%set_inputs([ic%inputs, input])

		call ic%interpret()

		out1 = ic%outputs(ic%io - 2)
		out2 = ic%outputs(ic%io - 1)

		!print *, 'out1, out2 = ', out1, out2

		color(x,y) = out1
		painted(x,y) = .true.

		if (out2 == 0) then
			! Turn left
			dir = wrap(dir - 1, ndirs)
		else if (out2 == 1) then
			! Turn right
			dir = wrap(dir + 1, ndirs)
		else
			write(*,*) 'Error: unexpected turn output:', out2
			stop
		end if

		x = x + dirs(1,dir)
		y = y + dirs(2,dir)

		!if (i == 10) exit

	end do

	write(*,*) 'part1 = ', count(painted)
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

