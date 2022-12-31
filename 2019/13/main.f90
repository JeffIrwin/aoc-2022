
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

	integer, parameter :: empty = 0, wall = 1, blockt = 2, paddle = 3, ball = 4

contains

!===============================================================================

subroutine part1()

	integer(kind = ick), allocatable :: prog(:), inputs(:)

	type(intcode) :: ic

	integer :: i, nblock

	prog = readprog(finput)

	!inputs = [0] ! TODO

	ic = new(prog, inputs)
	call ic%interpret()

	nblock = 0
	do i = 2, ic%io-1, 3
		if (ic%outputs(i) == blockt) nblock = nblock + 1
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
	!call part2()

	write(*,*) 'Ending AOC main'
	write(*,*) ''

end program main

!===============================================================================

