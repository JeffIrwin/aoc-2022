
!===============================================================================

module m

	use intcode
	use utils

	implicit none

#if 0
	character(len = *), parameter :: finput = 'test-input.txt'
#else
	character(len = *), parameter :: finput = 'input.txt'
#endif

contains

!===============================================================================

subroutine part2()

	character :: s*256

	integer :: iu, io, isum
	integer, allocatable :: prog(:), inputs(:), outputs(:)

	isum = 0

	!prog = readprog(finput)
	call readprog(finput, prog)
	print *, 'prog(0) = ', prog(0)

	allocate(inputs(0:0))
	inputs(0) = 5

	call interpret(prog, inputs, outputs)

	write(*,*) 'part2 = ', isum
	write(*,*) ''

end subroutine part2

!===============================================================================

subroutine part1()

	character :: s*256

	integer :: iu, io, isum
	integer, allocatable :: prog(:), inputs(:), outputs(:)

	isum = 0

	!prog = readprog(finput)
	call readprog(finput, prog)
	print *, 'prog(0) = ', prog(0)

	allocate(inputs(0:0))
	inputs(0) = 1

	call interpret(prog, inputs, outputs)

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

