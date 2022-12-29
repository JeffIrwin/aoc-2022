
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

	integer, allocatable :: prog(:), inputs(:), outputs(:)

	call readprog(finput, prog)

	inputs = [5]
	call interpret(prog, inputs, outputs)

	write(*,*) 'part2 = ', outputs(ubound(outputs))
	write(*,*) ''

end subroutine part2

!===============================================================================

subroutine part1()

	integer, allocatable :: prog(:), inputs(:), outputs(:)

	call readprog(finput, prog)
	!print *, 'prog(0) = ', prog(0)

	inputs = [1]
	call interpret(prog, inputs, outputs)

	write(*,*) 'part1 = ', outputs(ubound(outputs))
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

