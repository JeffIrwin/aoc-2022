
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

subroutine part1()

	integer, parameter :: namps = 5

	integer :: i, maxout
	integer, allocatable :: prog0(:), prog(:), inputs(:), outputs(:), phases(:)

	logical :: next

	call readprog(finput, prog0)

	!inputs = [6,4] ! TODO
	!call interpret(prog, inputs, outputs)

	maxout = -huge(maxout)

	phases = [(i, i = 0, namps - 1)]
	next = .true.
	do while (next)
		!print *, 'phases = ', phases

		! Amplifier A
		inputs = [phases(1), 0]
		prog = prog0
		call interpret(prog, inputs, outputs)

		! Amplifier B
		inputs = [phases(2), outputs(1)]
		prog = prog0
		call interpret(prog, inputs, outputs)

		! Amplifier C
		inputs = [phases(3), outputs(1)]
		prog = prog0
		call interpret(prog, inputs, outputs)

		! Amplifier D
		inputs = [phases(4), outputs(1)]
		prog = prog0
		call interpret(prog, inputs, outputs)

		! Amplifier E
		inputs = [phases(5), outputs(1)]
		prog = prog0
		call interpret(prog, inputs, outputs)

		maxout = max(maxout, outputs(1))

		next = next_perm(phases)
	end do

	write(*,*) 'part1 = ', maxout
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

