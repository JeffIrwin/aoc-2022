
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

	integer :: nprog, noun, verb
	integer, allocatable :: prog0(:), prog(:), outputs(:)

	call readprog(finput, prog0)
	nprog = size(prog0)
	allocate(prog(0: nprog - 1))

	do noun = 0, 99
	do verb = 0, 99
		!print *, 'noun, verb = ', noun, verb

		! Initialize memory to the progam's values.  TODO: do this inside execute()
		! with a local prog variable?
		prog = prog0(0: nprog - 1)

		! Special instuctions for day 2
		prog(1) = noun
		prog(2) = verb

		! Execute (technically, interpret) opcodes
		call interpret(prog, [0], outputs)

		if (prog(0) == 19690720) then
			!print *, 'found'
			goto 100
		end if

	end do
	end do

100	continue

	write(*,*) 'part2 = ', 100 * noun + verb
	write(*,*) ''

end subroutine part2

!===============================================================================

subroutine part1()

	integer, allocatable :: prog(:), outputs(:)

	call readprog(finput, prog)

	! Special instuctions for day 2
	prog(1) = 12
	prog(2) =  2

	!print *, 'prog = ', prog(0: nprog - 1)

	call interpret(prog, [0], outputs)

	!print *, 'prog = ', prog(0: nprog - 1)

	write(*,*) 'part1 = ', prog(0)
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

