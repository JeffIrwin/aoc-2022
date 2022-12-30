
!===============================================================================

module m

	!use intcode
	use mintcode_v2

	use utils

	implicit none

#if 0
	character(len = *), parameter :: finput  = 'test-input.txt'
	character(len = *), parameter :: finput2 = 'test-input2.txt'
#else
	character(len = *), parameter :: finput  = 'input.txt'
	character(len = *), parameter :: finput2 = 'input.txt'
#endif

contains

!===============================================================================

subroutine part2()

	integer, parameter :: namps = 5

	integer :: i, maxout
	integer, allocatable :: prog0(:), inputs(:), outputs(:), &
			phases(:), proga(:), progb(:), progc(:), progd(:), proge(:)

	logical :: next

	!print *, 'starting part2'

	!call readprog(finput2, prog0)

	!maxout = -huge(maxout)

	!phases = [(i, i = 5, 2*namps - 1)]
	!next = .true.
	!do while (next)
	!	!print *, 'phases = ', phases

	!	! Amplifier A
	!	inputs = [phases(1), 0]
	!	proga = prog0
	!	call interpret(proga, inputs, outputs)

	!	! Amplifier B
	!	inputs = [phases(2), outputs(1)]
	!	progb = prog0
	!	call interpret(progb, inputs, outputs)

	!	! Amplifier C
	!	inputs = [phases(3), outputs(1)]
	!	progc = prog0
	!	call interpret(progc, inputs, outputs)

	!	! Amplifier D
	!	inputs = [phases(4), outputs(1)]
	!	progd = prog0
	!	call interpret(progd, inputs, outputs)

	!	! Amplifier E
	!	inputs = [phases(5), outputs(1)]
	!	proge = prog0
	!	call interpret(proge, inputs, outputs)

	!	maxout = max(maxout, outputs(1))

	!	next = next_perm(phases)

	!	! TODO
	!	exit

	!end do

	!write(*,*) 'part2 = ', maxout
	!write(*,*) ''

end subroutine part2

!===============================================================================

subroutine part1()

	integer, parameter :: namps = 5

	integer :: i, maxout
	integer, allocatable :: prog0(:), outputs(:), phases(:)

	logical :: next

	type(intcode) :: pa, pb, pc, pd, pe

	call readprog(finput, prog0)

	!inputs = [6,4] ! TODO
	!call interpret(prog, inputs, outputs)

	maxout = -huge(maxout)

	phases = [(i, i = 0, namps - 1)]
	next = .true.
	do while (next)
		!print *, 'phases = ', phases

		! Amplifier A
		pa = new(prog0, [phases(1), 0])
		call interpret(pa, outputs)

		! Amplifier B
		pb = new(prog0, [phases(2), outputs(1)])
		call interpret(pb, outputs)

		! Amplifier C
		pc = new(prog0, [phases(3), outputs(1)])
		call interpret(pc, outputs)

		! Amplifier D
		pd = new(prog0, [phases(4), outputs(1)])
		call interpret(pd, outputs)

		! Amplifier E
		pe = new(prog0, [phases(5), outputs(1)])
		call interpret(pe, outputs)

		maxout = max(maxout, outputs(1))

		next = next_perm(phases)

		! TODO
		!exit

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
	call part2()

	write(*,*) 'Ending AOC main'
	write(*,*) ''

end program main

!===============================================================================

