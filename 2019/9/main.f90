
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

contains

!===============================================================================

subroutine part2()

	integer(kind = ick), allocatable :: prog(:), inputs(:)

	type(intcode) :: ic

	prog = readprog(finput)

	inputs = [2]

	ic = new(prog, inputs)
	call ic%interpret()

	write(*,*) 'part2 = ', ic%outputs(0: ic%io-1)
	write(*,*) ''

end subroutine part2

!===============================================================================

subroutine part1()

	integer(kind = ick), allocatable :: prog(:), inputs(:)

	type(intcode) :: ic

	prog = readprog(finput)

	inputs = [1]

	ic = new(prog, inputs)
	call ic%interpret()

	write(*,*) 'part1 = ', ic%outputs(0: ic%io-1)
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

