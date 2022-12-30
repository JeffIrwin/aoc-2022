
!===============================================================================

module m

	!use intcode
	use mintcode_v2

	use utils

	implicit none

#if 0
	character(len = *), parameter :: finput  = 'test-input.txt'
	!character(len = *), parameter :: finput2 = 'test-input2.txt'
	character(len = *), parameter :: finput2 = 'test-input3.txt'
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
			phases(:)

	type(intcode) :: pa, pb, pc, pd, pe

	logical :: next

	print *, 'starting part2'

	prog0 = readprog(finput2)

	maxout = -huge(maxout)

	phases = [(i, i = 5, 2*namps - 1)]
	!phases = [9, 8, 7, 6, 5] ! TODO
	!phases = [(i, i = 0, namps - 1)]

	next = .true.
	do while (next)
		print *, 'phases = ', phases

		! First loop: supply initial 0 input and phase settings

		! Amplifier A
		pa = new(prog0, [phases(1), 0])
		pa%debug = 1
		call pa%interpret()

		! Amplifier B
		pb = new(prog0, [phases(2), pa%outputs(0)])
		pb%debug = 1
		call pb%interpret()

		! Amplifier C
		pc = new(prog0, [phases(3), pb%outputs(0)])
		pc%debug = 1
		call pc%interpret()

		! Amplifier D
		pd = new(prog0, [phases(4), pc%outputs(0)])
		pd%debug = 1
		call pd%interpret()

		! Amplifier E
		pe = new(prog0, [phases(5), pd%outputs(0)])
		pe%debug = 1
		call pe%interpret()

		do while (pe%stat /= finish)

			print *, '********'
			print *, 'loop'

			call pa%set_inputs([pa%inputs, pe%outputs( pe%io-1 )])
			call pa%interpret()

			call pb%set_inputs([pb%inputs, pa%outputs( pa%io-1 )])
			call pb%interpret()

			call pc%set_inputs([pc%inputs, pb%outputs( pb%io-1 )])
			call pc%interpret()

			call pd%set_inputs([pd%inputs, pc%outputs( pc%io-1 )])
			call pd%interpret()

			call pe%set_inputs([pe%inputs, pd%outputs( pd%io-1 )])
			call pe%interpret()
		end do

		!********

		maxout = max(maxout, pe%outputs( pe%io-1 ))

		next = next_perm(phases)

		! TODO
		!exit

	end do

	write(*,*) 'part2 = ', maxout
	write(*,*) ''

end subroutine part2

!===============================================================================

subroutine part1()

	integer, parameter :: namps = 5

	integer :: i, maxout
	integer, allocatable :: prog0(:), phases(:)

	logical :: next

	type(intcode) :: pa, pb, pc, pd, pe

	prog0 = readprog(finput)

	maxout = -huge(maxout)

	phases = [(i, i = 0, namps - 1)]
	next = .true.
	do while (next)
		!print *, 'phases = ', phases

		! Amplifier A
		pa = new(prog0, [phases(1), 0])
		!pa%debug = 1
		call pa%interpret()

		! Amplifier B
		pb = new(prog0, [phases(2), pa%outputs(0)])
		call pb%interpret()

		! Amplifier C
		pc = new(prog0, [phases(3), pb%outputs(0)])
		call pc%interpret()

		! Amplifier D
		pd = new(prog0, [phases(4), pc%outputs(0)])
		call pd%interpret()

		! Amplifier E
		pe = new(prog0, [phases(5), pd%outputs(0)])
		call pe%interpret()

		maxout = max(maxout, pe%outputs(0))

		next = next_perm(phases)

	end do

	print *, 'pa%stat = ', pa%stat

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

