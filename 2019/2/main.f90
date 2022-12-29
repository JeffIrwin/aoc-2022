
!===============================================================================

module m

	use utils

	implicit none

#if 0
	character(len = *), parameter :: finput = 'test-input.txt'
#else
	character(len = *), parameter :: finput = 'input.txt'
#endif

	! opcodes
	integer, parameter :: add = 1, mul = 2, finish = 99

contains

!===============================================================================

subroutine execute(prog)

	! Execute an opcode program

	integer, intent(inout) :: prog(0:)

	!********

	integer :: ip, i1, i2, i3, opcode, ninst

	! Instruction pointer
	ip = 0

	do
		opcode = prog(ip)
		!print *, 'opcode = ', opcode

		! Number of values in an instruction
		ninst = 1

		if (opcode == finish) then

			ninst = 1
			exit

		else if (opcode == add) then
			ninst = 4

			! Parameter addresses
			i1 = prog(ip+1)
			i2 = prog(ip+2)
			i3 = prog(ip+3)

			prog(i3) = prog(i1) + prog(i2)

		else if (opcode == mul) then
			ninst = 4

			i1 = prog(ip+1)
			i2 = prog(ip+2)
			i3 = prog(ip+3)

			prog(i3) = prog(i1) * prog(i2)

		end if

		ip = ip + ninst
	end do

end subroutine execute

!===============================================================================

subroutine part2()

	character :: s*1024

	integer, parameter :: nprog_max = 1024

	integer :: i, iu, io, is, isum, nprog, noun, verb
	integer :: prog0(0: nprog_max - 1)
	integer, allocatable :: prog(:)

	open(file = finput, newunit = iu, status = 'old')

	isum = 0

	! Read opcodes into an array
	!
	! TODO: make reading function.  No fixed capacity

	read(iu, '(a)', iostat = io) s
	!if (io == iostat_end) exit

	!print *, 's = ', trim(s)

	is = 1
	i = 0
	do while (is < len_trim(s))

		if (i >= nprog_max) then
			write(*,*) 'Error: program overflow'
			stop
		end if

		prog0(i) = readint(s, is)
		i = i + 1

	end do
	nprog = i

	close(iu)

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

		! Execute opcodes
		call execute(prog)

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

	character :: s*1024

	integer, parameter :: nprog_max = 1024

	integer :: i, iu, io, is, isum, nprog
	integer :: prog(0: nprog_max - 1)

	open(file = finput, newunit = iu, status = 'old')

	isum = 0

	! Read opcodes into an array

	read(iu, '(a)', iostat = io) s
	!if (io == iostat_end) exit

	!print *, 's = ', trim(s)

	is = 1
	i = 0
	do while (is < len_trim(s))

		if (i >= nprog_max) then
			write(*,*) 'Error: program overflow'
			stop
		end if

		prog(i) = readint(s, is)
		i = i + 1

	end do
	nprog = i

	close(iu)

	! Special instuctions for day 2
	prog(1) = 12
	prog(2) =  2

	!print *, 'prog = ', prog(0: nprog - 1)

	call execute(prog)

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

