
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

	! Number of ints in an opcode and its inputs
	integer, parameter :: nint_op = 4

contains

!===============================================================================

subroutine part2()

	character :: s*1024

	integer, parameter :: nprog_max = 1024

	integer :: i, iu, io, is, isum, nprog, iop, &
			i1, i2, i3, noun, verb
	integer(kind = 4) :: prog0(0: nprog_max - 1)
	integer, allocatable :: prog(:)

	open(file = finput, newunit = iu, status = 'old')

	isum = 0

	! Read opcodes into an array

	read(iu, '(a)', iostat = io) s
	!if (io == iostat_end) exit

	print *, 's = ', trim(s)

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
	!noun = 12
	!verb = 2

	prog = prog0(0: nprog - 1)
	print *, 'noun, verb = ', noun, verb

	! Special instuctions for day 2
	prog(1) = noun
	prog(2) = verb

	!print *, 'prog = ', prog(0: nprog - 1)

	! Execute opcodes
	iop = 0
	do
		!print *, 'op = ', prog(iop)

		if (prog(iop) == finish) then

			exit

		else if (prog(iop) == add) then

			i1 = prog(iop+1)
			i2 = prog(iop+2)
			i3 = prog(iop+3)

			prog(i3) = prog(i1) + prog(i2)

		else if (prog(iop) == mul) then

			i1 = prog(iop+1)
			i2 = prog(iop+2)
			i3 = prog(iop+3)

			prog(i3) = prog(i1) * prog(i2)

		end if

		iop = iop + 4
	end do

	!print *, 'prog = ', prog(0: nprog - 1)
	if (prog(0) == 19690720) then
		print *, 'found'
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

	integer :: i, iu, io, is, isum, nprog, iop, &
			i1, i2, i3
	integer(kind = 4) :: prog(0: nprog_max - 1)

	open(file = finput, newunit = iu, status = 'old')

	isum = 0

	! Read opcodes into an array

	read(iu, '(a)', iostat = io) s
	!if (io == iostat_end) exit

	print *, 's = ', trim(s)

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

	print *, 'prog = ', prog(0: nprog - 1)

	! Execute opcodes
	iop = 0
	do
		print *, 'op = ', prog(iop)

		if (prog(iop) == finish) then

			exit

		else if (prog(iop) == add) then

			i1 = prog(iop+1)
			i2 = prog(iop+2)
			i3 = prog(iop+3)

			prog(i3) = prog(i1) + prog(i2)

		else if (prog(iop) == mul) then

			i1 = prog(iop+1)
			i2 = prog(iop+2)
			i3 = prog(iop+3)

			prog(i3) = prog(i1) * prog(i2)

		end if

		iop = iop + 4
	end do

	print *, 'prog = ', prog(0: nprog - 1)

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

