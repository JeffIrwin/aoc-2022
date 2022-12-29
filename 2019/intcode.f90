
!===============================================================================

module intcode

	use iso_fortran_env
	use utils

	implicit none

	! opcodes
	integer, parameter :: &
			add    = 1, &
			mul    = 2, &
			input  = 3, &
			output = 4, &
			jnz    = 5, &  ! jump if non-zero (true)
			jz     = 6, &  ! jump if zero (false)
			lt     = 7, &  ! less than
			eq     = 8, &  ! equals
			finish = 99

contains

!===============================================================================

subroutine interpret(prog, inputs, outputs)

	! Interpret an intcode program prog with inputs
	!
	! TODO: add debug arg

	integer, intent(inout) :: prog(0:)
	integer, intent(in) :: inputs(0:)
	integer, allocatable, intent(out) :: outputs(:)

	!********

	integer :: inst, ip, ii, io, i1, i2, i3, opcode, ninst, nwrite
	integer, allocatable :: p(:)

	! Instruction pointer
	ip = 0

	! Input and output indices
	ii = 0
	io = 0

	allocate(outputs(0: 1023))

	do
		inst = prog(ip)
		opcode = mod(inst, 100)

		!print *, 'opcode = ', opcode
		!print *, 'inst = ', inst

		! Number of values in an instruction
		ninst  = 1
		nwrite = 0

		if (opcode == finish) then

			ninst  = 1
			nwrite = 0
			exit

		else if (opcode == add) then
			ninst  = 4
			nwrite = 1

			! Parameter addresses.  TODO: refactor
			i1 = prog(ip+1)
			i2 = prog(ip+2)
			i3 = prog(ip+3)

			! Position mode?  Immediate mode is only for input parameters.  Output
			! parameters are always position mode.
			if (mod(inst /  100, 10) == 0) i1 = prog(i1)
			if (mod(inst / 1000, 10) == 0) i2 = prog(i2)

			!prog(i3) = prog(i1) + prog(i2)
			prog(i3) = i1 + i2

		else if (opcode == mul) then
			ninst  = 4
			nwrite = 1

			i1 = prog(ip+1)
			i2 = prog(ip+2)
			i3 = prog(ip+3)

			if (mod(inst /  100, 10) == 0) i1 = prog(i1)
			if (mod(inst / 1000, 10) == 0) i2 = prog(i2)

			!prog(i3) = prog(i1) * prog(i2)
			prog(i3) = i1 * i2

		else if (opcode == input) then
			ninst  = 2
			nwrite = 1

			i1 = prog(ip+1)

			if (ii >= size(inputs)) then
				write(*, '(a)') 'Error in intcode::interpret(): reached end of inputs'
				stop
			end if

			prog(i1) = inputs(ii)
			ii = ii + 1

		else if (opcode == output) then
			ninst  = 2
			nwrite = 0

			i1 = prog(ip+1)

			if (mod(inst /  100, 10) == 0) i1 = prog(i1)

			! TODO: growable array
			if (io >= size(outputs)) then
				write(*, '(a)') 'Error in intcode::interpret(): reached end of outputs'
				stop
			end if

			outputs(io) = i1
			io = io + 1

		else if (opcode == jnz) then
			ninst  = 3
			nwrite = 0

			i1 = prog(ip+1)
			i2 = prog(ip+2)

			if (mod(inst /  100, 10) == 0) i1 = prog(i1)
			if (mod(inst / 1000, 10) == 0) i2 = prog(i2)

			if (i1 /= 0) then
				! Instruction pointer is not automatically increased by ninst
				ip = i2
				ninst = 0
			end if

		else if (opcode == jz) then
			ninst  = 3
			nwrite = 0

			i1 = prog(ip+1)
			i2 = prog(ip+2)

			if (mod(inst /  100, 10) == 0) i1 = prog(i1)
			if (mod(inst / 1000, 10) == 0) i2 = prog(i2)

			if (i1 == 0) then
				ip = i2
				ninst = 0
			end if

		else if (opcode == lt) then
			ninst  = 4
			nwrite = 1

			!i1 = prog(ip+1)
			!i2 = prog(ip+2)
			!i3 = prog(ip+3)
			!if (mod(inst /  100, 10) == 0) i1 = prog(i1)
			!if (mod(inst / 1000, 10) == 0) i2 = prog(i2)
			!prog(i3) = 0
			!if (i1 < i2) prog(i3) = 1

			p = get_pars()
			prog(p(3)) = 0
			if (p(1) < p(2)) prog(p(3)) = 1

		else if (opcode == eq) then
			ninst  = 4
			nwrite = 1

			!i1 = prog(ip+1)
			!i2 = prog(ip+2)
			!i3 = prog(ip+3)
			!if (mod(inst /  100, 10) == 0) i1 = prog(i1)
			!if (mod(inst / 1000, 10) == 0) i2 = prog(i2)
			!prog(i3) = 0
			!if (i1 == i2) prog(i3) = 1

			!p = [0,0,0]
			!p(1) = prog(ip+1)
			!p(2) = prog(ip+2)
			!p(3) = prog(ip+3)
			!if (mod(inst /  100, 10) == 0) p(1) = prog(p(1))
			!if (mod(inst / 1000, 10) == 0) p(2) = prog(p(2))
			!!if (mod(inst / 10000,10) == 0) p(3) = prog(p(3))
			!prog(p(3)) = 0
			!if (p(1) == p(2)) prog(p(3)) = 1

			p = get_pars()
			prog(p(3)) = 0
			if (p(1) == p(2)) prog(p(3)) = 1

		else

			write(*, '(a,i0,a)') 'Error in intcode::interpret(): unknown opcode "', &
					opcode, '"'
			stop

		end if

		ip = ip + ninst
	end do

	!print *, 'outputs = ', outputs(0: io - 1)

	! Trim
	outputs = outputs(0: io - 1)

contains

!===============================================================================

!function get_pars(prog, ip, ninst) result(pars)
function get_pars() result(pars)

	! Get the parameters for the instruction at ip in program prog.  Handle
	! immediate vs position mode

	!integer :: prog(:), ip, ninst
	integer, allocatable :: pars(:)
	integer :: i, div
	integer, parameter :: base = 10

	!print *, 'inst = ', inst

	div = base ** 2

	if (allocated(pars)) deallocate(pars)
	allocate(pars(ninst - 1))

	! The opcode is at i=0, so start the loop at 1.
	do i = 1, ninst - 1
		pars(i) = prog(ip + i)

		if (i < ninst - nwrite .and. mod(inst / div, base) == 0) then
			! Output (write) parameters are ! always in position mode, so leave them
			! as an index
			pars(i) = prog( pars(i) )
		end if

		div = div * base
	end do

end function get_pars

end subroutine interpret

!===============================================================================

subroutine readprog(finput, prog)

	! Read an intcode program prog from a file finput
	!
	! This is easier as a subroutine than a fn since prog is 0-based

	character(len = *) :: finput

	integer, allocatable :: prog(:)

	!********

	character(len = :), allocatable :: s

	integer :: nprog, junk, iu, is, i

	nprog = 0

	open(file = finput, newunit = iu, status = 'old')
	s = readline(iu)
	close(iu)

	! Count ints
	is = 1
	do while (is < len_trim(s))

		junk = readint(s, is)
		nprog = nprog + 1

		!print *, 'nprog = ', nprog
		!print *, 'junk = ', junk
		!print *, ''

	end do
	!print *, 'nprog = ', nprog

	! Save ints
	allocate(prog(0: nprog - 1))
	is = 1
	i = 0
	do while (is < len_trim(s))
		prog(i) = readint(s, is)
		i = i + 1
	end do
	!print *, 'prog = ', prog

end subroutine readprog

!===============================================================================

end module intcode

!===============================================================================
