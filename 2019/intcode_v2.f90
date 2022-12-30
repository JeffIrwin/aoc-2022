
!===============================================================================

module mintcode_v2

	! This is breaking change v2.  Days <= 5 still use v1 (intcode.f90)

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

	! An intcode program and its state
	type intcode

		integer, allocatable :: prog(:), inputs(:), outputs(:)

		! Instruction pointer
		integer :: ip = 0

		! Input and output indices
		integer :: ii = 0
		integer :: io = 0

		! Debug log verbosity
		integer :: debug = 0

		contains
			procedure interpret

	end type intcode

	interface new
		module procedure new_intcode
	end interface new

contains

!===============================================================================

function new_intcode(prog, inputs) result(ic)

	! intcode constructor

	integer, intent(in) :: prog(0:)
	integer, intent(in) :: inputs(0:)

	type(intcode) :: ic

	allocate(ic%prog(0: size(prog)-1))
	ic%prog = prog

	allocate(ic%inputs(0: size(inputs)-1))
	ic%inputs = inputs

end function new_intcode

!===============================================================================

!subroutine interpret(prog, inputs, outputs)
subroutine interpret(ic)

	! Interpret an intcode program prog with inputs
	!
	! TODO: type-bound procedure

	class(intcode), intent(inout) :: ic

	!********

	integer :: inst, opcode, ninst, nwrite
	integer, allocatable :: p(:)

	! Instruction pointer.  TODO: move state initialization to new()
	ic%ip = 0

	! Input and output indices
	ic%ii = 0
	ic%io = 0

	allocate(ic%outputs(0: 1023))

	do
		inst = ic%prog(ic%ip)
		opcode = mod(inst, 100)

		!print *, 'opcode = ', opcode
		!print *, 'inst = ', inst

		! Number of values in an instruction.  Only initialized here to
		! safeguard against an infinite loop
		ninst  = 1
		nwrite = 0

		if (opcode == finish) then

			ninst  = 1
			nwrite = 0
			exit

		else if (opcode == add) then
			ninst  = 4
			nwrite = 1
			p = get_pars()

			ic%prog(p(3)) = p(1) + p(2)

		else if (opcode == mul) then
			ninst  = 4
			nwrite = 1
			p = get_pars()

			ic%prog(p(3)) = p(1) * p(2)

		else if (opcode == input) then
			ninst  = 2
			nwrite = 1
			p = get_pars()

			if (ic%ii >= size(ic%inputs)) then
				write(*, '(a)') 'Error in intcode::interpret():' &
						//' reached end of ic%inputs'
				exit
				!stop
			end if

			ic%prog(p(1)) = ic%inputs(ic%ii)
			ic%ii = ic%ii + 1

		else if (opcode == output) then
			ninst  = 2
			nwrite = 0
			p = get_pars()

			! TODO: growable array
			if (ic%io >= size(ic%outputs)) then
				write(*, '(a)') 'Error in intcode::interpret():' &
						//' reached end of outputs'
				stop
			end if

			ic%outputs(ic%io) = p(1)
			ic%io = ic%io + 1

		else if (opcode == jnz) then
			ninst  = 3
			nwrite = 0
			p = get_pars()

			if (p(1) /= 0) then
				ic%ip = p(2)
				ninst = 0
			end if

		else if (opcode == jz) then
			ninst  = 3
			nwrite = 0
			p = get_pars()

			if (p(1) == 0) then
				ic%ip = p(2)
				ninst = 0
			end if

		else if (opcode == lt) then
			ninst  = 4
			nwrite = 1
			p = get_pars()

			ic%prog(p(3)) = 0
			if (p(1) < p(2)) ic%prog(p(3)) = 1

		else if (opcode == eq) then
			ninst  = 4
			nwrite = 1
			p = get_pars()

			ic%prog(p(3)) = 0
			if (p(1) == p(2)) ic%prog(p(3)) = 1

		else

			write(*, '(a,i0,a)') 'Error in intcode::interpret():' &
					//' unknown opcode "', opcode, '"'
			stop

		end if

		ic%ip = ic%ip + ninst
	end do

	if (ic%debug > 0) print *, 'outputs = ', ic%outputs(0: ic%io - 1)

	! Trim
	ic%outputs = ic%outputs(0: ic%io - 1)

contains

!********

function get_pars() result(pars)

	! Get the parameters (arguments) for the instruction at ip in program prog.
	! Handle immediate mode vs position mode.  Assume write args are always at
	! the end
	!
	! Unlike most other arrays in this interpreter, the result pars array is
	! a Fortran default 1-based array.  You can think of pars(0) as the
	! inst/opcode, just like argv[0] is the command (not an argument) in C.

	integer, allocatable :: pars(:)
	integer :: i, div
	integer, parameter :: base = 10

	!print *, 'inst = ', inst

	div = base ** 2

	allocate(pars(ninst - 1))

	! The opcode is at i=0, so start the loop at 1.
	do i = 1, ninst - 1
		pars(i) = ic%prog(ic%ip + i)

		if (i < ninst - nwrite .and. mod(inst / div, base) == 0) then
			! Output (write) parameters are always in position mode, so leave
			! them as an index for use in caller
			pars(i) = ic%prog( pars(i) )
		end if

		div = div * base
	end do

end function get_pars

!********

end subroutine interpret

!===============================================================================

function readprog(finput) result(prog)

	! Read an intcode program prog from a file finput.  This can be a fn
	! because new() takes care of the lbound for us

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

end function readprog

!===============================================================================

end module mintcode_v2

!===============================================================================

