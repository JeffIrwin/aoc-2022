
!===============================================================================

module m

	use utils

	implicit none

#if 0
	character(len = *), parameter :: finput = 'test-input.txt'
	integer, parameter :: nmonkeys = 4
#else
	character(len = *), parameter :: finput = 'input.txt'
	integer, parameter :: nmonkeys = 8
#endif

	integer, parameter :: nmax = 64

	type monkey

		integer :: items(nmax), nitems = 0

		! Left operand, operator, and right operand
		character(len = :), allocatable :: opl, op, opr

		integer :: testmod, dst_true, dst_false

	end type monkey

contains

!===============================================================================

subroutine readmonkeys(m)

	character :: s*256

	integer :: i, is, iu, io, isum

	type(monkey), allocatable :: m(:)

	open(file = finput, newunit = iu, status = 'old')

	isum = 0

	do i = 1, size(m)

		! Monkey ID.  Assume increasing 0 through nmonkeys-1
		read(iu, '(a)', iostat = io) s
		!print *, 's = ', trim(s)

		! Items
		read(iu, '(a)', iostat = io) s
		!print *, 's = ', trim(s)

		! Skip past "Starting items:"
		is = 18
		do while (is < len_trim(s))
			m(i)%nitems = m(i)%nitems + 1
			m(i)%items( m(i)%nitems ) = readint(s, is)
		end do

		! Operation new = opl op opr
		read(iu, '(a)', iostat = io) s

		is = 19
		m(i)%opl = readword(s, is)
		m(i)%op  = readword(s, is)
		m(i)%opr = readword(s, is)

		! Test modulo divisor
		read(iu, '(a)', iostat = io) s
		read(s(21:), *) m(i)%testmod

		! Destination if test is true
		read(iu, '(a)', iostat = io) s
		read(s(29:), *) m(i)%dst_true

		! Destination if test is false
		read(iu, '(a)', iostat = io) s
		read(s(30:), *) m(i)%dst_false

		! Blank line
		read(iu, '(a)', iostat = io) s

		print *, 'Monkey ', i-1, ':'
		print *, '  Starting items:', m(i)%items( 1: m(i)%nitems )
		print *, '  Operation: new = ', m(i)%opl, m(i)%op, m(i)%opr
		print *, '  Test: divisible by ', m(i)%testmod
		print *, '    If true: throw to monkey ', m(i)%dst_true
		print *, '    If false: throw to monkey ', m(i)%dst_false
		print *, ''

	end do

	close(iu)

end subroutine readmonkeys

!===============================================================================

subroutine part1()

	integer :: isum

	type(monkey), allocatable :: m(:)

	allocate(m(nmonkeys))
	call readmonkeys(m)

	write(*,*) 'part1 = ', isum
	write(*,*) ''

end subroutine part1

!===============================================================================

end module m

!===============================================================================

program main

	use m

	write(*,*) 'Starting AOC main'
	write(*,*) ''

	call part1()
	!call part2()

	write(*,*) 'Ending AOC main'
	write(*,*) ''

end program main

!===============================================================================

