
!===============================================================================

module m

	use utils
	!use fmzm

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

		!type(im) :: items(nmax), testmod
		integer :: items(nmax), testmod

		! Items mod each monkey's testmod
		integer, allocatable :: itemsm(:,:)

		integer :: nitems = 0, ninspect = 0

		! Left operand, operator, and right operand
		character(len = :), allocatable :: opl, op, opr

		integer :: dst_true, dst_false

	end type monkey

contains

!===============================================================================

subroutine readmonkeys(m)

	character :: s*256

	integer :: i, is, iu, io, itmp

	type(monkey), allocatable :: m(:)

	open(file = finput, newunit = iu, status = 'old')

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
		read(s(21:), *) itmp
		m(i)%testmod = itmp

		! Destination if test is true
		read(iu, '(a)', iostat = io) s
		read(s(29:), *) m(i)%dst_true

		! Destination if test is false
		read(iu, '(a)', iostat = io) s
		read(s(30:), *) m(i)%dst_false

		! Blank line
		read(iu, '(a)', iostat = io) s

		!print *, 'Monkey ', i-1, ':'
		!print *, '  Starting items:', m(i)%items( 1: m(i)%nitems )
		!print *, '  Operation: new = ', m(i)%opl, m(i)%op, m(i)%opr
		!print *, '  Test: divisible by ', m(i)%testmod
		!print *, '    If true: throw to monkey ', m(i)%dst_true
		!print *, '    If false: throw to monkey ', m(i)%dst_false
		!print *, ''

	end do

	close(iu)

end subroutine readmonkeys

!===============================================================================

subroutine printitems(m)

	character(len = :), allocatable :: str

	type(monkey), allocatable :: m(:)

	!type(im) :: i, j
	integer :: i, j

	do i = 1, size(m)
		write(*, '(a,i0,a)', advance = 'no') 'Monkey ', i-1, ': '
		do j = 1, m(i)%nitems

			!str = im_format( 'i200', m(i)%items(j) )
			!str = trim(adjustl(str))
			!write(*, '(a)', advance = 'no') trim(str)//', '

			write(*, '(i0,a)', advance = 'no') m(i)%items(j), ', '

		end do
		write(*,*)
	end do
	write(*,*)

end subroutine printitems

!===============================================================================

subroutine part2()

	!type(im) :: old, new, opl, opr
	integer :: opl, opr
	integer, allocatable :: old(:), new(:)

	integer :: i, j, k, ir, nrounds = 10000
	integer :: iopl, iopr, dst
	integer(kind = 8) :: imax1, imax2, isum

	type(monkey), allocatable :: m(:)

	isum = 0

	allocate(m(nmonkeys))
	call readmonkeys(m)

	! Without dividing by 3, the item numbers become too large for even 16-byte
	! ints.  We could use big nums, but it takes too long.
	!
	! Because all the operations are multiplication or addition, and all of the
	! tests are whether the item number is divisible by something, we only need
	! to keep track of the item numbers *mod* each monkey's testmod value.  This
	! turns each item number from a scalar to a vector (mod for each monkey),
	! but it prevents their size from blowing up.

	do i = 1, nmonkeys
		allocate(m(i)%itemsm(nmonkeys, nmax))

		do j = 1, m(i)%nitems
			m(i)%itemsm(:,j) = m(i)%items(j)
		end do

	end do

	allocate(old(nmonkeys), new(nmonkeys))

	do ir = 1, nrounds
		!print *, 'Round = ', ir

		do i = 1, nmonkeys

			do j = 1, m(i)%nitems

				! Inspect
				m(i)%ninspect = m(i)%ninspect + 1

				do k = 1, nmonkeys

					old(k) = m(i)%itemsm(k,j)

					if (m(i)%opl == 'old') then
						opl = old(k)
					else
						read(m(i)%opl, *) iopl
						opl = iopl
					end if

					if (m(i)%opr == 'old') then
						opr = old(k)
					else
						read(m(i)%opr, *) iopr
						opr = iopr
					end if

					if      (m(i)%op == '+') then
						new(k) = opl + opr
					else if (m(i)%op == '*') then
						new(k) = opl * opr
					end if

					new(k) = mod(new(k), m(k)%testmod)

				end do

				!! Divide by 3 (and floor)
				!new = new / 3

				if (mod(new(i), m(i)%testmod) == 0) then
					dst = m(i)%dst_true
				else
					dst = m(i)%dst_false
				end if

				! 0-based to 1-based index
				dst = dst + 1

				!if (new < 0) then
				!	write(*,*) 'Error: arithmetic overflow'
				!	stop
				!end if

				! Throw to monkey dst
				m(dst)%nitems = m(dst)%nitems + 1
				do k = 1, nmonkeys
					m(dst)%itemsm( k, m(dst)%nitems ) = new(k)
				end do

			end do

			! Monkey i has thrown all items.  Assume it never throws to itself
			m(i)%nitems = 0

		end do

		!call printitems(m)
		!exit

	end do

	!print *, 'ninspect = ', m%ninspect

	imax1 = maxval(m%ninspect)
	imax2 = maxval(m%ninspect, m%ninspect < imax1)

	!print *, 'imax1 = ', imax1
	!print *, 'imax2 = ', imax2

	isum = imax1 * imax2

	write(*,*) 'part2 = ', isum
	write(*,*) ''

end subroutine part2

!===============================================================================

subroutine part1()

	!type(im) :: isum, ir, old, new, opl, opr, dst, &
	!		imax1, imax2

	integer :: isum, ir, old, new, opl, opr, dst, &
			imax1, imax2

	integer :: i, j, nrounds = 20

	type(monkey), allocatable :: m(:)

	isum = 0

	allocate(m(nmonkeys))
	call readmonkeys(m)

	do ir = 1, nrounds

		do i = 1, nmonkeys

			do j = 1, m(i)%nitems

				! Inspect
				m(i)%ninspect = m(i)%ninspect + 1
				old = m(i)%items(j)

				if (m(i)%opl == 'old') then
					opl = old
				else
					read(m(i)%opl, *) opl
				end if

				if (m(i)%opr == 'old') then
					opr = old
				else
					read(m(i)%opr, *) opr
				end if

				if      (m(i)%op == '+') then
					new = opl + opr
				else if (m(i)%op == '*') then
					new = opl * opr
				end if

				! Divide by 3 (and floor)
				new = new / 3

				if (mod(new, m(i)%testmod) == 0) then
					dst = m(i)%dst_true
				else
					dst = m(i)%dst_false
				end if

				! 0-based to 1-based index
				dst = dst + 1

				! Throw to monkey dst
				m(dst)%nitems = m(dst)%nitems + 1
				m(dst)%items( m(dst)%nitems ) = new

			end do

			! Monkey i has thrown all items.  Assume it never throws to itself
			m(i)%nitems = 0

		end do

		!call printitems(m)
		!exit

	end do

	!call printitems(m)
	!print *, 'ninspect = ', m%ninspect

	imax1 = maxval(m%ninspect)
	imax2 = maxval(m%ninspect, m%ninspect < imax1)

	isum = imax1 * imax2

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
	call part2()

	write(*,*) 'Ending AOC main'
	write(*,*) ''

end program main

!===============================================================================

