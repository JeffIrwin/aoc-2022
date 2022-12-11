
!===============================================================================

module m

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

	integer, parameter :: ncrt = 240

	character :: s*256, crt(0: ncrt - 1)
	character(len = :), allocatable :: inst

	integer :: iu, io, isum, is, v, rx, ic, work, icrt

	open(file = finput, newunit = iu, status = 'old')

	isum = 0

	! X register
	rx = 1

	! Number of cycle to complete current work
	work = 0

	! Clock cycle loop
	ic = 0
	do
		ic = ic + 1

		if (work == 0) then

			! Only read a new instruction if work is finished
			read(iu, '(a)', iostat = io) s
			if (io == iostat_end) exit

			!print *, 's = ', trim(s)

			is = 1
			inst = readword(s, is)

			if      (inst == 'noop') then
				work = 1

			else if (inst == 'addx') then
				work = 2
				v = readint(s, is)

			else
				write(*,*) 'Error: unknown instruction "'//inst//'"'
				stop
			end if

		end if

		work = work - 1

		! CRT draws pixel 0 at cycle 1, 1 at 2, ...
		icrt = ic - 1

		! Sprite is 3 pixels wide (center plus 1 on either side)
		if (abs(mod(icrt,40) - rx) <= 1) then
			crt(icrt) = '#'
		else
			crt(icrt) = '.'
		end if

		if (inst == 'addx' .and. work == 0) then
			rx = rx + v
			!print *, 'rx = ', rx
		end if

	end do

	close(iu)

	!print *, 'crt = ', crt

	write(*,*) 'part 2 = '

	write(*,*) crt(  0:  39)
	write(*,*) crt( 40:  79)
	write(*,*) crt( 80: 119)
	write(*,*) crt(120: 159)
	write(*,*) crt(160: 199)
	write(*,*) crt(200: 239)

	write(*,*) ''

end subroutine part2

!===============================================================================

subroutine part1()

	character :: s*256
	character(len = :), allocatable :: inst

	integer :: iu, io, isum, is, v, rx, ic, work, signal

	open(file = finput, newunit = iu, status = 'old')

	isum = 0

	! X register
	rx = 1

	! Number of cycle to complete current work
	work = 0

	! Clock cycle loop
	ic = 0
	do
		ic = ic + 1

		if (work == 0) then

			! Only read a new instruction if work is finished
			read(iu, '(a)', iostat = io) s
			if (io == iostat_end) exit

			!print *, 's = ', trim(s)

			is = 1
			inst = readword(s, is)

			if      (inst == 'noop') then
				work = 1

			else if (inst == 'addx') then
				work = 2
				v = readint(s, is)

			else
				write(*,*) 'Error: unknown instruction "'//inst//'"'
				stop
			end if

		end if

		work = work - 1

		if (mod(ic - 20, 40) == 0) then
			signal = ic * rx
			isum = isum + signal

			!print *, 'ic, rx, signal = ', ic, rx, signal
		end if

		if (inst == 'addx' .and. work == 0) then
			rx = rx + v
			!print *, 'rx = ', rx
		end if

	end do

	close(iu)

	write(*,*) 'part 1 = ', isum
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

