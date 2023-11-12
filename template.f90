
!===============================================================================

module m

	use utils

	implicit none

#if 1
	character(len = *), parameter :: finput = "test-input.txt"
#else
	character(len = *), parameter :: finput = "input.txt"
#endif

	character, parameter :: &
			ESC             = char(27)

	! ANSI escape codes.  c.f.
	! https://github.com/JeffIrwin/cali/blob/main/src/cali.f90
	character(len = *), parameter :: &
			RED         = ESC//"[91;1m", &
			MAGENTA     = ESC//"[95;1m", &
			GREEN       = ESC//"[92m", &
			RESET_COLOR = ESC//"[0m"

contains

!===============================================================================

subroutine part1()

	character :: s*256

	integer :: iu, io, isum

	open(file = finput, newunit = iu, status = "old")

	isum = 0

	do
		read(iu, "(a)", iostat = io) s
		if (io == iostat_end) exit

		print *, "s = ", trim(s)

	end do

	close(iu)

	write(*,*) "part1 = ", isum
	write(*,*)

end subroutine part1

!===============================================================================

end module m

!===============================================================================

program main

	use m

	write(*,*) MAGENTA//"Starting AOC main"//RESET_COLOR
	write(*,*) "Input file = ", finput
	write(*,*)

	call part1()
	!call part2()

	write(*,*) GREEN  //"Success!"//RESET_COLOR
	write(*,*) MAGENTA//"Ending AOC main"//RESET_COLOR
	write(*,*)

end program main

!===============================================================================

