
!===============================================================================

module m

	use utils

	implicit none

#if 0
	character(len = *), parameter :: finput = 'test-input.txt'
#else
	character(len = *), parameter :: finput = 'input.txt'
#endif

#define n 4

#define n2 14

contains

!===============================================================================

logical function unique(s)

	! Are all the characters in s unique?

	character(len = *), intent(in) :: s

	integer :: i, j

	unique = .true.
	do i = 1, len_trim(s)
		do j = 1, i - 1

			if (s(i:i) == s(j:j)) then
				unique = .false.
				return
			end if

		end do
	end do

end function unique

!===============================================================================

subroutine part1()

	character :: s*4096, c*n

	integer :: i, iu, io, ans

	open(file = finput, newunit = iu, status = 'old')

	ans = 0

	read(iu, '(a)', iostat = io) s
	!if (io == iostat_end) exit

	print *, 's = ', trim(s)

	do i = 1, len_trim(s) - n + 1

		c = s(i: i + n - 1)
		print *, 'c = ', c

		if (unique(c)) then
			ans = i + n - 1
			exit
		end if

	end do

	close(iu)

	print *, 'part 1 = ', ans
	print *, ''

end subroutine part1

!===============================================================================

subroutine part2()

	character :: s*4096, c*n2

	integer :: i, iu, io, ans

	open(file = finput, newunit = iu, status = 'old')

	ans = 0

	read(iu, '(a)', iostat = io) s
	!if (io == iostat_end) exit

	print *, 's = ', trim(s)

	do i = 1, len_trim(s) - n2 + 1

		c = s(i: i + n2 - 1)
		print *, 'c = ', c

		if (unique(c)) then
			ans = i + n2 - 1
			exit
		end if

	end do

	close(iu)

	print *, 'part 2 = ', ans
	print *, ''

end subroutine part2

!===============================================================================

end module m

!===============================================================================

program main

	use m

	print *, 'Starting AOC main'
	print *, ''

	call part1()
	call part2()

	print *, 'Ending AOC main'
	print *, ''

end program main

!===============================================================================

