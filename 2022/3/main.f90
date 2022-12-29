
!===============================================================================

module m

	use iso_fortran_env

	implicit none

	character(len = :), allocatable :: finput

contains

!===============================================================================

integer function priority(c)

	character :: c

	if ('a' <= c .and. c <= 'z') then
		!print *, 'lowercase'
		priority = iachar(c) - iachar('a') + 1
		return
	else
		!print *, 'uppercase'
		priority = iachar(c) - iachar('A') + 27
		return
	end if

end function priority

!===============================================================================

subroutine get_shared_item(a, b, c)

	character :: c
	character(len = *) :: a, b

	integer :: i, j, n

	!print *, 'a|b = ', a, '|', b

	n = len_trim(a)

	do i = 1, n
		do j = 1, n
			if (a(i:i) == b(j:j)) then
				c = a(i:i)
				return
			end if
		end do
	end do

end subroutine get_shared_item

!===============================================================================

subroutine get_badge(a, b, c, badge)

	character :: badge
	character(len = *) :: a, b, c

	integer :: i, j, k, n

	!print *, 'a|b = ', a, '|', b

	do i = 1, len_trim(a)
		do j = 1, len_trim(b)
			do k = 1, len_trim(c)
				!print *, i, j, k
				if (a(i:i) == b(j:j) .and. b(j:j) == c(k:k)) then
					badge = a(i:i)
					return
				end if
			end do
		end do
	end do

end subroutine get_badge

!===============================================================================

subroutine part1

	character :: s*256, c
	character(len = :), allocatable :: c1, c2

	integer :: iu, io, n, n2, isum

	open(file = finput, newunit = iu, status = 'old')

	isum = 0

	do
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		n = len_trim(s)
		n2 = n / 2

		c1 = s(     1: n2)
		c2 = s(n2 + 1: n )

		!print *, 's = ', trim(s)
		!print *, 'n = ', n
		!print *, 'c1 = ', c1
		!print *, 'c2 = ', c2

		call get_shared_item(c1, c2, c)
		isum = isum + priority(c)

		!ic = priority(c)
		!print *, 'c, ic = ', c, ic

	end do

	close(iu)

	print *, 'part 1 = ', isum
	print *, ''

end subroutine part1

!===============================================================================

subroutine part2

	character :: s1*256, s2*256, s3*256, badge

	integer :: iu, io, n, n2, isum

	open(file = finput, newunit = iu, status = 'old')

	isum = 0

	do
		read(iu, '(a)', iostat = io) s1
		if (io == iostat_end) exit
		read(iu, '(a)', iostat = io) s2
		if (io == iostat_end) exit
		read(iu, '(a)', iostat = io) s3
		if (io == iostat_end) exit

		!print *, 's1 = ', trim(s1)
		!print *, 's2 = ', trim(s2)
		!print *, 's3 = ', trim(s3)

		call get_badge(s1, s2, s3, badge)
		print *, 'badge = ', badge
		isum = isum + priority(badge)

	end do

	close(iu)

	print *, 'part 2 = ', isum
	print *, ''

end subroutine part2

!===============================================================================

end module m

!===============================================================================

program main

	use m

	print *, 'Starting AOC main'
	print *, ''

	finput = 'input.txt'
	!finput = 'test-input.txt'

	call part1()
	call part2()

	print *, 'Ending AOC main'
	print *, ''

end program main

!===============================================================================

