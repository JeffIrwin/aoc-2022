
!===============================================================================

module m

	use utils

	implicit none

#if 0
	character(len = *), parameter :: finput = 'test-input.txt'
	integer, parameter :: nstack = 3, ncratemax = 128
#else
	character(len = *), parameter :: finput = 'input.txt'
	integer, parameter :: nstack = 9, ncratemax = 128
#endif

	integer :: ncratetot, ncrate(nstack)

	character :: stack(nstack, ncratemax)

contains

!===============================================================================

subroutine printstack()

	integer :: i, j

	write(*,*) 'stack = '
	do j = 1, maxval(ncrate)
		do i = 1, nstack
			if (j <= ncrate(i)) then
				write(*, '(a)', advance = 'no') stack(i,j)//' '
			else
				write(*, '(a)', advance = 'no') '  '
			end if
		end do
		write(*, *)
	end do

end subroutine printstack

!===============================================================================

subroutine part1()

	character :: s*256
	character :: col(ncratemax)

	integer :: i, k, iu, io, n, src, des, is, ncol, nsrc

	print *, 'finput = ', finput
	print *, 'nstack = ', ncratemax

	open(file = finput, newunit = iu, status = 'old')

	ncratetot = 0
	ncrate = 0

	! Read starting stack configuration
	do
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		!print *, 's = ', trim(s)

		if (len_trim(s) == 0) exit

		i = 0
		do k = 2, len_trim(s), 4
			i = i + 1

			if (isalpha(s(k:k))) then
				ncrate(i) = ncrate(i) + 1

				stack(i, ncrate(i)) = s(k:k)

			end if

		end do

	end do

	!call printstack()

	print *, 'ncratemax = ', ncratemax
	!col = 'a'
	!print *, 'col = ', col

	! Read rearrangement proceduce
	k = 0
	do
		k = k + 1
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		!print *, 's = ', trim(s)

		is = 1
		n   = readint(s, is)
		src = readint(s, is)
		des = readint(s, is)

		!print *, 'move = ', n, src, des
		!print *, 'n + ncrate(des) = ', n + ncrate(des)
		!print *, 'stack(des, 1: ncrate(des)) = ', stack(des, 1: ncrate(des))

		ncol = n + ncrate(des)
		nsrc = ncrate(src) - n

		col(    1: n) = stack(src, n: 1: -1)
		col(n + 1: ncol) = stack(des, 1: ncrate(des))

		!print *, 'col = ', col(1: ncol)

		! Move column to destination
		stack(des, 1: ncol) = col(1: ncol)
		ncrate(des) = ncol

		! Remove crates from source
		stack(src, 1: nsrc) = stack(src, n + 1: n + nsrc)
		ncrate(src) = nsrc

		!call printstack()

		!if (k == 2) exit

	end do

	call printstack()

	close(iu)

	print *, 'part 1 = ', stack(:, 1)
	print *, ''

end subroutine part1

!===============================================================================

subroutine part2()

	character :: s*256
	character :: col(ncratemax)

	integer :: i, k, iu, io, n, src, des, is, ncol, nsrc

	print *, 'finput = ', finput
	print *, 'nstack = ', ncratemax

	open(file = finput, newunit = iu, status = 'old')

	ncratetot = 0
	ncrate = 0

	! Read starting stack configuration
	do
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		!print *, 's = ', trim(s)

		if (len_trim(s) == 0) exit

		i = 0
		do k = 2, len_trim(s), 4
			i = i + 1

			if (isalpha(s(k:k))) then
				ncrate(i) = ncrate(i) + 1

				stack(i, ncrate(i)) = s(k:k)

			end if

		end do

	end do

	!call printstack()

	print *, 'ncratemax = ', ncratemax
	!col = 'a'
	!print *, 'col = ', col

	! Read rearrangement proceduce
	k = 0
	do
		k = k + 1
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		!print *, 's = ', trim(s)

		is = 1
		n   = readint(s, is)
		src = readint(s, is)
		des = readint(s, is)

		!print *, 'move = ', n, src, des
		!print *, 'n + ncrate(des) = ', n + ncrate(des)
		!print *, 'stack(des, 1: ncrate(des)) = ', stack(des, 1: ncrate(des))

		ncol = n + ncrate(des)
		nsrc = ncrate(src) - n

		col(    1: n) = stack(src, 1: n)
		col(n + 1: ncol) = stack(des, 1: ncrate(des))

		!print *, 'col = ', col(1: ncol)

		! Move column to destination
		stack(des, 1: ncol) = col(1: ncol)
		ncrate(des) = ncol

		! Remove crates from source
		stack(src, 1: nsrc) = stack(src, n + 1: n + nsrc)
		ncrate(src) = nsrc

		!call printstack()

		!if (k == 2) exit

	end do

	close(iu)

	call printstack()

	print *, 'part 2 = ', stack(:, 1)
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

