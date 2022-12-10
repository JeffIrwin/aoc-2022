
!===============================================================================

module m

	use utils

	implicit none

#if 1
	character(len = *), parameter :: finput = 'test-input.txt'
#else
	character(len = *), parameter :: finput = 'input.txt'
#endif

	integer, parameter :: nmax = 1024

	type file
		integer :: size
		character(len = :), allocatable :: name
	end type file

	type dir
		type(dir)  :: dirs(nmax)
		type(file) :: files(nmax)
		integer :: nfiles = 0, ndirs = 0
	end type dir

contains

!===============================================================================

subroutine printdir(d, level)

	type(dir) :: d

	integer, optional :: level

	!********

	integer :: i, l
	integer, parameter :: iu = output_unit

	l = 0
	if (present(level)) l = level

	l = l + 1

	do i = 1, d%nfiles
		write(iu, '(a,i0,a)') repeat(' ', 2 * l)//'- '//d%files(i)%name  &
			//' (file, size=', d%files(i)%size, ')'
	end do

	write(iu, *)

end subroutine printdir

!===============================================================================

subroutine part1()

	character :: s*256

	integer :: iu, io
	integer(kind = 8) :: isum

	type(file) :: f

	type(dir) :: r ! root

	open(file = finput, newunit = iu, status = 'old')

	isum = 0

	f%name = 'b.txt'
	f%size = 14848514

	r%nfiles = r%nfiles + 1
	r%files(r%nfiles) = f

	f%name = 'c.dat'
	f%size = 8504156

	r%nfiles = r%nfiles + 1
	r%files(r%nfiles) = f

	call printdir(r)

	return ! TODO

	do
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		print *, 's = ', trim(s)

	end do

	close(iu)

	print *, 'part 1 = ', isum
	print *, ''

end subroutine part1

!===============================================================================

end module m

!===============================================================================

program main

	use m

	print *, 'Starting AOC main'
	print *, ''

	call part1()
	!call part2()

	print *, 'Ending AOC main'
	print *, ''

end program main

!===============================================================================

