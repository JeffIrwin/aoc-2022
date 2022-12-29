
!===============================================================================

module m

	use utils

	implicit none

#if 0
	character(len = *), parameter :: finput = 'test-input.txt'
#else
	character(len = *), parameter :: finput = 'input.txt'
#endif

	integer, parameter :: nmax = 64

	type file
		character(len = :), allocatable :: name
		integer :: size
	end type file

	type dir

		type(dir), pointer :: parent
		type(dir), pointer  :: dirs(:)

		type(file) :: files(nmax)
		integer :: nfiles = 0, ndirs = 0
		character(len = :), allocatable :: name

		integer :: size = 0  ! sum of all subdirs and files

		contains
			procedure :: pushfile => pushfile
			procedure :: pushdir  => pushdir

	end type dir

	integer(kind = 8) :: smallest = -1

contains

!===============================================================================

recursive subroutine printdir(d, level)

	type(dir) :: d

	integer, optional :: level

	!********

	integer :: i, l
	integer, parameter :: iu = output_unit

	l = 0
	if (present(level)) l = level

	write(iu, '(a,i0,a)') repeat(' ', 2 * l)//'- '//d%name//' (dir, size=', &
		d%size,')'

	l = l + 1

	do i = 1, d%ndirs
		call printdir(d%dirs(i), l)
	end do

	do i = 1, d%nfiles
		write(iu, '(a,i0,a)') repeat(' ', 2 * l)//'- '//d%files(i)%name  &
			//' (file, size=', d%files(i)%size, ')'
	end do

	if (l == 1) write(iu, *)

end subroutine printdir

!===============================================================================

recursive function sum_under(d, n) result(isum)

	type(dir) :: d

	!********

	integer :: i, n
	integer(kind = 8) :: suml, isum

	suml = 0

	do i = 1, d%ndirs
		suml = suml + sum_under(d%dirs(i), n)
	end do

	if (d%size <= n) then
		!print *, 'size(', d%name, ') = ', d%size
		suml = suml + d%size
	end if

	!d%size = suml

	isum = suml

end function sum_under

!===============================================================================

recursive function find_smallest(d, n) result(isum)

	type(dir) :: d

	!********

	integer :: i
	integer(kind = 8) :: suml, isum, n

	suml = 0

	if (d%size >= n) then
		!print *, 'size(', d%name, ') = ', d%size
		suml = d%size

		if (smallest < 0 .or. d%size < smallest) smallest = d%size

	end if

	do i = 1, d%ndirs
		suml = suml + find_smallest(d%dirs(i), n)
	end do

	!d%size = suml

	!isum = suml
	isum = smallest

end function find_smallest

!===============================================================================

subroutine get_sizes(d)

	type(dir) :: d

	integer(kind = 8) :: junk

	junk = dirsize(d)

end subroutine get_sizes

!===============================================================================

recursive function dirsize(d) result(isum)

	type(dir) :: d

	!********

	integer :: i
	integer(kind = 8) :: suml, isum

	suml = 0

	do i = 1, d%ndirs
		suml = suml + dirsize(d%dirs(i))
	end do

	do i = 1, d%nfiles
		suml = suml + d%files(i)%size
	end do

	!print *, 'size(', d%name, ') = ', suml
	d%size = suml

	isum = suml

end function dirsize

!===============================================================================

subroutine pushfile(d, f)

	class(dir), intent(inout) :: d

	type(file), intent(in) :: f

	d%nfiles = d%nfiles + 1

	if (d%nfiles > size(d%files)) then
		print *, 'Error: in pushfile, nfiles overflow'
		print *, 'Dir name  = ', d%name
		print *, 'File name = ', f%name
		stop
	end if

	d%files(d%nfiles) = f

end subroutine pushfile

!===============================================================================

subroutine pushdir(d, sub)

	!class(dir), pointer, intent(inout) :: d
	class(dir), target, intent(inout) :: d

	type(dir), intent(inout) :: sub

	d%ndirs = d%ndirs + 1

	!print *, 'ndirs = ', d%ndirs

	if (d%ndirs > size(d%dirs)) then
		print *, 'Error: in pushdir, ndirs overflow'
		print *, 'Dir name  = ', d%name
		print *, 'Subd name = ', sub%name
		stop
	end if

	!print *, 'associating parent'
	sub%parent => d

	!print *, 'adding subdir'
	d%dirs(d%ndirs) = sub

	!print *, 'done pushdir'

end subroutine pushdir

!===============================================================================

function new(name) result(d)

	type(dir) :: d

	character(len = *) :: name

	d%name = name
	allocate(d%dirs(nmax))

end function new

!===============================================================================

function readroot() result(r)

	character :: s*256
	character(len = :), allocatable :: junk, name, word, cmd, arg

	integer :: i, iu, io, is, size
	integer(kind = 8) :: isum

	!type(dir) :: r, sub !, cwd

	type(dir) :: sub
	type(dir), target  :: r
	type(dir), pointer :: cwd
	!type(dir) :: a

	open(file = finput, newunit = iu, status = 'old')

	isum = 0

	!! Manually test the directory tree data structure
	!r = new('/')
	!call r%pushfile(file('b.txt', 14848514))
	!call r%pushfile(file('c.dat', 8504156))
	!!call printdir(r)
	!a = new('a')
	!call r%pushdir(a)
	!call r%dirs(1)%pushdir(new('e'))
	!call r%dirs(1)%dirs(1)%pushfile(file('i', 584))
	!call r%pushdir(new('l'))
	!call r%pushdir(new('m'))
	!call r%pushdir(new('n'))
	!call printdir(r)
	!return

	read(iu, '(a)', iostat = io) s
	!print *, 's0 = ', trim(s)

	is = 1
	junk = readword(s, is)
	junk = readword(s, is)
	name = readword(s, is)

	r = new(name)
	cwd => r
	!allocate(cwd)
	!cwd = r

	outer: do
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		!print *, 's = ', trim(s)

		is = 1
		word = readword(s, is)

		if (word == '$') then
			cmd = readword(s, is)

			if (cmd == 'ls') then

				! Parse output of ls command
				do
					read(iu, '(a)', iostat = io) s
					if (io == iostat_end) exit outer

					!call printdir(r)

					is = 1
					word = readword(s, is)
					name = readword(s, is)

					!print *, 'word, name = ', word, ', ', name

					if (word == '$') then

						!call printdir(cwd)
						!call printdir(r)
						!return 

						backspace(iu)
						exit

					else if (word == 'dir') then
						!print *, 'dir'

						sub = new(name)

						!print *, 'calling pushdir'
						call cwd%pushdir(sub)

					else
						!print *, 'file'
						!print *, 'word = ', word

						read(word, *) size
						call cwd%pushfile(file(name, size))

					end if

				end do

			else if (cmd == 'cd') then

				arg = readword(s, is)
				!print *, 'arg = ', arg

				if (arg == '..') then
					cwd => cwd%parent
				else

					! Search for a subdir named arg in cwd.  It should have already been pushed
					i = 1
					do while (cwd%dirs(i)%name /= arg)
						i = i + 1

						if (i > cwd%ndirs) then
							print *, 'Error: cd to unknown dir "'//arg//'"'
							!call printdir(cwd)
							!call printdir(r)
							stop
						end if

					end do

					cwd => cwd%dirs(i)

				end if

			else
				print *, 'Error: unknown cmd "'//cmd//'"'
				stop
			end if

		else
		end if

	end do outer

	close(iu)

	!call printdir(r)

	!print *, 'returning'

end function readroot

!===============================================================================

subroutine part2()

	type(dir) :: r

	integer(kind = 8) :: isum, free, delete

	r = readroot()

	!call printdir(r)

	call get_sizes(r)
	!call printdir(r)

	!print *, 'calling sum_under'
	isum = sum_under(r, 100000)

	!print *, 'root size = ', r%size

	free = 70000000 - r%size

	!print *, 'free = ', free

	delete = 30000000 - free

	!print *, 'need to delete = ', delete

	isum = find_smallest(r, delete)

	print *, 'part 2 = ', isum
	print *, ''

end subroutine part2

!===============================================================================

subroutine part1()

	type(dir) :: r

	integer(kind = 8) :: isum

	r = readroot()

	!call printdir(r)

	call get_sizes(r)
	!call printdir(r)

	isum = sum_under(r, 100000)

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
	call part2()

	print *, 'Ending AOC main'
	print *, ''

end program main

!===============================================================================

