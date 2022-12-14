
!===============================================================================

module m

	use utils

	implicit none

#if 0
	character(len = *), parameter :: finput = 'test-input.txt'
#else
	character(len = *), parameter :: finput = 'input.txt'
#endif

	integer, parameter :: nmax = 256

	! A packet is a list
	type list
		type(val), pointer :: vals(:)
		integer :: nvals = 0
	end type list

	! Each value in a list is either an integer (> 0) or another list
	type val
		integer :: int = -1
		type(list) :: list
	end type val

	interface operator (<)
		module procedure lt_list
	end interface

	interface operator (>)
		module procedure gt_list
	end interface

	interface operator (==)
		module procedure eq_list
	end interface

	interface operator (<=)
		module procedure lte_list
	end interface

	interface operator (>=)
		module procedure gte_list
	end interface

contains

!===============================================================================

logical recursive function lt_list(pl, pr) result(lt)

	type(list), intent(in) :: pl, pr

	type(list) :: ll, lr, ptmp

	integer :: i, nl, nr, il, ir

	!write(*, '(a)', advance = 'no') 'Compare '
	!call printlist(pl)
	!write(*, '(a)', advance = 'no') ' vs '
	!call printlist(pr)
	!print *, ''

	lt = .false.

	nl = pl%nvals
	nr = pr%nvals
	do i = 1, min(nl, nr)

		il = pl%vals(i)%int
		ir = pr%vals(i)%int

		if (il >= 0 .and. ir >= 0) then

			! Both ints
			if (il < ir) then
				lt = .true.
				return
			else if (ir < il) then
				lt = .false.
				return
			end if

		else if (il >= 0) then

			! Left is int, right is list
			allocate(ptmp%vals(nmax))
			ptmp%nvals = 1
			ptmp%vals(1)%int = il

			lr = pr%vals(i)%list

			if (ptmp < lr) then
				lt = .true.
				return
			else if (lr < ptmp) then
				lt = .false.
				return
			end if

		else if (ir >= 0) then

			! Right is int, left is list
			allocate(ptmp%vals(nmax))
			ptmp%nvals = 1
			ptmp%vals(1)%int = ir

			ll = pl%vals(i)%list

			if (ll < ptmp) then
				lt = .true.
				return
			else if (ptmp < ll) then
				lt = .false.
				return
			end if

		else

			! Both lists
			ll = pl%vals(i)%list
			lr = pr%vals(i)%list

			if (ll < lr) then
				lt = .true.
				return
			else if (lr < ll) then
				lt = .false.
				return
			end if

		end if

	end do

	if (nl < nr) then
		lt = .true.
		return
	else if (nr < nl) then
		lt = .false.
		return
	end if

	! Equal.  Could optimize eq_list() by returning an extra bit here
	lt = .false.
	return

end function lt_list

!===============================================================================

! Implement all other comparison functions in terms of lt_list()

logical function gt_list(pl, pr) result(gt)

	type(list), intent(in) :: pl, pr

	gt = pr < pl

end function gt_list

logical function eq_list(pl, pr) result(eq)

	type(list), intent(in) :: pl, pr

	eq = .not. (pl < pr .or. pl > pr)

end function eq_list

logical function lte_list(pl, pr) result(lte)

	type(list), intent(in) :: pl, pr

	lte = pl < pr .or. pl == pr

end function lte_list

logical function gte_list(pl, pr) result(gte)

	type(list), intent(in) :: pl, pr

	gte = pl > pr .or. pl == pr

end function gte_list

!===============================================================================

recursive subroutine printlist(p, level)

	type(list) :: p

	integer, optional :: level

	integer :: l, iv

	l = 0
	if (present(level)) l = level

	write(*, '(a)', advance = 'no') '['
	do iv = 1, p%nvals
		if (p%vals(iv)%int >= 0) then
			write(*, '(i0)', advance = 'no') p%vals(iv)%int
		else
			call printlist(p%vals(iv)%list, l + 1)
		end if
	  if (iv < p%nvals) write(*, '(a)', advance = 'no') ','
	end do
	write(*, '(a)', advance = 'no') ']'

	!if (l == 0) write(*,*)

end subroutine printlist

!===============================================================================

recursive function readlist(s, is0) result(p)

	character(len = *) :: s

	integer :: is, ns, iv
	integer, optional :: is0

	type(list) :: p

	!print *, 's = ', s

	ns = len_trim(s)
	!print *, 'ns = ', ns

	if (s(1:1) /= '[' .or. s(ns:ns) /= ']') then
		write(*,*) 'Error: malformed list "'//s//'"'
		stop
	end if

	allocate(p%vals(nmax))

	is = 1
	if (present(is0)) is = is0

	iv = 0

	!print *, 'is, ns = ', is, ns

	do while (is < ns)
		is = is + 1
		if (s(is:is) == ']') exit

		!print *, 'is = ', is

		iv = iv + 1

		if (isnum(s(is:is))) then

			if (iv > nmax) then
				write(*,*) 'Error: list overflow on int'
				stop
			end if

			p%vals(iv)%int = readint(s, is)

			! Backspace because loop will add anyway.  This could probably be
			! refactored
			is = is - 1

			!print *, 'num = ', p%vals(iv)%int

		else if (s(is:is) == '[') then
			!print *, 'not num'

			if (iv > nmax) then
				write(*,*) 'Error: list overflow on sublist'
				stop
			end if

			!print *, 'reading sublist'
			!print *, 'is = ', is
			p%vals(iv)%list = readlist(s, is)

			!print *, 'returned'
			!print *, 'is = ', is

		else if (s(is:is) == ',') then
			! Again, a lot of this could be refactored cleaner, but it works
			! :)
			iv = iv - 1
		end if

	end do
	p%nvals = iv

	! Set out arg if it was provided so caller can pick up after reading
	! a sublist
	if (present(is0)) is0 = is

	!call printlist(p)

end function readlist

!===============================================================================

subroutine bubblesort_packets(p)

	! O(n^2) cause idgaf

	type(list), allocatable :: p(:)

	integer :: i, np

	logical :: swapped

	np = size(p)
	!print *, 'np = ', np

	do

		swapped = .false.
		do i = 2, np

			if (p(i-1) > p(i)) then
				p([i-1, i]) = p([i, i-1])
				swapped = .true.
			end if
		end do

		if (.not. swapped) exit

	end do

end subroutine bubblesort_packets

!===============================================================================

subroutine part2()

	character :: s*256

	integer :: iu, io, isum, ip, np, npmax, i1, i2

	type(list) :: p1, p2
	type(list), allocatable :: buffer(:), packets(:)

	open(file = finput, newunit = iu, status = 'old')

	isum = 0

	npmax = 512
	allocate(buffer(npmax))

	ip = 0
	do
		ip = ip + 1

		if (ip + 2 > npmax) then
			write(*,*) 'Error: buffer overflow in part2'
			stop
		end if

		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit
		!print *, 's = ', trim(s)

		if (len_trim(s) == 0) then
			ip = ip - 1
		else
			buffer(ip) = readlist(trim(s))
		end if

	end do
	ip = ip - 1

	close(iu)

	! Insert divider packets [[2]] and [[6]]
	np = ip + 2
	allocate(packets(np))
	packets(1: np - 2) = buffer(1: np - 2)

	! It's easiest to assign a packet literal by reading from string
	p1 = readlist('[[2]]')
	p2 = readlist('[[6]]')

	packets(np - 1) = p1
	packets(np - 0) = p2

	call bubblesort_packets(packets)

	do ip = 1, np

		!call printlist(packets(ip))
		!print *, ''

		if (packets(ip) == p1) i1 = ip
		if (packets(ip) == p2) i2 = ip

	end do

	isum = i1 * i2

	write(*,*) 'part2 = ', isum
	write(*,*) ''

end subroutine part2

!===============================================================================

subroutine part1()

	character :: s*256

	integer :: iu, io, isum, ip

	type(list) :: p1, p2

	logical :: lt

	open(file = finput, newunit = iu, status = 'old')

	isum = 0

	ip = 0
	do
		ip = ip + 1

		!print '(a,i0,a)', '== Pair ', ip, ' =='

		! Packet 1
		read(iu, '(a)', iostat = io) s
		!print *, 's = ', trim(s)
		p1 = readlist(trim(s))

		! Packet 2
		read(iu, '(a)', iostat = io) s
		!print *, 's = ', trim(s)
		p2 = readlist(trim(s))

		!print *, ''
		!print *, 'p1 = '
		!call printlist(p1)
		!print *, 'p2 = '
		!call printlist(p2)
		!print *, ''

		!if (ip == 2) exit

		lt = p1 < p2
		if (lt) then
			!print *, 'Right order'
			isum = isum + ip
		end if
		!print *, ''

		! Blank line
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

	end do

	close(iu)

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

