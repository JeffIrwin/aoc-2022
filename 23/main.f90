
!===============================================================================

module m

	use utils

	implicit none

#if 0
	character(len = *), parameter :: finput = 'test-input.txt'
#else
	character(len = *), parameter :: finput = 'input.txt'
#endif

	integer, parameter :: less = -1, equal = 0, greater = 1

	!interface operator (<)
	!	module procedure lt_lex
	!end interface
	!generic :: operator(<) => lt_lex

	! Cardinal directions.  c.f. day 12.  These are in CCW order: right,
	! up, left, down.  Note y is positive down!
	integer, parameter :: nd = 2, ndirs = 4
	integer, parameter :: dirs(nd,ndirs) = &
		reshape([  &
			  0, -1, &
			  0, +1, &
			 -1,  0, &
			 +1,  0  &
		], [nd,ndirs])

	integer, parameter :: north = 1, south = 2, west = 3, east = 4

contains

!===============================================================================

integer function compare_lex(a, b) result(compare)

	! Lexicographically compare int vectors a and b.  Return less if a < b,
	! equal if a == b, or greater if a > b.

	integer, intent(in) :: a(:), b(:)
	!integer, intent(in) :: a, b

	integer :: i

	if (size(a) /= size(b)) then
		write(*,*) 'Error: incompatible sizes of args in compare_lex()'
		stop
	end if

	do i = 1, size(a)
		if (a(i) < b(i)) then
			compare = less
			return
		else if (a(i) > b(i)) then
			compare = greater
			return
		end if
	end do

	compare = equal

end function compare_lex

!===============================================================================

logical function lt_lex(a, b) result(lt)

	integer, intent(in) :: a(:), b(:)

	lt = compare_lex(a, b) == less

end function lt_lex

!===============================================================================

logical function lte_lex(a, b) result(lte)

	integer, intent(in) :: a(:), b(:)
	integer :: comp

	comp = compare_lex(a, b)
	lte = comp == less .or. comp == equal

end function lte_lex

!===============================================================================

logical function gt_lex(a, b) result(gt)

	integer, intent(in) :: a(:), b(:)

	gt = compare_lex(a, b) == greater

end function gt_lex

!===============================================================================

logical function eq_lex(a, b) result(eq)

	integer, intent(in) :: a(:), b(:)

	eq = compare_lex(a, b) == equal

end function eq_lex

!===============================================================================

recursive subroutine sort(a, idx, lo_in, hi_in)

	! Quicksort a rank-2 array a along its 2nd dimension and return the
	! sort permutation idx

	integer, intent(in) :: a(:,:)
	integer, allocatable :: idx(:)
	integer, optional :: lo_in, hi_in

	integer :: lo, hi, p, i

	logical :: outer

	lo = 1
	hi = size(a, 2)
	outer = .true.

	if (present(lo_in)) lo = lo_in
	if (present(hi_in)) then
		hi = hi_in
		outer = .false.
	end if

	if (lo >= hi .or. lo < 1) return

	if (.not. allocated(idx)) then
		idx = [(i, i = 1, size(a, 2))]
	end if

	p = partition(a, idx, lo, hi)

	call sort(a, idx, lo, p - 1)
	call sort(a, idx, p + 1, hi)

	if (outer) then
		! Check result.  TODO debug only

		do i = 2, size(a, 2)
			if (lt_lex(a(:,idx(i)), a(:,idx(i-1)))) then
				write(*,*) 'Error in sort'
				stop
			end if
		end do

	end if

end subroutine sort

!===============================================================================

integer function partition(a, idx, lo, hi) result(ans)

	integer, intent(in) :: a(:,:)
	integer, allocatable :: idx(:), pivot(:)
	integer :: lo, hi, i, j, mid

	!pivot = a(:, idx(hi))

	mid = (lo + hi) / 2
	if (lt_lex(a(:, idx(mid)), a(:, idx(lo)))) then
		idx([lo, mid]) = idx([mid, lo])
	else if (lt_lex(a(:, idx(hi)), a(:, idx(lo)))) then
		idx([lo, hi]) = idx([hi, lo])
	else if (lt_lex(a(:, idx(mid)), a(:, idx(hi)))) then
		idx([mid, hi]) = idx([hi, mid])
	end if
	pivot = a(:, idx(hi))

	i = lo - 1
	do j = lo, hi - 1
	!do j = lo, hi
		if (lte_lex(a(:, idx(j)), pivot)) then
			i = i + 1
			idx([i, j]) = idx([j, i])
		end if
	end do

	i = i + 1
	idx([i, hi]) = idx([hi, i])
	ans = i

end function partition

!===============================================================================

subroutine binary_search(a, idx, t, il, ir)

	! Search array a for target t.  Return indices il and ir which bound
	! target (or are equal if target is in a)

	integer, intent(in) :: a(:,:), idx(:)
	integer, intent(in) :: t(:)
	integer :: il, ir, m

	il = 1
	ir = size(a, 2)

	do while (il <= ir)
		m = (il + ir) / 2

		if (lt_lex(a(:, idx(m)), t)) then
			il = m + 1
		else if (gt_lex(a(:, idx(m)), t)) then
			ir = m - 1
		else
			! Found it
			il = m
			ir = m
			return
		end if

	end do

	if (il == ir) then
		write(*,*) 'Error in binary_search'
		stop
	end if

	if (il > ir) then
		m = il
		il = ir
		ir = m
	end if

	!print *, 'unsuccesful'
	!print *, 'il, ir = ', il, ir

end subroutine binary_search

!===============================================================================

logical function in_array(a, idx, t)

	! Is target row vector t(:) in previously sorted array a(:,:)?

	integer, intent(in) :: a(:,:), idx(:), t(:)
	integer :: il, ir

	call binary_search(a, idx, t, il, ir)
	in_array = il == ir

end function in_array

!===============================================================================

logical function is_unique(a, idx, i)

	! Is row i a(:,i) in previously sorted array a unique?

	integer, intent(in) :: a(:,:), idx(:), i

	if (i == 1) then
		is_unique = .not. eq_lex(a(:,idx(1)), a(:,idx(2)))
	else if (i == size(a,2)) then
		is_unique = .not. eq_lex(a(:,idx(i-1)), a(:,idx(i)))
	else
		is_unique = (.not. eq_lex(a(:,idx(i-1)), a(:,idx(i)))) .and. &
		            (.not. eq_lex(a(:,idx(i+1)), a(:,idx(i))))
	end if

end function is_unique

!===============================================================================

function readinput() result(pos)

	character :: s*256

	integer :: iu, io, x, y, npos
	integer, allocatable :: pos(:,:)

	open(file = finput, newunit = iu, status = 'old')

	! First pass: count elf positions
	npos = 0
	do
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		!print *, 's = ', trim(s)

		do x = 1, len_trim(s)
			if (s(x:x) == '#') then
				npos = npos + 1
			end if
		end do

	end do
	rewind(iu)

	!print *, 'npos = ', npos

	! Second pass: save elf positions
	allocate(pos(nd, npos))
	npos = 0
	y = 0
	do
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		y = y + 1

		do x = 1, len_trim(s)
			if (s(x:x) == '#') then
				npos = npos + 1
				pos(:,npos) = [x, y]
			end if
		end do

	end do

	close(iu)

end function readinput

!===============================================================================

subroutine part1()

	integer :: isum, i, j, il, ir, npos, dir0, p(nd), x, y, idir, it
	integer, parameter :: nrounds = 10

	integer, allocatable :: pos(:,:), idx(:), prop(:,:)

	logical :: l(-1:1, -1:1), l3(3)

	isum = 0

	pos = readinput()
	npos = size(pos, 2)
	allocate(prop(nd, npos))

	!print *, 'pos = '
	!print '(2i4)', pos

	call sort(pos, idx)

	!print *, 'sorted pos = '
	!do i = 1, npos
	!	print '(i4,a,2i4)', i, ':', pos(:, idx(i))
	!end do

	!! Unit testing :)
	!!call binary_search(pos, idx, [6,4], il, ir)
	!call binary_search(pos, idx, [10,9], il, ir)
	!print *, 'il, ir = ', il, ir

	! Index of first direction to check
	dir0 = 0

	! Time loop
	do it = 1, nrounds

		dir0 = wrap(dir0 + 1, ndirs)
		!print *, 'dir0 = ', dir0

		! First half of round: propose new positions

		! Elf loop
		do i = 1, npos

			! Are any other elves in the 3x3 grid centered on the current elf's
			! position?  It's a bit redundant to check the center position but
			! idc
			l = .false.
			do y = -1, 1
				do x = -1, 1

					p = pos(:,i) + [x,y]
					l(x,y) = in_array(pos, idx, p)

				end do
			end do

			!print *, 'Elf ', i, ':', pos(:,i)
			!print *, 'l ='
			!print '(3l)', l

			! If no other Elves are in one of those eight positions, the Elf does not
			! do anything during this round (i.e. it stays in its current position)
			if (count(l) == 1) then
				!print *, 'no other elves'
				prop(:,i) = pos(:,i)
				cycle
			end if

			! Look in each of 4 directions and propose the first valid one
			do j = 0, ndirs - 1
				idir = wrap(dir0 + j, ndirs)

				! Remember y is positive south!
				if      (idir == north) then
					!print *, 'north'
					l3 = l(:,-1)

				else if (idir == south) then
					!print *, 'south'
					l3 = l(:,+1)

				else if (idir == west ) then
					!print *, 'west'
					l3 = l(-1,:)

				else if (idir == east ) then
					!print *, 'east'
					l3 = l(+1,:)

				end if

				! If there is no other elf, propose moving in that direction
				if (.not. any(l3)) then
					prop(:,i) = pos(:,i) + dirs(:,idir)
					!print *, 'Elf ', i, ': dir ', idir
					exit
				end if

			end do

		end do  ! elf loop

		! Second half of round: simultaneously, each Elf moves to their proposed
		! destination tile if they were the only Elf to propose moving to that
		! position

		!print *, 'prop = '
		!print '(2i4)', prop

		! Sort the proposed array to check uniqueness.  Not we're re-using idx now,
		! so it can no longer be used for pos array
		deallocate(idx)
		call sort(prop, idx)
		!print *, 'prop sorted = '
		!print '(2i4)', prop(:,idx)

		! Elf loop
		do i = 1, npos

			!if (.not. is_unique(prop, idx, i)) then
			!	! Return to previous position
			!	prop(:,idx(i)) = pos(:,idx(i))
			!	print *, 'clash elf ', idx(i)
			!end if

			if (is_unique(prop, idx, i)) then
				! Update position to proposal
				!prop(:,idx(i)) = pos(:,idx(i))
				pos(:,idx(i)) = prop(:,idx(i))
				!print *, 'clash elf ', idx(i)
			end if

		end do

		!pos = prop

		!print *, 'pos = '
		!print '(2i4)', pos

		! Re-sort at each iter.  This is a pre-requisite for binary searching with
		! in_array()
		deallocate(idx)
		call sort(pos, idx)

		!print '(a,i0,a)', '== End of Round ', it, ' =='
		!do y = minval(pos(2,:)), maxval(pos(2,:))
		!	do x = minval(pos(1,:)), maxval(pos(1,:))
		!		if (in_array(pos, idx, [x,y])) then
		!			write(*, '(a)', advance = 'no') '#'
		!		else
		!			write(*, '(a)', advance = 'no') '.'
		!		end if
		!	end do
		!	write(*,*)
		!end do
		!write(*,*)

		!if (it == 2) exit

	end do  ! time loop

	do y = minval(pos(2,:)), maxval(pos(2,:))
		do x = minval(pos(1,:)), maxval(pos(1,:))
			if (.not. in_array(pos, idx, [x,y])) then
				isum = isum + 1
			end if
		end do
	end do

	write(*,*) 'part1 = ', isum
	write(*,*) ''

end subroutine part1

!===============================================================================

subroutine part2()

	integer :: isum, i, j, il, ir, npos, dir0, p(nd), x, y, idir, it

	integer, allocatable :: pos(:,:), idx(:), prop(:,:), pos0(:,:)

	logical :: l(-1:1, -1:1), l3(3)

	isum = 0

	pos = readinput()
	npos = size(pos, 2)
	allocate(prop(nd, npos))

	call sort(pos, idx)

	! Index of first direction to check
	dir0 = 0

	! Time loop
	it = 0
	do !it = 1, 10
		it = it + 1
		print *, 'it = ', it

		pos0 = pos
		dir0 = wrap(dir0 + 1, ndirs)
		!print *, 'dir0 = ', dir0

		! First half of round: propose new positions

		! Elf loop
		do i = 1, npos

			! Are any other elves in the 3x3 grid centered on the current elf's
			! position?  It's a bit redundant to check the center position but
			! idc
			l = .false.
			do y = -1, 1
				do x = -1, 1

					p = pos(:,i) + [x,y]
					l(x,y) = in_array(pos, idx, p)

				end do
			end do

			! If no other Elves are in one of those eight positions, the Elf does not
			! do anything during this round (i.e. it stays in its current position)
			if (count(l) == 1) then
				prop(:,i) = pos(:,i)
				cycle
			end if

			! Look in each of 4 directions and propose the first valid one
			do j = 0, ndirs - 1
				idir = wrap(dir0 + j, ndirs)

				! Remember y is positive south!
				if      (idir == north) then
					l3 = l(:,-1)
				else if (idir == south) then
					l3 = l(:,+1)
				else if (idir == west ) then
					l3 = l(-1,:)
				else if (idir == east ) then
					l3 = l(+1,:)
				end if

				! If there is no other elf, propose moving in that direction
				if (.not. any(l3)) then
					prop(:,i) = pos(:,i) + dirs(:,idir)
					exit
				end if

			end do

		end do  ! elf loop

		! Second half of round: simultaneously, each Elf moves to their proposed
		! destination tile if they were the only Elf to propose moving to that
		! position

		! Sort the proposed array to check uniqueness.  Note we're re-using idx now,
		! so it can no longer be used for pos array
		deallocate(idx)
		call sort(prop, idx)

		! Elf loop
		do i = 1, npos
			if (is_unique(prop, idx, i)) then
				! Update position to proposal
				pos(:,idx(i)) = prop(:,idx(i))
			end if
		end do

		! Re-sort at each iter.  This is a pre-requisite for binary searching with
		! in_array()
		deallocate(idx)
		call sort(pos, idx)

		! TODO: debug only
		do i = 1, npos
			if (.not. is_unique(pos, idx, i)) then
				write(*,*) 'Error in part2'
				stop
			end if
		end do

		!print '(a,i0,a)', '== End of Round ', it, ' =='
		!do y = minval(pos(2,:)), maxval(pos(2,:))
		!	do x = minval(pos(1,:)), maxval(pos(1,:))
		!		if (in_array(pos, idx, [x,y])) then
		!			write(*, '(a)', advance = 'no') '#'
		!		else
		!			write(*, '(a)', advance = 'no') '.'
		!		end if
		!	end do
		!	write(*,*)
		!end do
		!write(*,*)

		!if (it == 10) exit
		if (all(pos == pos0)) exit

	end do  ! time loop

	print *, 'size pos pos0 = ', size(pos), size(pos0)

	do y = minval(pos(2,:)), maxval(pos(2,:))
		do x = minval(pos(1,:)), maxval(pos(1,:))
			if (.not. in_array(pos, idx, [x,y])) then
				isum = isum + 1
			end if
		end do
	end do
	print *, 'isum = ', isum

	! 919 is too low

	!print *, 'pos sorted = '
	!print '(2i4)', pos(:,idx)

	write(*,*) 'part2 = ', it
	write(*,*) ''

end subroutine part2

!===============================================================================

end module m

!===============================================================================

program main

	use m

	write(*,*) 'Starting AOC main'
	write(*,*) 'Input file = ', finput
	write(*,*) ''

	call part1()
	call part2()

	write(*,*) 'Ending AOC main'
	write(*,*) ''

end program main

!===============================================================================

