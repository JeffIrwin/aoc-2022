
!===============================================================================

module m

	use utils

	implicit none

#if 0
	character(len = *), parameter :: finput  = 'test-input.txt'
	character(len = *), parameter :: finput2 = 'test2-input.txt'
#else
	character(len = *), parameter :: finput  = 'input.txt'
	character(len = *), parameter :: finput2 = 'input.txt'
#endif

contains

!===============================================================================

subroutine part2()

	character :: s*256, dc

	integer, parameter :: nk = 10, nmax = 256

	integer :: iu, io, isum, ik, il, l, h(2,nk), h0(2,nk), d(2), &
		xl, xh, yl, yh, ix, iy, dk(2), xli, xhi, yli, yhi

	! Positions visited by tail
	logical ::  v(-nmax: nmax, -nmax: nmax)
	integer :: iv(-nmax: nmax, -nmax: nmax)

	! Cardinal directions
	integer, parameter :: &
		dl(2) = [-1,  0], &
		dr(2) = [+1,  0], &
		dd(2) = [ 0, -1], &
		du(2) = [ 0, +1]

	open(file = finput2, newunit = iu, status = 'old')

	isum = 0

	!print *, 'dl = ', dl

	! Initial rope positions (all knots at origin)
	h = 0
	h0 = h

	! Bounds for visualization
	xl = 0
	xh = 0
	yl = 0
	yh = 0
	xli = 0
	xhi = 0
	yli = 0
	yhi = 0

	v = .false.

	do
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		!print *, 's = ', trim(s)

		dc = s(1:1)
		read(s(2:), *) l

		!print *, 'dc, l = ', dc, l

		! Direction of head motion
		if      (dc == 'L') then
			d = dl
		else if (dc == 'R') then
			d = dr
		else if (dc == 'D') then
			d = dd
		else if (dc == 'U') then
			d = du
		else
			print *, 'Error: unknown direction "', dc, '"'
			stop
		end if

		! Move the head 1 position at a time
		do il = 1, l

			! Rope position from previous time step (all knots)
			h0 = h

			! Move the head in the prescribed direction
			h(:,1) = h(:,1) + d

			!print *, 'h(:,1) = ', h(:,1)

			! Drag each knot behind the segment ahead of it
			do ik = 2, nk
				if (     abs(h(1,ik-1) - h(1,ik)) > 1 &
				    .or. abs(h(2,ik-1) - h(2,ik)) > 1) then

					! Find the direction of the leading knot
					dk = h(:,ik-1) - h0(:,ik-1)
					!dk = h(:,ik-1) - h(:,ik)

					!! Get the sign only (move 1, not >=2)
					!dk(1) = sign(1, dk(1))
					!dk(2) = sign(1, dk(2))

					if (dk(1) /= 0 .and. dk(2) /= 0) then
						! Simplify diagonal motion to an axis-aligned motion

						if (abs(dk(1)) > abs(dk(2))) then

							! I think these two conditional branches are dead
							! code, but I'm afraid to touch it

							! Primary motion in x
							dk(2) = 0
							dk(1) = sign(1, dk(1))
						else if (abs(dk(2)) > abs(dk(1))) then
							! Primary motion in y
							dk(1) = 0
							dk(2) = sign(1, dk(2))
						else

							!print *, 'Error: total diagonal motion'
							!stop

							! In diagonal motion, the trailing knot maintains
							! it's configuration relative to the leading knot
							dk = h0(:,ik-1) - h(:,ik)

							! Unless that motion is also diagonal
							if (dk(1) /= 0 .and. dk(2) /= 0) then
								dk = h(:,ik-1) - h(:,ik)
								if (abs(dk(1)) > abs(dk(2))) then
									! Primary motion in x
									dk(2) = 0
									dk(1) = sign(1, dk(1))
								else if (abs(dk(2)) > abs(dk(1))) then
									! Primary motion in y
									dk(1) = 0
									dk(2) = sign(1, dk(2))
								else
									! This is the case where both knots are
									! moving diagonally in the same direction
									dk = h0(:,ik-1) - h(:,ik)
								end if
							end if

						end if

					end if

					!t = h - d
					!h(:,ik) = h(:,ik-1) - d
					h(:,ik) = h(:,ik-1) - dk

				end if
			end do

			!print *, 'h = ', h

			if (any(abs(h(:,nk)) > nmax)) then
				print *, 'Error: tail overflowed grid dimensions'
				stop
			end if

			xl = min(xl, h(1,nk))
			xh = max(xh, h(1,nk))
			yl = min(yl, h(2,nk))
			yh = max(yh, h(2,nk))

			xli = min(xli, minval(h(1,:)))
			xhi = max(xhi, maxval(h(1,:)))
			yli = min(yli, minval(h(2,:)))
			yhi = max(yhi, maxval(h(2,:)))

			v(h(1,nk), h(2,nk)) = .true.

		! Populate current state of rope for visualization
		iv = 0
		do ik = nk, 1, -1
			iv(h(1,ik), h(2,ik)) = ik
		end do

		!! Print current state of rope
		!do iy = yhi, yli, -1
		!do ix = xli, xhi
		!	if (ix == 0 .and. iy == 0) then
		!		write(*, '(a)', advance = 'no') 's'
		!	else if (iv(ix,iy) > 0) then
		!		write(*, '(i0)', advance = 'no') iv(ix,iy) - 1
		!	else
		!		write(*, '(a)', advance = 'no') '.'
		!	end if
		!end do
		!write(*,*)
		!end do
		!write(*,*)

		end do
	end do

	close(iu)

	!! Visualize the tail visit locations
	!print *, 'xl, xh, yl, yh = ', xl, xh, yl, yh
	!do iy = yh, yl, -1
	!do ix = xl, xh
	!	if (ix == 0 .and. iy == 0) then
	!		write(*, '(a)', advance = 'no') 's'
	!	else if (v(ix,iy)) then
	!		write(*, '(a)', advance = 'no') '#'
	!	else
	!		write(*, '(a)', advance = 'no') '.'
	!	end if
	!end do
	!write(*,*)
	!end do

	print *, 'part 2 = ', count(v)
	print *, ''

end subroutine part2

!===============================================================================

subroutine part1()

	character :: s*256, dc

	integer :: iu, io, isum, il, l, h(2), t(2), d(2)

	integer, parameter :: nmax = 256

	! Positions visited by tail
	logical :: v(-nmax: nmax, -nmax: nmax)

	! Cardinal directions
	integer, parameter :: &
		dl(2) = [-1,  0], &
		dr(2) = [+1,  0], &
		dd(2) = [ 0, -1], &
		du(2) = [ 0, +1]

	open(file = finput, newunit = iu, status = 'old')

	isum = 0

	!print *, 'dl = ', dl

	h = 0
	t = h

	v = .false.

	do
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit

		!print *, 's = ', trim(s)

		dc = s(1:1)
		read(s(2:), *) l

		!print *, 'dc, l = ', dc, l

		if      (dc == 'L') then
			d = dl
		else if (dc == 'R') then
			d = dr
		else if (dc == 'D') then
			d = dd
		else if (dc == 'U') then
			d = du
		else
			print *, 'Error: unknown direction "', dc, '"'
			stop
		end if

		! Move the head 1 position at a time
		do il = 1, l

			h = h + d

			!print *, 'h = ', h

			if (abs(h(1) - t(1)) > 1 .or. abs(h(2) - t(2)) > 1) then
				t = h - d
			end if

			!print *, 't = ', t

			if (any(abs(t) > nmax)) then
				print *, 'Error: tail overflowed grid dimensions'
				stop
			end if

			v(t(1), t(2)) = .true.

		end do

	end do

	close(iu)

	print *, 'part 1 = ', count(v)
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

