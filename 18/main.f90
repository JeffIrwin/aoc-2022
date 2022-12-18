
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

subroutine part1()

	character :: s*256

	integer :: iu, io, is, xmin, xmax, ymin, ymax, zmin, zmax, &
			x, y, z, area

	logical, allocatable :: lxyz(:,:,:)

	open(file = finput, newunit = iu, status = 'old')

	xmin =  huge(xmin)
	ymin =  huge(xmin)
	zmin =  huge(xmin)
	xmax = -huge(xmin)
	ymax = -huge(xmin)
	zmax = -huge(xmin)

	! First pass: get bounds
	do
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit
		!print *, 's = ', trim(s)

		is = 1
		x = readint(s, is)
		y = readint(s, is)
		z = readint(s, is)

		xmin = min(xmin, x)
		ymin = min(ymin, y)
		zmin = min(zmin, z)
		xmax = max(xmax, x)
		ymax = max(ymax, y)
		zmax = max(zmax, z)

	end do

	!print *, 'x in ', xmin, xmax
	!print *, 'y in ', ymin, ymax
	!print *, 'z in ', zmin, zmax

	allocate(lxyz(xmin:xmax, ymin:ymax, zmin:zmax))
	lxyz = .false.

	! Second pass: save elements
	rewind(iu)
	do
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit
		!print *, 's = ', trim(s)

		is = 1
		x = readint(s, is)
		y = readint(s, is)
		z = readint(s, is)

		!print *, 'xyz = ', x, y, z

		if (lxyz(x,y,z)) print *, 'duplicate'
		lxyz(x,y,z) = .true.

	end do
	close(iu)

	!print *, 'lxyz = ', lxyz

	! Initialize area as if no faces are shared
	area = 6 * count(lxyz)

	!print *, 'count = ', count(lxyz)
	!print *, 'unshared area = ', area

	do z = zmin, zmax
	do y = ymin, ymax
	do x = xmin, xmax

		! Subtract shared faces.  Each shared face counts for both elements
		! which share it, so subtract 2
		!
		! Voxel elements which share only an edge or a corner aren't special

		! x face
		if (x>xmin) then
			if (lxyz(x-1, y, z) .and. lxyz(x, y, z)) area = area - 2
		end if

		! y face
		if (y>ymin) then
			if (lxyz(x, y-1, z) .and. lxyz(x, y, z)) area = area - 2
		end if

		! z face
		if (z>zmin) then
			if (lxyz(x, y, z-1) .and. lxyz(x, y, z)) area = area - 2
		end if

	end do
	end do
	end do

	write(*,*) 'part1 = ', area
	write(*,*) ''

end subroutine part1

!===============================================================================

subroutine part2()

	character :: s*256

	integer :: iu, io, is, xmin, xmax, ymin, ymax, zmin, zmax, &
			x, y, z, area, voltot

	logical, allocatable :: lxyz(:,:,:), lext(:,:,:)

	open(file = finput, newunit = iu, status = 'old')

	xmin =  huge(xmin)
	ymin =  huge(xmin)
	zmin =  huge(xmin)
	xmax = -huge(xmin)
	ymax = -huge(xmin)
	zmax = -huge(xmin)

	! First pass: get bounds
	do
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit
		!print *, 's = ', trim(s)

		is = 1
		x = readint(s, is)
		y = readint(s, is)
		z = readint(s, is)

		xmin = min(xmin, x)
		ymin = min(ymin, y)
		zmin = min(zmin, z)
		xmax = max(xmax, x)
		ymax = max(ymax, y)
		zmax = max(zmax, z)

	end do

	!print *, 'x in ', xmin, xmax
	!print *, 'y in ', ymin, ymax
	!print *, 'z in ', zmin, zmax

	! Pad by 1 due to reasons for part2: it's easier if lxyz and lext have the
	! same bounds.  We need to guarantee that the exterior covers the lava
	! droplet on all sides
	xmin = xmin - 1
	ymin = ymin - 1
	zmin = zmin - 1
	xmax = xmax + 1
	ymax = ymax + 1
	zmax = zmax + 1

	allocate(lxyz(xmin:xmax, ymin:ymax, zmin:zmax))
	lxyz = .false.

	! Second pass: save elements
	rewind(iu)
	do
		read(iu, '(a)', iostat = io) s
		if (io == iostat_end) exit
		!print *, 's = ', trim(s)

		is = 1
		x = readint(s, is)
		y = readint(s, is)
		z = readint(s, is)

		!print *, 'xyz = ', x, y, z

		if (lxyz(x,y,z)) print *, 'duplicate'
		lxyz(x,y,z) = .true.

	end do
	close(iu)
	!print *, 'lxyz = ', lxyz

	! Make a second array which extends beyond lxyz by 1 in each direction
	allocate(lext(xmin:xmax, ymin:ymax, zmin:zmax))
	lext = .false.

	! Mark the exterior by flood filling.  Start at the min corner, which is
	! guarenteed to be exterior
	call dfs(lext, lxyz, xmin, ymin, zmin)

	! Get area of "exterior" using a method like from part1.  This could use
	! some DRYing up

	! Initialize area as if no faces are shared
	area = 6 * count(lext)

	!print *, 'count = ', count(lext)
	!print *, 'unshared area = ', area

	! Total bounding box volume, including lava droplets, air pockets, and
	! exterior
	voltot = (xmax-xmin+1) * (ymax-ymin+1) * (zmax-zmin+1)

	!print *, 'Total volume = ', voltot
	!print *, 'air pocket volume = ', voltot - count(lxyz) - count(lext)

	do z = zmin, zmax
	do y = ymin, ymax
	do x = xmin, xmax

		if (x>xmin) then
			if (lext(x-1, y, z) .and. lext(x, y, z)) area = area - 2
		end if
		if (y>ymin) then
			if (lext(x, y-1, z) .and. lext(x, y, z)) area = area - 2
		end if
		if (z>zmin) then
			if (lext(x, y, z-1) .and. lext(x, y, z)) area = area - 2
		end if

	end do
	end do
	end do

	! Subtract outer shell area of "exterior" at the bounding box boundary
	area = area - 2 * (             &
		(xmax-xmin+1) * (ymax-ymin+1) + &
		(ymax-ymin+1) * (zmax-zmin+1) + &
		(zmax-zmin+1) * (xmax-xmin+1))

	write(*,*) 'part2 = ', area
	write(*,*) ''

end subroutine part2

!===============================================================================

recursive subroutine dfs(lext, lxyz, x, y, z)

	logical, allocatable :: lext(:,:,:), lxyz(:,:,:)

	integer :: x, y, z

	! Out of bounds
	if (x < lbound(lext,1)) return
	if (x > ubound(lext,1)) return
	if (y < lbound(lext,2)) return
	if (y > ubound(lext,2)) return
	if (z < lbound(lext,3)) return
	if (z > ubound(lext,3)) return

	! Already visited
	if (lext(x, y, z)) return

	! Not exterior if in lava droplet
	if (lxyz(x, y, z)) return

	! Mark exterior point visited
	lext(x, y, z) = .true.
	!print *, 'dfs'

	! Try neihbors.  Never expand diagonally
	call dfs(lext, lxyz, x-1, y  , z  )
	call dfs(lext, lxyz, x  , y-1, z  )
	call dfs(lext, lxyz, x  , y  , z-1)
	call dfs(lext, lxyz, x+1, y  , z  )
	call dfs(lext, lxyz, x  , y+1, z  )
	call dfs(lext, lxyz, x  , y  , z+1)

end subroutine dfs

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

