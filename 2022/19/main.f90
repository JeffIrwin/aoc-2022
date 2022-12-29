
!===============================================================================

module m

	use utils

	implicit none

#if 1
	character(len = *), parameter :: finput = 'test-input.txt'
#else
	character(len = *), parameter :: finput = 'input.txt'
#endif

	! Blueprint
	type bp
		integer :: ore_cost_ore, cla_cost_ore, obs_cost_ore, obs_cost_cla, &
				geo_cost_ore, geo_cost_obs
	end type bp

contains

!===============================================================================

function readinput() result(bps)

	character :: s*256

	integer :: i, is, iu, io, n, junk

	type(bp), allocatable :: bps(:)

	n = countlines(finput)

	allocate(bps(n))

	open(file = finput, newunit = iu, status = 'old')

	do i = 1, n
		read(iu, '(a)', iostat = io) s

		!print *, 's = ', trim(s)

		is = 1
		junk = readint(s, is)

		bps(i)%ore_cost_ore = readint(s, is)
		bps(i)%cla_cost_ore = readint(s, is)
		bps(i)%obs_cost_ore = readint(s, is)
		bps(i)%obs_cost_cla = readint(s, is)
		bps(i)%geo_cost_ore = readint(s, is)
		bps(i)%geo_cost_obs = readint(s, is)

	end do

	close(iu)

	print *, 'Blueprints = '
	do i = 1, n
		print '(6i4)',           &
				bps(i)%ore_cost_ore, &
				bps(i)%cla_cost_ore, &
				bps(i)%obs_cost_ore, &
				bps(i)%obs_cost_cla, &
				bps(i)%geo_cost_ore, &
				bps(i)%geo_cost_obs
	end do

end function readinput

!===============================================================================

subroutine part1()

	character :: s*256

	integer :: i, it, isum, nore, ncla, nobs, ngeo, nore_bot, ncla_bot, &
			nobs_bot, ngeo_bot, nore0, ncla0, nobs0, ngeo0, nore_bot_add, &
			ncla_bot_add, nobs_bot_add, ngeo_bot_add

	type(bp), allocatable :: bps(:)

	isum = 0

	bps = readinput()

	! Blueprint ID
	i = 1

	print *, 'Blueprint = '
	print '(6i4)',           &
			bps(i)%ore_cost_ore, &
			bps(i)%cla_cost_ore, &
			bps(i)%obs_cost_ore, &
			bps(i)%obs_cost_cla, &
			bps(i)%geo_cost_ore, &
			bps(i)%geo_cost_obs

	nore = 0
	ncla = 0
	nobs = 0
	ngeo = 0

	nore_bot = 1  ! kickstart
	ncla_bot = 0
	nobs_bot = 0
	ngeo_bot = 0

	do it = 1, 24

		print '(a,i0,a)', '== Minute ', it, ' =='

		!nore0 = nore
		!ncla0 = ncla
		!nobs0 = nobs
		!ngeo0 = ngeo

		ngeo_bot_add = min((nore / bps(i)%geo_cost_ore), (nobs / bps(i)%geo_cost_obs))
		print *, 'a, b = ', nore / bps(i)%geo_cost_ore, nobs / bps(i)%geo_cost_obs
		print *, 'min(a,b) = ', ngeo_bot_add
		nore = nore - ngeo_bot_add * bps(i)%geo_cost_ore
		nobs = nobs - ngeo_bot_add * bps(i)%geo_cost_obs

		nobs_bot_add = min(nore / bps(i)%obs_cost_ore, ncla / bps(i)%obs_cost_cla)
		nore = nore - nobs_bot_add * bps(i)%obs_cost_ore
		ncla = ncla - nobs_bot_add * bps(i)%obs_cost_cla

		ncla_bot_add = nore / bps(i)%cla_cost_ore
		nore = nore - ncla_bot_add * bps(i)%cla_cost_ore

		nore_bot_add = nore / bps(i)%ore_cost_ore
		nore = nore - nore_bot_add * bps(i)%ore_cost_ore

		nore = nore + nore_bot
		ncla = ncla + ncla_bot
		nobs = nobs + nobs_bot
		ngeo = ngeo + ngeo_bot

		nore_bot = nore_bot + nore_bot_add
		ncla_bot = ncla_bot + ncla_bot_add
		nobs_bot = nobs_bot + nobs_bot_add
		ngeo_bot = ngeo_bot + ngeo_bot_add

		print *, 'nore_bot = ', nore_bot
		print *, 'ncla_bot = ', ncla_bot
		print *, 'nobs_bot = ', nobs_bot
		print *, 'ngeo_bot = ', ngeo_bot
		print *, ''

		print *, 'nore = ', nore
		print *, 'ncla = ', ncla
		print *, 'nobs = ', nobs
		print *, 'ngeo = ', ngeo
		print *, ''

	end do

	write(*,*) 'part1 = ', isum
	write(*,*) ''

end subroutine part1

!===============================================================================

end module m

!===============================================================================

program main

	use m

	write(*,*) 'Starting AOC main'
	write(*,*) 'Input file = ', finput
	write(*,*) ''

	call part1()
	!call part2()

	write(*,*) 'Ending AOC main'
	write(*,*) ''

end program main

!===============================================================================

