c SccsId = @(#)propa_simp.f	2.41 3/24/98



	subroutine propa_simp(r_x1,r_y1,r_z1,v_x1,v_y1,v_z1,
     *                        delta_t,t_spacing,
     *                         r_x2,r_y2,r_z2,v_x2,v_y2,v_z2)

        implicit none

        character*128 SccsId_propa_simp
        data SccsId_propa_simp
     +  /'@(#)PPpropa_simp.f:2.41'/


	real*8 gmass,dt,v_z2,v_x2,v_y2,ay,az,ax,rcub
	real*8 v_x1,v_y1,r_z1,r_x1,r_y1,v_z1,r_y2,r_z2,r_x2
	real*8 delta_t,t_spacing

	integer i,loop

	gmass = 3.9860045e14
	dt = t_spacing

c added this line to handle negative delta_t
	if(delta_t.lt.0.0) dt = -t_spacing

	loop = int(delta_t/dt)+1

	r_x2 = r_x1
	r_y2 = r_y1
	r_z2 = r_z1
	v_x2 = v_x1
	v_y2 = v_y1
	v_z2 = v_z1

	do i = 1, loop
	if(i.eq.loop) dt = delta_t - (loop-1)*dt
	rcub = (r_x2**2+r_y2**2+r_z2**2)**1.5
	ax = - gmass/rcub * r_x2
	ay = - gmass/rcub * r_y2
	az = - gmass/rcub * r_z2
	v_x2 = v_x2 + ax * dt
	v_y2 = v_y2 + ay * dt
	v_z2 = v_z2 + az * dt
	r_x2 = r_x2 + v_x2 * dt
	r_y2 = r_y2 + v_y2 * dt
	r_z2 = r_z2 + v_z2 * dt
	end do

	return
	end


