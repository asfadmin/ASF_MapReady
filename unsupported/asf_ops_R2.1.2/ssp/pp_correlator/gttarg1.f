c SccsId = @(#)gttarg1.f	2.41 3/24/98



        subroutine gttarg1(rst0,fd0,lambda,xs,ys,zs,vxs,vys,vzs,h,
     1             px,py,pz,xt,yt,zt,vxt,vyt,vzt,alkang,yaw,frame_mode)

        implicit none

        character*128 SccsId_gttarg1
        data SccsId_gttarg1
     +  /'@(#)PPgttarg1.f:2.41'/


	include '/usr/lpp/ppe.poe/include/mpif.h'

        integer ierror

        real*8 lat,lat_d,lon,lambda

        real*8 vzt
        real*8 alkang
        real*8 yaw
        real*8 zt
        real*8 vxt
        real*8 vyt
        integer n_iteration
        real*8 fd
        real*8 thi_inc
        real*8 vst
        integer n_iter
        real*8 r_local
        real*8 rst
        real*8 yt
        real*8 ys
        real*8 zs
        real*8 vxs
        real*8 rst0
        real*8 fd0
        real*8 xs
        real*8 vys
        real*8 py
        real*8 pz
        real*8 xt
        real*8 vzs
        real*8 h
        real*8 px
        integer frame_mode

c        print *, "gttarg1: initial quess for look angle: ", alkang

	n_iteration = 50
c        alkang = 48.
        yaw = 0.
	n_iter = 0
1111    continue

        call pointing(xs,ys,zs,vxs,vys,vzs,alkang,yaw,px,py,pz,
     +                frame_mode)

        call seetarg(h,xs,ys,zs,px,py,pz,xt,yt,zt,r_local)

        call ebf_to_ll(xt,yt,zt,lat,lat_d,lon)

        call get_tv(lat,lon,lat_d,h,xt,yt,zt,vxt,vyt,vzt)

        n_iter = n_iter + 1
        if(n_iter.gt.n_iteration) then
           print *, 'gttarg1: cannot see target after ',
     +     n_iter,' iterations'
	   call mpi_abort(mpi_comm_world,22,ierror)
           call exit(22)
           return
        else
           call rngdop(xs,ys,zs,vxs,vys,vzs,xt,yt,zt,rst,fd,lambda)

           call inc_ang(alkang,xs,ys,zs,xt,yt,zt,thi_inc)
           vst = sqrt((vxs-vxt)**2+(vys-vyt)**2+(vzs-vzt)**2)
           alkang = alkang + atand((rst0-rst)/(tand(thi_inc)*rst))
           yaw = yaw - asind(lambda*(fd0-fd)/(2*vst))
           if(abs(fd-fd0).gt.0.1.or.abs(rst-rst0).gt.0.1) then
           go to 1111
           end if

        end if

        return
        end
