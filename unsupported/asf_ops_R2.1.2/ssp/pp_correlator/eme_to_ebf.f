c SccsId = @(#)eme_to_ebf.f	2.41 3/24/98


        subroutine eme_to_ebf(gha,second0,second,x,y,z,x1,y1,z1)

        implicit none

        character*128 SccsId_eme_to_ebf
        data SccsId_eme_to_ebf
     +  /'@(#)PPeme_to_ebf.f:2.41'/


        real*8 y1,x1,z1,omega_e_day,pi,z,second0,gha,second,y,x
        real*8 omega_e

c        parameter (omega_e = 360.9856296d0 / (24.0d0 * 3600.0d0 ))
        parameter (omega_e=7.29211585d-5)

	pi = 4*atan(1.)	
        omega_e_day = gha+omega_e*(second-second0)*180.0/pi
        x1=cosd(omega_e_day)*x+sind(omega_e_day)*y
        y1=-1*sind(omega_e_day)*x+cosd(omega_e_day)*y
        z1 = z

        return
        end
