c SccsId = @(#)pointing.f	2.41 3/24/98


        subroutine pointing(fxs,fys,fzs,fvxs,fvys,fvzs,alk,yaw,px,py,pz,
     +                      frame_mode)

        implicit none

        character*128 SccsId_pointing
        data SccsId_pointing
     +  /'@(#)PPpointing.f:2.41'/


        real*8 a3,r,v1,c3,a1,a2,pgy,pgz,pp,v2,v3,pgx,c2,fvxs,fvys,fvzs
        real*8 fxs,fys,fzs,py,pz,c1,alk,yaw,px
        integer frame_mode

        call cross(fvxs,fvys,fvzs,fxs,fys,fzs,c1,c2,c3)   !right looking
        call cross(fxs,fys,fzs,c1,c2,c3,a1,a2,a3)

        if(frame_mode .eq. 1) then      !left looking
         c1 = -c1
         c2 = -c2
         c3 = -c3
        end if

        r=(fxs**2+fys**2+fzs**2)**.5
        v1=-1*fxs/r
        v2=-1*fys/r
        v3=-1*fzs/r
        pgx=c1*sind(alk)
        pgy=c2*sind(alk)
        pgz=c3*sind(alk)
        px=v1*cosd(alk)+pgx*cosd(yaw)-a1*sind(yaw)*sind(alk)
        py=v2*cosd(alk)+pgy*cosd(yaw)-a2*sind(yaw)*sind(alk)
        pz=v3*cosd(alk)+pgz*cosd(yaw)-a3*sind(yaw)*sind(alk)
        pp = sqrt(px**2+py**2+pz**2)
        px = px/pp
        py = py/pp
        pz = pz/pp

        return
        end



