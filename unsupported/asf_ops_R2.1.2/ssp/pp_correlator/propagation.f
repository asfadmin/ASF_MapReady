c SccsId = @(#)propagation.f	2.41 3/24/98
c****************************************************************
c       Jet Propulsion Laboratory
c       Project: ASF RadarSAT
c       Task:    ScanSAR Processor
c       Code:    Fortran 77
c------------------------------------------------------------------
c
c       8/31/94 A.Chu Initial code & utilize the modified Johnny Kwok's 
c	              ASAP code.
c------------------------------------------------------------------
	subroutine propagation(r_x1,r_y1,r_z1,v_x1,v_y1,v_z1,
     *                         start_time,delta_t,
     *                         r_x2,r_y2,r_z2,v_x2,v_y2,v_z2)
	implicit none

        character*128 SccsId_propagation
        data SccsId_propagation
     +  /'@(#)PPpropagation.f:2.41'/


c       ----------------
c       Abstract:
c               Propagate one state vector based on starting state vector,
c		start time in second(convert from GMT) and time interval.
c
c               Input:
c		     1).State Vector:
c		        S/C Position & velocity.
c                    2).GMT Time in second:
c	                Start time & time interval
c               Output:
c	             State Vector of the start time + time interval.
c
c               Subroutines included:
c                    propag_init.f
c                    eme_kepler.f
c                    asap.f (modified version by A.Chu)
c	-----------------
c	INPUT PARAMETERS PASSING
c	-----------------
	real*8	r_x1,r_y1,r_z1	!Position of S/C
	real*8	v_x1,v_y1,v_z1	!Velocity of S/C
	real*8	start_time	!Start_time in second
	real*8	delta_t		!Time interval in sec.

c	-----------------
c	OUTPUT PARAMETERS PASSING
c	-----------------
	real*8	r_x2,r_y2,r_z2	!New Position of S/C of delta_t
	real*8	v_x2,v_y2,v_z2	!New velocity of S/C of delta_t

c	-----------------
c	OUTPUT PARAMETERS PASSING
c	-----------------
	real*8	kepler(6)	!Kepler 6 elements
	real*8	state_vec(6)	!New state vectors

D       write(6,*) 'delta_t (seconds):', delta_t
D	write(6,*) 'old x,y,z:',r_x1,r_y1,r_z1
D	write(6,*) 'old vx,vy,vz:',v_x1,v_y1,v_z1

c	------------	
c	1. Setup flags & coefficients
c	2. Convert State Vector to Kepler Elements
c	------------	
	call propag_init(r_x1,r_y1,r_z1,v_x1,v_y1,v_z1,kepler)

c	   write(6,*)'kepler r',kepler(1),kepler(2),kepler(3)
c	   write(6,*)'kepler v',kepler(4),kepler(5),kepler(6)

c	------------	
c	State Vector propagation
c	------------	
	call asap(kepler,start_time,delta_t,state_vec)

	r_x2=state_vec(1)
	r_y2=state_vec(2)
	r_z2=state_vec(3)
	v_x2=state_vec(4)
	v_y2=state_vec(5)
	v_z2=state_vec(6)

D	write(6,*) 'new x,y,z:',state_vec(1),state_vec(2),state_vec(3)
D	write(6,*) 'new vx,vy,vz:',state_vec(4),state_vec(5),state_vec(6)
	end

