c SccsId = @(#)HEY.f	2.41 3/24/98


c       program prb_keypp

       subroutine prb_keypp(file_odl,
     *      lamb_lat_n,lamb_lat_s,
     *      echo_file,aux_file,rep_file,eph_file,cali_file,ceos_leader,
     *      image_file,scan_results_file,burst_loc_file,pmf_file,
     *      proc_id,product_id,
     *      c1_pxl,c2_pxl,p1_pxl,p2_pxl,
c    *      proj,radio_comp_mode,prod_type,res_prod,beam_mode,
     *      proj,radio_comp_mode,prod_type,res_prod,beam_mode,
     *      sv_type,sv_coord_sys,gha_time,gha,sv_ref_time,
     *      sv_x_pos,sv_y_pos,sv_z_pos,sv_x_vel,sv_y_vel,sv_z_vel,
     *      polar_flag,utm_sph,ceos_hdf,ps_lat,ps_lon,slat,along0,
     *      utm_zone,ter_correct,dem_ellip,
     *      echo_start_time,echo_end_time,proc_gain,avg_terrain,orbit_num,
     *      sitename,istatus,
     *      platform,sensor,rev,sequence,record_id,act_id,station_id,
     *      frame_mode,sv_prec,media_id,media_type,media_location,subframe_id)



        include 'ssp2_const.inc'
        character*128 SccsId
        data SccsId /'@(#)PPHEY.f:2.41'/
c       ***********************************************
        character*256    file_odl
        integer*4       subframe_id
        real*8          lamb_lat_n
        real*8          lamb_lat_s
        character*256    echo_file
        character*256    aux_file
        character*256    rep_file
        character*256    eph_file
        character*256    cali_file
        character*256    ceos_leader
        character*256    image_file
        character*256    scan_results_file
        character*256    burst_loc_file
        character*256    pmf_file
        integer*4      proc_id
        character*60      product_id
        real*8  	c1_pxl 
        real*8  	c2_pxl
        real*8  	p1_pxl
        real*8  	p2_pxl
        integer*4 	proj
        integer*4       prod_type
        integer*4       res_prod
        integer         radio_comp_mode
	integer*4       beam_mode
	integer*4       sv_type
	integer*4       sv_coord_sys
	real*8          gha_time
	real*8          gha
	real*8          sv_ref_time
	real*8          sv_x_pos
	real*8          sv_y_pos
	real*8          sv_z_pos
	real*8          sv_x_vel
	real*8          sv_y_vel
	real*8          sv_z_vel
	integer*4       polar_flag
        integer*4       utm_sph
	integer*4       ceos_hdf
	real*8          ps_lat
        real*8          ps_lon
        real*8          slat
        real*8          along0
	integer*4       utm_zone
	integer*4       ter_correct
        integer*4       dem_ellip
        character*60    echo_start_time
        character*60    echo_end_time
	real*8          proc_gain
 	real*8          avg_terrain
        integer*4       orbit_num
        character*60    sitename

        character*60    platform 
        character*60    sensor
        integer         rev
        integer         sequence
        character*60    record_id
        character*60    act_id
        character*60    station_id
        character*60    frame_mode
        character*60    sv_prec
        character*60    media_id
        character*60    media_type
        character*60    media_location



c	--------------	
c	LOCAL VARIABLES
c	--------------	
        integer         isuccess_load,load_rqst_odl
        integer      istatus
        real*8          r8
c       ***********************************************


C**************************
        integer*4       prb_proc_id
        character*60       prb_product_id
        character*60    prb_prod_type
	character*60    prb_comp_flag
        character*60    prb_gha_corr_time
        real*8  	prb_gha_corr_angle
        character*60    prb_instr_mode
        character*60    prb_sv_type
        character*60    prb_sv_coord_sys
        character*60    prb_sv_time
        real*8  prb_sv_x_pos
        real*8  prb_sv_y_pos
        real*8  prb_sv_z_pos
        real*8  prb_sv_x_vel
        real*8  prb_sv_y_vel
        real*8  prb_sv_z_vel
        character*60    prb_frame_mode
        character*60    prb_output_format
        character*60    prb_projection
        real*8  prb_pixel_spacing
        real*8  prb_ref_lat
        real*8  prb_ref_lon
        integer*4     prb_utm_zone
        character*60    prb_ter_correct
        integer*4     prb_proc_gain     !processing gain
        real*8  prb_avg_terrain !average terrain height
        character*256    prb_echo_file
        character*256    prb_aux_file
        character*256    prb_rep_file
        character*256    prb_eph_file
        character*256    prb_cali_file
        character*256    prb_ceos_leader
        character*256    prb_image_file
        character*256    prb_scan_results_file
        character*256    prb_burst_loc_file
        character*256    prb_pmf_file
        character*60    prb_msg_type
        character*60    prb_dest
        character*60    prb_source
        character*60    prb_time
        integer         prb_num_records
        character*60    prb_platform
        character*60    prb_sensor
        integer         prb_rev 
        integer         prb_sequence
        character*60    prb_activity_id 
        integer         prb_start_address
        integer         prb_end_address
        character*60    prb_start_time
        character*60    prb_end_time
        character*60    prb_record_id
        character*60    prb_station_id
        character*60    prb_sv_satellite
        integer     	prb_sv_rev
        integer     	prb_sv_number_vec
        integer     	prb_frame_id
        integer     	prb_subframe_id
        character*60    prb_sitename
        real*8          prb_lamb_lat_n
        real*8          prb_lamb_lat_s
        character*60    prb_media_id
        character*60    prb_media_type
        character*60    prb_media_location
C*************************************************


c       -------------
c       LOADING PROCESSING REQUEST from PROC_RQST.INC  into KEY_PP.INC
c       -------------

        istatus=   load_rqst_odl (file_odl,
     *            prb_proc_id,prb_product_id,prb_prod_type,prb_comp_flag,
     *            prb_gha_corr_time,prb_gha_corr_angle,prb_instr_mode,prb_sv_type,
     *            prb_sv_coord_sys,prb_sv_time,prb_sv_x_pos,prb_sv_y_pos,
     *            prb_sv_z_pos,prb_sv_x_vel,prb_sv_y_vel,prb_sv_z_vel,
     *            prb_frame_mode,prb_output_format,prb_projection,prb_pixel_spacing,
     *            prb_ref_lat,prb_ref_lon,prb_utm_zone,prb_ter_correct,
     *            prb_proc_gain,prb_avg_terrain,
C
     *            prb_echo_file,prb_aux_file,
     *            prb_rep_file,prb_eph_file,prb_cali_file,prb_ceos_leader,prb_image_file,
     *            prb_scan_results_file,prb_burst_loc_file,prb_pmf_file,
C
     *            prb_msg_type,prb_dest,prb_source,prb_time,prb_number_records,
     *            prb_platform,prb_sensor,prb_rev,prb_sequence,prb_activity_id,
     *            prb_start_address,prb_end_address,prb_start_time,prb_end_time,
     *            prb_record_id,prb_station_id,prb_sv_satellite,prb_sv_rev,
     *            prb_sv_number_vec,prb_frame_id,prb_subframe_id,prb_sitename,prb_lamb_lat_n,
     *            prb_lamb_lat_s,prb_media_id,prb_media_type,prb_media_location)

        write(6,*)'WAHT IS STATUS ',istatus
        

C*****************************************************************************************
        
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
        
C*****************************************************************************************
        lamb_lat_n      =prb_lamb_lat_n
        lamb_lat_s      =prb_lamb_lat_s
C*****************************************************************************************
        echo_file        =prb_echo_file
        aux_file         =prb_aux_file
        rep_file         =prb_rep_file
        eph_file         =prb_eph_file
        cali_file        =prb_cali_file
        ceos_leader      =prb_ceos_leader
        image_file       =prb_image_file
        scan_results_file=prb_scan_results_file
        burst_loc_file   =prb_burst_loc_file
        pmf_file         =prb_pmf_file
        sitename         =prb_sitename
C*****************************************************************************************
        platform         =prb_platform
        sensor           =prb_sensor
        rev              =prb_rev
        sequence         =prb_sequence
        record_id        =prb_record_id
        act_id           =prb_activity_id
        station_id       =prb_station_id
        frame_mode       =prb_frame_mode
        sv_prec          =prb_sv_type
        media_id         =prb_media_id
        media_type       =prb_media_type
        media_location   =prb_media_location
        subframe_id      =prb_subframe_id
C*****************************************************************************************
        proc_id  = prb_proc_id
        product_id  = prb_product_id
C*****************************************************************************************
c       rgc_est_mode=1
c       fdc_est_mode=1
c       prf_est_mode=1
c	pta_refn_mode=1	!    RUN PTA WITH PARAMETERS
c	proc_mode = 1
C*****************************************************************************************
c	iquick = 0
c	dem_ellip=ellip
c	ire_sim=0		! simulation
c	data_cond=1
c	norm_look_mode=1
C*****************************************************************************************
	c1_pxl=int(prb_pixel_spacing)
	c2_pxl=int(prb_pixel_spacing)
	p1_pxl=int(prb_pixel_spacing)
	p2_pxl=int(prb_pixel_spacing)

	if (prb_projection(1:3) .eq. 'UTM') then
	    proj = utm
	else if (prb_projection(1:2) .eq. 'PS') then
	    proj = ps
	else if (prb_projection(1:4) .eq. 'ATCT') then
	    proj = atct
	else if (prb_projection(1:12) .eq. 'GROUND_RANGE') then
	    proj = atct
	else 
	    proj = lambert
	endif 

	if (prb_prod_type(1:8) .eq. 'STANDARD') then
	    prod_type = prod_1	
	else if ((prb_prod_type(1:7) .eq. 'CAL_SET') .and. (prb_comp_flag(1:3) .eq. 'YES')) then
	    prod_type = prod_pvs_2	
	else if ((prb_prod_type(1:7) .eq. 'CAL_SET') .and. (prb_comp_flag(1:2) .eq. 'NO')) then
	    prod_type = prod_pvs_3	
	endif

	if (prb_comp_flag(1:3) .eq. 'YES') then
	     radio_comp_mode=1 
	     write(6,*)'***No Ant comp for PVS ***',radio_comp_mode
c            proj=atct
	     res_prod=150
	     c1_pxl=100
	     c2_pxl=100
	     p1_pxl=100
	     p2_pxl=100
	else if (prb_comp_flag(1:2) .eq. 'NO') then
	     radio_comp_mode=4 ! no antenna comp for pvs
c            proj=atct
	     res_prod=150
	     c1_pxl=100
	     c2_pxl=100
	     p1_pxl=100
	     p2_pxl=100
	endif 

	if (prb_instr_mode(1:3) .eq. 'SWA') then
	    beam_mode=1
	else if (prb_instr_mode(1:3) .eq. 'SWB' ) then
	        beam_mode=2
	else if (prb_instr_mode(1:3) .eq. 'SNA' ) then
	        beam_mode=3
	else
	        beam_mode=4
	endif 

	if (prb_sv_type(1:9) .eq. 'PREDICTED') then
	   sv_type=predicted
	else
	   sv_type=restituted
	endif

	if (prb_sv_coord_sys(1:15) .eq. 'TRUE_EQUATORIAL') then
	    sv_coord_sys=true_equatorial
	else
	    sv_coord_sys=mean_date
	endif

	call gmt2sec(prb_gha_corr_time,gha_time)
c	gha=prb_gha_corr_angle*180.d0/pi
        gha=prb_gha_corr_angle
        write(6,*)'gha ',gha
        write(6,*)'gha_time ',gha_time
        write(6,*)'prb_gha_corr_gha_time ',prb_gha_corr_time

	call gmt2sec(prb_sv_time,sv_ref_time)
        write(6,*)'prb_sv_time ',prb_sv_time
        write(6,*)'sv_ref_time ',sv_ref_time
     
         

	sv_x_pos=prb_sv_x_pos*1000.0d0
	sv_y_pos=prb_sv_y_pos*1000.0d0
	sv_z_pos=prb_sv_z_pos*1000.0d0
	sv_x_vel=prb_sv_x_vel
	sv_y_vel=prb_sv_y_vel
	sv_z_vel=prb_sv_z_vel


c       sv_x_pos=2980519.72d0
c       sv_y_pos=4642416.47d0
c       sv_z_pos=4573743.91d0
c       sv_x_vel=-1310.5334d0
c       sv_y_vel=-4717.50401d0
c       sv_z_vel=5627.44766d0
c       write(6,*) prb_sv_y_pos,prb_sv_y_pos*1000.0
c       stop

	if (prb_frame_mode(1:6) .eq. 'ARCTIC') then
	   polar_flag=arctic
           utm_sph   =north_sph
	else
	   polar_flag=antarctic
           utm_sph   =south_sph
	endif

	if (prb_output_format(1:4) .eq. 'CEOS') then
	   ceos_hdf=ceos
	else if (prb_output_format(1:3) .eq. 'HDF') then
c	   ceos_hdf=hdf
 	   ceos_hdf=ceos
	endif

 	ps_lat=prb_ref_lat		!ps ref. lat.	
 	ps_lon=prb_ref_lon		!ps ref. lon.	
        slat  =ps_lat
        along0=ps_lon
	
	utm_zone=prb_utm_zone

	if (prb_pixel_spacing .eq. space_50) then	!spacing 50 meter
	   res_prod=75					!resolution 75 meter
	else if (prb_pixel_spacing .eq. space_100) then
	   res_prod=150
	else if (prb_pixel_spacing .eq. space_400) then
	   res_prod=600
	   c1_pxl=100
	   c2_pxl=100
	   p1_pxl=100
	   p2_pxl=100
	endif

	if (prb_ter_correct .eq. 'YES') then
	   ter_correct=yes
           dem_ellip  =dem
	else
	   ter_correct=none
           dem_ellip  =ellip
	endif

        call gmt2sec(prb_start_time,echo_start_time)
        call gmt2sec(prb_end_time,echo_end_time)


 	r8=dfloat(prb_proc_gain) !dB in integer
 	proc_gain=10**(r8/10.)   !amplitude
c Nov 27/96: correct the computation of the gain for amplitude
        proc_gain=10**(r8/20.)   !amplitude

 	avg_terrain=prb_avg_terrain

        orbit_num=prb_rev

	
        return
        end
