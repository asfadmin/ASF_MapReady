c SccsId = @(#)load_aux_parameter.f	2.41 3/24/98
	subroutine load_aux_parameter(file_name,
     *             burst_start,burst_end,
     *             beam_mode,nbeams,burst_cnt,nr,burst_good,t_b,
     *             gain_rep,gain_rec,fs,prf,plen,yaw_o,yaw_o_rate,
     *             roll_o,roll_o_rate,pitch_o,pitch_o_rate,ns,
     *             np,rg_gate_delay,np_v,beam_no,beam_id,
     *             aux_window_start_time,
     *             aux_wdp_chg,aux_wdp_np,aux_agc_np,aux_wdp_cnt,aux_agc_chg,aux_agc_cnt,
     *             aux_replica_agc,aux_lna_temp,aux_subsystem_temp,aux_protector_temp,
     *             aux_rx_agc,istatus,
     *             aux_calibration_temp,aux_repl_pow,bit_error_rate,
     *             aux_yaw_valid,aux_roll_valid,aux_pitch_valid)


	implicit none

        character*128 SccsId_load_aux_parameter
        data SccsId_load_aux_parameter
     +  /'@(#)PPload_aux_parameter.f:2.41'/


c	include "auxiliary.inc"
c	include "../main/key_const.inc"
c	include "../main/key_pp.inc"
        include 'ssp2_const.inc'
        include 'ssp2_dim.inc'
        integer iprocessor,n_times_pre,n_times,btotal,kg
c**************************************************************
        integer*4       burst_start,burst_end
        integer*4       beam_mode,nbeams
        integer*4       burst_cnt(burst_max)
        integer*4       nr
        integer*4       burst_good(burst_max)
        real*8          t_b(burst_max)
        real*8          gain_rep(burst_max)
        real*8          gain_rec(burst_max)
        real*8          fs 
        real*8          prf(burst_max)
        real*8          plen
        real*8          yaw_o(burst_max)
        real*8          yaw_o_rate(burst_max)
        real*8          roll_o(burst_max)
        real*8          roll_o_rate(burst_max)
        real*8          pitch_o(burst_max)
        real*8          pitch_o_rate(burst_max)
        integer*4       ns(burst_max)
        integer*4       np(burst_max)
        real*8          rg_gate_delay(burst_max)
        integer*4       np_v(burst_max)
        integer*4       beam_no(burst_max)
        integer*4       beam_id(burst_max)
c**************************************************************
        integer*4        aux_hdr_start_burst,aux_hdr_end_burst
        character*60     aux_hdr_instr_mode
        integer*4        aux_beam_select(1600)
        integer*4        aux_burst(1600)
        character*10     aux_echo_valid(1600) 
        integer*4        aux_replica_samples(1600)
        character*40     aux_sc_time(1600)
        real*8           aux_window_start_time(1600)
        real*8           aux_window_duration(1600)
        real*8           aux_replica_agc(1600)
        real*8           aux_rx_agc(1600)
        real*8           aux_sampling_rate(1600)
        real*8           aux_pulse_period(1600)
        character*10     aux_yaw_valid(1600)
        real*8           aux_yaw_error(1600)
        real*8           aux_yaw_rate(1600)
        character*10     aux_roll_valid(1600)
        real*8           aux_roll_error(1600)
        real*8           aux_roll_rate(1600)
        character*10     aux_pitch_valid(1600)
        real*8           aux_pitch_error(1600)
        real*8           aux_pitch_rate(1600)
        integer*4           aux_echo_samples(1600)
        integer*4           aux_chirp_type(1600)
        integer*4           aux_echo_pulses(1600)
        integer*4           aux_pulse_count_1(1600)
        integer*4           aux_pulse_count_2(1600)

        real*8        aux_wdp_chg(max_agc,1600)
        integer*4     aux_wdp_np(max_agc,1600)
        integer*4     aux_agc_np(max_agc,1600)
        integer*4     aux_wdp_cnt(1600)
        real*8        aux_agc_chg(max_agc,1600)
        integer*4     aux_agc_cnt(1600)
        real*8  aux_lna_temp(1600)
        real*8  aux_subsystem_temp(1600)
        real*8  aux_protector_temp(1600)
        real*8  aux_calibration_temp(1600)
        real*8  aux_repl_pow
        real*8  bit_error_rate

C*************************************************************
        

	integer*4	burst_i
	integer*4	i,burst_off
	real*8		time_sec,r8,np_delay,rst
        integer*4       istatus
        character*60    file_name
        integer         ssp_get_aux

        istatus= ssp_get_aux(file_name,aux_hdr_start_burst,aux_hdr_end_burst,
     *  aux_hdr_instr_mode,aux_beam_select,aux_burst,
     *  aux_echo_valid,aux_replica_samples,aux_sc_time,
     *  aux_window_start_time,aux_window_duration,aux_replica_agc,
     *  aux_rx_agc,aux_sampling_rate,aux_pulse_period,
     *  aux_yaw_valid,aux_yaw_error,aux_yaw_rate,
     *  aux_roll_valid,aux_roll_error,aux_roll_rate,
     *  aux_pitch_valid,aux_pitch_error,aux_pitch_rate,
     *  aux_echo_samples,aux_chirp_type,aux_echo_pulses,
     *  aux_pulse_count_1,aux_pulse_count_2,
     *  aux_wdp_np,aux_wdp_chg,aux_wdp_cnt,aux_agc_np,aux_agc_chg,aux_agc_cnt,
     *  aux_lna_temp,aux_subsystem_temp,aux_protector_temp,
     *  aux_calibration_temp,aux_repl_pow,bit_error_rate)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return

        


	write(6,*)'pass_aux:',istatus
	write(6,*)'aux_hdr_instrument_mode:',aux_hdr_instr_mode(1:3)

	burst_start=aux_hdr_start_burst
	burst_end=aux_hdr_end_burst



	if (aux_hdr_instr_mode(1:3) .eq. 'SWA') then
	   beam_mode=1
	   nbeams=4
	   write(6,*)'AUX INSTR MODE SS1:',beam_mode
	else if (aux_hdr_instr_mode(1:3) .eq. 'SWB') then
	   beam_mode=2
	   nbeams=4
	   write(6,*)'AUX INSTR MODE SS2:',beam_mode
	else if (aux_hdr_instr_mode(1:3) .eq. 'SNA') then
	   beam_mode=3
	   nbeams=2
	   write(6,*)'AUX INSTR MODE SS3:',beam_mode
	else if (aux_hdr_instr_mode(1:3) .eq. 'SNB') then
	   beam_mode=4
	   nbeams=3
	   write(6,*)'AUX INSTR MODE SS4:',beam_mode
	endif


	burst_off=0
        call beam_seq_4(nbeams,burst_off,
     *   aux_hdr_instr_mode,aux_beam_select)

        fs = aux_sampling_rate(1)
c       plen =aux_pulse_period(1)
        nr =aux_replica_samples(1)



	i=0
	do burst_i = burst_start,  burst_end

	   i=i+1
	   if (aux_burst(burst_i) .ne. i .and. i .le. 4) then
	       write(6,*)' ERROR of burst sequence!',i,aux_burst(burst_i)
               istatus = ierr_19
               return
	    endif

	    burst_cnt(burst_i)=aux_burst(burst_i)

	   if ((aux_echo_valid(burst_i)(1:3) .eq. 'YES') .or. 
     *         (aux_echo_valid(burst_i)(1:3) .eq. 'yes'))   then
	       nr=aux_replica_samples(burst_i)
               burst_good(burst_i)= 1
	   else 
               burst_good(burst_i)= 0
           endif
	   
	   if (aux_echo_valid(burst_i)(1:3) .eq. 'bad'.or.
     *         aux_echo_valid(burst_i)(1:3) .eq. 'BAD' ) then
	       write(6,*)' BAD burst # (pass_aux):',burst_i
	   endif
           if (aux_echo_valid(burst_i)(1:2) .eq. 'NO'.or.
     *         aux_echo_valid(burst_i)(1:2) .eq. 'no' ) then
               write(6,*)' BAD burst # (pass_aux):',burst_i
           endif


	  call  gmt2sec(aux_sc_time(burst_i),t_b(burst_i))
c qdn 4/18/97 adjust along track -0.046 sec
c qdn 4/21/97 adjust along track -0.041 sec
	if (burst_i .eq. 1) then
	  write(6,*)'s/c gmt:',t_b(burst_i),aux_sc_time(burst_i)
          t_b(burst_i) = t_b(burst_i) - 0.041
          write(6,*)'adjust s/c gmt of 1st burst:',t_b(burst_i),
     *               aux_sc_time(burst_i)
	endif

c	   ***pulse start time ref. to the 1st burst***
c	   t_b(burst_i) = t_b(burst_i)  +
c    *                    aux_window_start_time(burst_i)
c	  write(6,*)'s/c time ref&window start t:',t_b(burst_i),
c     *            aux_window_start_time(burst_i)

c	   ***center time of burst ***
c	   t_b(burst_i)=t_b(burst_i)+aux_window_duration(burst_i)/2
d	  write(6,*)'s/c Center time ref 1st:',t_b(burst_i)

	   r8=10**(aux_replica_agc(burst_i)/10.)
	   gain_rep(burst_i)=sqrt(r8)
	   r8=10**(aux_rx_agc(burst_i)/10.)
	   gain_rec(burst_i)=sqrt(r8)

	   prf(burst_i) =1/aux_pulse_period(burst_i)

	   if (aux_yaw_valid(burst_i)(1:3) .eq. 'YES') then 
     	      yaw_o(burst_i)=aux_yaw_error(burst_i)
	      yaw_o_rate(burst_i)=aux_yaw_rate(burst_i)
	   else
     	      yaw_o(burst_i)=0.0
	      yaw_o_rate(burst_i)=0.0
	   endif
	   if (aux_roll_valid(burst_i)(1:3) .eq. 'YES') then
     	      roll_o(burst_i)=aux_roll_error(burst_i)
	      roll_o_rate(burst_i)=aux_roll_rate(burst_i)
	   else
     	      roll_o(burst_i)=0.0
	      roll_o_rate(burst_i)=0.0
	   endif
	   if (aux_pitch_valid(burst_i)(1:3) .eq. 'YES')  then
     	      pitch_o(burst_i)=aux_pitch_error(burst_i)
	      pitch_o_rate(burst_i)=aux_pitch_rate(burst_i)
	   else
     	      pitch_o(burst_i)=0.0
	      pitch_o_rate(burst_i)=0.0
	   endif


	    ns(burst_i)	=aux_echo_samples(burst_i)

d	   write(6,*)' chirp #:',nr,nint(plen*fs)

	   if (aux_chirp_type(burst_i) .ne. 2) then
           write(6,*)'chirp type(2) 11.58MHz:',aux_chirp_type(burst_i)
           endif

	   np(burst_i) = aux_echo_pulses(burst_i)
	   if (np(burst_i) .ne. aux_pulse_count_1(burst_i)) then
              write(6,*)'# of pulse not match !!!', burst_i,np(burst_i),
     *                 aux_pulse_count_1(burst_i)
	   endif

	   rg_gate_delay(burst_i)=aux_window_start_time(burst_i)

	   np_v(burst_i) = aux_pulse_count_2(burst_i)
c	   np_v(burst_i) = aux_pulse_count_1(burst_i)
c     *                     - aux_pulse_count_2(burst_i)
D	   write(6,*)'p1 p2 air:',aux_pulse_count_1(burst_i)
D    *     ,aux_pulse_count_2(burst_i),np(burst_i)

           Call beam_seq_id(aux_hdr_instr_mode,burst_i,nbeams,
     *          aux_beam_select(burst_i),beam_mode,beam_no(burst_i),
     *                      beam_id(burst_i),burst_off,istatus)
           if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
c	  
c	   Calibration Attenuator Setting in dB
c			=aux_cal1_attenuator(burst_i)
c	   LowPowerTransmitter Power setting in dBm
c	 		=aux_cal2_attenuator(burst_i)
c	   Receiver Low Noise Amplifier Temp in C degree
c	 		=aux_lna_temp(burst_i)
c	   Receiver Sub-system Temp in C degree
c	 		=aux_subsystem_temp(burst_i)
c	   Receiver Protector Unit Temp in C degree
c	 		=aux_protector_temp(burst_i)
c	   Calibration Subsystem Temp in C degree
c	 		=aux_calibration_temp(burst_i)
	enddo


	return
	end

c**********************************************************************
        subroutine beam_seq_id(instr_mode,burst_i,nbeams,
     *                      beam_select,beam_mode,beam_no,
     *                      beam_id,burst_off,istatus)
c**********************************************************************
	implicit none
        include 'error.inc'

        integer  istatus
c	----------------
c	INPUT PARAMETERS	
c	----------------
	character*3	instr_mode	!SS1,SS2,SS3 & SS4	
	integer*4	beam_select	!beam id for ss1:1,2,3,7
	integer*4	burst_i		!burst sequence #
	integer*4	nbeams		!# of beams
	integer*4	beam_mode	!ss1:1 ss2:2 ss3:3 ss4:4
	integer*4	burst_off	! burst error offset

c	----------------
c	OUTPUT PARAMETER	
c	----------------
	integer*4	beam_id		!burst seq # n(1,2,3,7)
	integer*4	beam_no		!burst seq # n(1,2,3,4)

c	----------------
c	LOCAL VARIABLES
c	----------------
	integer*4	beam_seq(4)
	integer*4	k,burst_up

c	---------------------
c	CHECK 1ST CYCLE BEAM
c	---------------------

c	--------------------
c	LOAD BEAM_ID & BEAM_NO 
c	--------------------

	burst_up = burst_i + burst_off
 	k = burst_up - nbeams * int((burst_up-1)/nbeams)	  

	if ((burst_up .gt. nbeams) .or.(burst_off .eq. 0))  then

	if(instr_mode .eq. 'SWA' ) then
	  if (beam_select .eq. 1 .and. k .eq. 1 ) then
             beam_id=1
	     beam_no=1
	  else if (beam_select .eq. 2 .and. k .eq. 2 ) then
             beam_id=2
	     beam_no=2
	  else if (beam_select .eq. 3 .and. k .eq. 3 ) then
             beam_id=3
	     beam_no=3
	  else if (beam_select .eq. 7 .and. k .eq. 4 ) then
             beam_id=7
	     beam_no=4
	  else 
             write(6,*)'ERROR Beam ID for SS1:',burst_i,k,beam_select
             istatus = ierr_19
          endif
	else if(instr_mode .eq. 'SWB' ) then
	  if (beam_select .eq. 1 .and. k .eq. 1 ) then
             beam_id=1
	     beam_no=1
	  else if (beam_select .eq. 2 .and. k .eq. 2 ) then
             beam_id=2
	     beam_no=2
	  else if (beam_select .eq. 5 .and. k .eq. 3 ) then
             beam_id=5
	     beam_no=3
	  else if (beam_select .eq. 6 .and. k .eq. 4 ) then
             beam_id=6
	     beam_no=4
	  else 
             write(6,*)'ERROR Beam ID for SS2:',burst_i,k,beam_select
             istatus = ierr_19
          endif
	else if(instr_mode .eq. 'SNA' ) then
	   beam_mode=3
	  if (beam_select .eq. 1 .and. k .eq. 1 ) then
             beam_id=1
	     beam_no=1
	  else if (beam_select .eq. 2 .and. k .eq. 2 ) then
             beam_id=2
	     beam_no=2
	  else 
             write(6,*)'ERROR Beam ID for SS3:',burst_i,k,beam_select
             istatus = ierr_19
          endif
	else if(instr_mode .eq. 'SNB' ) then
	  if (beam_select .eq. 2 .and. k .eq. 1 ) then
             beam_id=2
	     beam_no=1
	  else if (beam_select .eq. 5 .and. k .eq. 2 ) then
             beam_id=5
	     beam_no=2
	  else if (beam_select .eq. 6 .and. k .eq. 3 ) then
             beam_id=6
	     beam_no=3
	  else 
             write(6,*)'ERROR Beam ID for SS4:',burst_i,k,beam_select
             istatus = ierr_19
          endif

	endif

	endif

	return
	end

c**********************************************************************
        subroutine beam_seq_4(nbeams,burst_off,
     *   aux_hdr_instr_mode,aux_beam_select)
c**********************************************************************
	implicit none

        character*60    aux_hdr_instr_mode
        integer*4       aux_beam_select(1600)
c	include "auxiliary.inc"
c	----------------
c	INPUT PARAMETERS	
c	----------------
	integer*4	nbeams		!# of beams

c	----------------
c	OUTPUT PARAMETER	
c	----------------
	integer*4	burst_off	! burst error offset

c	----------------
c	LOCAL VARIABLES
c	----------------
	integer*4	beam_seq(4)
	integer*4	burst_i
	character*3	instr_mode	!SS1,SS2,SS3 & SS4	

c	---------------------
c	CHECK 1ST CYCLE BEAM
c	---------------------
        instr_mode=aux_hdr_instr_mode(1:3)

	do burst_i=1,4
	   if (burst_i .eq. 1 ) then
	      beam_seq(1)=aux_beam_select(burst_i)
	   else if (burst_i .eq. 2 ) then
	      beam_seq(2)=aux_beam_select(burst_i)
	   else if (burst_i .eq. 3 ) then
	      beam_seq(3)=aux_beam_select(burst_i)
	   else if (burst_i .eq. 4 ) then
	      beam_seq(4)=aux_beam_select(burst_i)
	   endif
	enddo
	
	if (nbeams .eq. 4)  then
	   if ((instr_mode.eq.'SWA').or.(instr_mode.eq.'SWB')) then

	      if (beam_seq(1) .eq. 2 .and.
     *            beam_seq(2) .eq. 3 .and.  
     *            beam_seq(3) .eq. 7 ) then
	       burst_off=1
	       write(6,*)'SS1 BEAM MIS-MATCH 1st:',beam_seq
	      else if (beam_seq(1) .eq. 2 .and.
     *                 beam_seq(2) .eq. 5 .and.  
     *                 beam_seq(3) .eq. 6 ) then
	       burst_off=1
	       write(6,*)'SS2 BEAM MIS-MATCH 1st:',beam_seq
  	      endif 

	      if (beam_seq(1) .eq. 3 .and.
     *            beam_seq(2) .eq. 7 .and.  
     *            beam_seq(3) .eq. 1 ) then
	       burst_off=2
	       write(6,*)'SS1 BEAM MIS-MATCH two:',beam_seq
	      else if (beam_seq(1) .eq. 5 .and.
     *                 beam_seq(2) .eq. 6 .and.  
     *                 beam_seq(3) .eq. 1 ) then
	       burst_off=2
	       write(6,*)' SS2 BEAM MIS-MATCH two:',beam_seq
  	      endif 

	      if (beam_seq(1) .eq. 7 .and.
     *            beam_seq(2) .eq. 1 .and.  
     *            beam_seq(3) .eq. 2 ) then
	       burst_off=3
	       write(6,*)'SS1 BEAM MIS-MATCH 3:',beam_seq
	      else if (beam_seq(1) .eq. 6 .and.
     *                 beam_seq(2) .eq. 1 .and.  
     *                 beam_seq(3) .eq. 2 ) then
	       burst_off=3
	       write(6,*)' SS2 BEAM MIS-MATCH 3:',beam_seq
  	      endif 
  	   endif 
	endif	

	if (nbeams .eq. 3)  then
	   if (instr_mode.eq.'SNB') then

	      if (beam_seq(1) .eq. 5 .and.
     *            beam_seq(2) .eq. 6 ) then
	       burst_off=1
	       write(6,*)'SS4 BEAM MIS-MATCH 1st:',beam_seq
  	      endif 

	      if (beam_seq(1) .eq. 6 .and.
     *            beam_seq(2) .eq. 2 ) then
	       burst_off=2
	       write(6,*)'SS4 BEAM MIS-MATCH two:',beam_seq
  	      endif 

	   endif
	endif

	if (nbeams .eq. 2) then
	   if ((instr_mode.eq.'SNA').and.(beam_seq(1).eq.2)) then
	       burst_off=1
	       write(6,*)'SS4 BEAM MIS-MATCH 1st:',beam_seq
	   endif
	endif

	return
	end
