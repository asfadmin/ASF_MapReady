typedef struct {

} packed_attitude_rec;
typedef struct {
	
} packed_time_rec;
typedef struct {
	u32 aux_sync;
	/* payload_status: */
		enum { off=0,idle,ready_cal,ready_image,image } payload_mode:3;
		enum { off=0,exec_image,scan_image,image_fail } image:2;
		u8 calibration:4;
		u8 fault:1;
		u8 warning:1;
		u8 image_type:2;
		u8 scan_strip:1;
		u8 hpmc_ready:1;
		u8 auto_manual:1;
	u8 replica_acg:6;
	u8 spare1:2;
	u8 caln_atten;
	u8 pulse_wave:4;
	u16 spare2:12;
	u32 temperature;
	u16 beam_sequence;
	u16 ephemeris;
	u8 number_of_beams:2;
	u8 adc_sampling_rate:2;
	u8 spare3:4;
	u8 pulse_count1;
	u8 pulse_count2;
	u16 prf_beam:13;
	u8 beam_select:2;
	u8 spare4:1;
	u16 rx_window_start:12;
	u8 spare5:4;
	u16 rx_window_duration:12;
	u8 spare6:4;
	packed_attitude_rec attitude;
	packed_time_rec time;
	u8 sct02_defaults:1;
	u8 replica_present:1;
	u8 rx_acg_setting:6;
} packed_auxData;


typedef enum {
	no_beam=0,
	standard1,
	standard2,
	standard3,
	standard4,
	standard5,
	standard6,
	standard7,
	wide1,
	wide2,
	wide3,
	wide2_recorded,
	experimental1,
	experimental2,
	experimental3,
	experimental4,
	fine1,
	fine2,
	fine3,
	fine4,
	fine5,
	too_big_beam
} beam_type;

typedef struct {
	enum {realTime=0,recorded} image_type;
	int num_beams;
	beam_type beam[4];
	int nom_attenuation; /*AGC attenuation in dB */
	real lpt_attenuation; /* LPT attenuation in dB */
	
} auxData;

int parse_auxData(packed_auxData *in, auxData *out); /*Returns 1 on sucess, 0 on failure.*/
