/*
    extern variables 
*/


static char sccsid_extern_h[] = "@(#)extern.h	1.4 96/10/01 13:54:45";

typedef struct ceos_struct {
	char infile[BUFSIZ];
        char outfile[BUFSIZ];
        int  inputType;
        int  out;
	char odl_w_file[BUFSIZ];
	char odl_r_file[BUFSIZ];
	int  odl_out;
        int  debug;
}Junk;

extern struct ceos_struct CEOS;


void init_ceos( void );

/* from io_utils.c */

int cvt2int(unsigned char *cint);
int read_record( FILE* fp, unsigned char* buf, size_t size, size_t nitems );
void get_file_name(unsigned char *cbuf,int clen,char *fname);
int get_I4(unsigned char *cbuf, int field);
float get_F4(unsigned char *cbuf, int field);
int Get_I4(unsigned char *cbuf, int field);
float Get_F4(unsigned char *cbuf, int field);
void strNcpy(char *dbuf, char *sbuf, int field);
int get_file_type(unsigned char *cbuf, int clen);
void printfd( char* buf, int value);
void printfs( char* buf, char* value);
void printff( char* buf, float value);
void printfe( char* buf, double value);
void printfdl( char* buf, int value1, long value2);
void printfdf( char* buf, int value1, float value2);
void ODL_WRITEd(char* format, int args);
void ODL_WRITEl(char* format, long args);
void ODL_WRITEs(char* format, char* args);
void ODL_WRITEf(char* format, float args);
void ODL_WRITEe(char* format, double args);

void cvt2char(int *val, unsigned char* buf);
int write_record( FILE* fp, unsigned char* buf, size_t size, size_t nitems );
void put_I4(int val, char* field, int ifield, unsigned char* buf);
void put_F4(float val, char* field, int ifield, unsigned char* buf);
void put_chars( char* s1, char* s2, int ifield);
void put_blanks( char* s1, int ifield);
void zero_set( unsigned char* buf, int nitems);
void put_byte(unsigned char* out, unsigned char in);
void recalc_length(void);
void reset_rec_length(void);
int get_rec_length(void);
int get_recalc_status(void);

/* from process_ldr.c */

FILE* Open_LDR(char* name, int mode);
SARL_ptr* Allocate_SARL( void );
int Allocate_SARL_ODR( SARL_ptr* tree);
Dataset_Sum* Allocate_Data_Sum( SARL_ptr* tree );
Map_Proj* Allocate_Map_Proj( SARL_ptr* tree );
Pos_Data* Allocate_Plat_Pos( SARL_ptr* tree );
Att_Data* Allocate_Att_Data( SARL_ptr* tree );
Radi_Data* Allocate_Radi_Data( SARL_ptr* tree );
Radi_Comp* Allocate_Radi_Comp( SARL_ptr* tree );
Qual_Sum* Allocate_Qual_Sum( SARL_ptr* tree );
Data_Hist* Allocate_Data_Hist( SARL_ptr* tree );
Rng_Spec* Allocate_Rng_Spec( SARL_ptr* tree );
Digital_Elev* Allocate_Digital_Elev( SARL_ptr* tree );
Proc_Parm* Allocate_Proc_Parm( SARL_ptr* tree );
Calib_Data* Allocate_Calib_Data( SARL_ptr* tree );
Fac_Related* Allocate_Fac_Related( SARL_ptr* tree );
Dem_Desc*  Allocate_DEM_sets( Digital_Elev* e );
Corner_Pts* Allocate_DEM_pts( Dem_Desc* set );
Hist_Data_Set* Allocate_Hist_Data_Set( Data_Hist* h );
long* Allocate_DH_table( Hist_Data_Set* ht);
Rad_Comp_Set* Allocate_Comp_Sets( Radi_Comp* r );
Radio_Comp_Tbl* Allocate_Comp_Tbl( Rad_Comp_Set* s );
Pos_Vect_Rec* Allocate_Position_Velocity_Sets( Pos_Data* p);
Att_Vect_Rec* Allocate_Attitude_Sets( Att_Data* a);
Beam_Info*  Allocate_Beam_Info( Proc_Parm* e );
Pix_Count*  Allocate_Pix_Count( Proc_Parm* e );
Temp_Rec*  Allocate_Temp_Rec( Proc_Parm* e );
Dopcen_Est*  Allocate_Dopcen_Est( Proc_Parm* e );
SRGR_Coefset*  Allocate_SRGR_Coefset( Proc_Parm* e );
int Check_Rec_Seq( int* val, unsigned char* tmp, SARL_ptr* tree, int mode);

void Free_SARL( SARL_ptr* tree);
int Read_FDR_SARL( FILE* fp, SARL_ptr* tree );
int Read_ALL_SARL( FILE* fp, SARL_ptr* tree );

void Decode_SARL_FDR( unsigned char* buf, Sarl_Desc_Rec* t );
int  Decode_SARL_DSR( unsigned char* buf, SARL_ptr* t );
int  Decode_SARL_MPR( unsigned char* buf, SARL_ptr* t );
int  Decode_SARL_PPR( unsigned char* buf, SARL_ptr* t );
int  Decode_SARL_ATR( unsigned char* buf, SARL_ptr* t );
int  Decode_SARL_RDR( unsigned char* buf, SARL_ptr* t );
int  Decode_SARL_RCR( unsigned char* buf, SARL_ptr* t );
int  Decode_SARL_DQSR( unsigned char* buf, SARL_ptr* t );
int  Decode_SARL_DHR( unsigned char* buf, SARL_ptr* t );
int  Decode_SARL_RSR( unsigned char* buf, SARL_ptr* t );
int  Decode_SARL_DEM( unsigned char* buf, SARL_ptr* t );
int  Decode_SARL_DPR( unsigned char* buf, SARL_ptr* t );
int  Decode_SARL_FRR( unsigned char* buf, SARL_ptr* t );

void CEOS_to_LDR(void);

int Write_CEOS_SARL( char name[], SARL_ptr* tree );

Sarl_Desc_Rec* Get_L_FDR( SARL_ptr* t );
int FDR_to_LDR( Sarl_Desc_Rec* f, FILE* out );

Dataset_Sum* Get_L_DSS( SARL_ptr* t );
Dataset_Sum* Get_DSS_Num( SARL_ptr* t, int num );
int DSS_to_LDR( Dataset_Sum* d, FILE* out, int mode );

Map_Proj* Get_L_MP( SARL_ptr* t );
Map_Proj* Get_MP_Num( SARL_ptr* t, int num );
int MP_to_LDR( Map_Proj* m, FILE* out, int mode );

Pos_Data* Get_L_PP( SARL_ptr* t );
Pos_Data* Get_PP_Num( SARL_ptr* t, int num );
int PP_to_LDR( Pos_Data* p, FILE* out, int mode );

Att_Data* Get_L_AT( SARL_ptr* t );
Att_Data* Get_AT_Num( SARL_ptr* t, int num );
int AT_to_LDR( Att_Data* a, FILE* out, int mode );

Radi_Data* Get_L_RD( SARL_ptr* t );
Radi_Data* Get_RD_Num( SARL_ptr* t, int num );
int RD_to_LDR( Radi_Data* r, FILE* out, int mode  );

Radi_Comp* Get_L_RC( SARL_ptr* t );
Radi_Comp* Get_RC_Num( SARL_ptr* t, int num );
int RC_to_LDR( Radi_Comp* r, FILE* out, int mode  );

Qual_Sum* Get_L_DQS( SARL_ptr* t );
Qual_Sum* Get_DQS_Num( SARL_ptr* t, int num );
int DQS_to_LDR( Qual_Sum* d, FILE* out, int mode  );

Data_Hist* Get_L_DH( SARL_ptr* t );
Data_Hist* Get_DH_Num( SARL_ptr* t, int num );
int DH_to_LDR( Data_Hist* d, FILE* out, int mode  );

Rng_Spec* Get_L_RS( SARL_ptr* t );
Rng_Spec* Get_RS_Num( SARL_ptr* t, int num );
int RS_to_LDR( Rng_Spec* r, FILE* out, int mode  );

Digital_Elev* Get_L_DE( SARL_ptr* t );
Digital_Elev* Get_DE_Num( SARL_ptr* t, int num );
int DE_to_LDR( Digital_Elev* r, FILE* out, int mode  );

Proc_Parm* Get_L_DP( SARL_ptr* t );
Proc_Parm* Get_DP_Num( SARL_ptr* t, int num );
int DP_to_LDR( Proc_Parm* r, FILE* out, int mode  );

Calib_Data* Get_L_CD( SARL_ptr* t );
Calib_Data* Get_CD_Num( SARL_ptr* t, int num );
int CD_to_LDR( Calib_Data* r, FILE* out, int mode  );

Fac_Related* Get_L_FR( SARL_ptr* t );
Fac_Related* Get_FR_Num( SARL_ptr* t, int mode );
int FR_to_LDR( Fac_Related* r, FILE* out, int mode  );

void SetCurrentSARL( SARL_ptr* tree );
SARL_ptr* GetCurrentSARL(void);

/* from obl_lib.c */

int CEOS_to_ODL(void);
int FDR_to_ODL( Sarl_Desc_Rec* d);
int DSS_to_ODL( Dataset_Sum* d);
int MP_to_ODL( Map_Proj* d);
int PP_to_ODL( Pos_Data* d);
int ATT_to_ODL( Att_Data* a);
int RD_to_ODL( Radi_Data* r);
int RC_to_ODL( Radi_Comp* rc);
int DQS_to_ODL( Qual_Sum* q);
int DH_to_ODL( Data_Hist* h);
int RS_to_ODL( Rng_Spec* r);
int DE_to_ODL( Digital_Elev* e);
int DP_to_ODL( Proc_Parm* e);
int CD_to_ODL( Calib_Data* e);
int FR_to_ODL( Fac_Related* r);

