/*
    extern variables
*/

typedef struct ceos_struct {
        char infile[BUFSIZ];
        char outfile[BUFSIZ];
        int  inputType;
        int  out;
        int  debug;
}Junk;

extern struct ceos_struct CEOS;


int init_ceos( void );

/* from io_utils.c */

int cvt2int(unsigned char *cint);
int read_record( FILE* fp, unsigned char* buf, size_t size, size_t nitems );
int get_file_name(unsigned char *cbuf,int clen,char *fname);
int get_I4(unsigned char *cbuf, int field);
float get_F4(unsigned char *cbuf, int field);
int get_file_type(unsigned char *cbuf, int clen);

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
Fac_Related* Allocate_Fac_Related( SARL_ptr* tree );
Dem_Desc*  Allocate_DEM_sets( Digital_Elev* e );
Corner_Pts* Allocate_DEM_pts( Dem_Desc* set );
Hist_Data_Set* Allocate_Hist_Data_Set( Data_Hist* h );
long* Allocate_DH_table( Hist_Data_Set* ht);
Rad_Comp_Set* Allocate_Comp_Sets( Radi_Comp* r );
Radio_Comp_Tbl* Allocate_Comp_Tbl( Rad_Comp_Set* s );
Att_Vect_Rec* Allocate_Attitude_Sets( Att_Data* a);
int Check_Rec_Seq( int* val, unsigned char* tmp, SARL_ptr* tree, int mode);

void Free_SARL( SARL_ptr* tree);
int Read_FDR_SARL( FILE* fp, SARL_ptr* tree, int );
int Read_ALL_SARL( FILE* fp, SARL_ptr* tree, int );

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
int  Decode_SARL_FRR( unsigned char* buf, SARL_ptr* t );

int Write_ODL_SARL( SARL_ptr* tree );

Sarl_Desc_Rec* Get_L_FDR( SARL_ptr* t );
int Write_L_FDR_ODL( Sarl_Desc_Rec* f, int out );

Dataset_Sum* Get_L_DSS( SARL_ptr* t );
Dataset_Sum* Get_DSS_Num( SARL_ptr* t, int num );
int Write_L_DSS_ODL( Dataset_Sum* d, int out, int mode );

Map_Proj* Get_L_MP( SARL_ptr* t );
Map_Proj* Get_MP_Num( SARL_ptr* t, int num );
int Write_L_MP_ODL( Map_Proj* m, int out, int mode );

Pos_Data* Get_L_PP( SARL_ptr* t );
Pos_Data* Get_PP_Num( SARL_ptr* t, int num );
int Write_L_PP_ODL( Pos_Data* p, int out, int mode );

Att_Data* Get_L_AT( SARL_ptr* t );
Att_Data* Get_AT_Num( SARL_ptr* t, int num );
int Write_L_AT_ODL( Att_Data* a, int out, int mode );

Radi_Data* Get_L_RD( SARL_ptr* t );
Radi_Data* Get_RD_Num( SARL_ptr* t, int num );
int Write_L_RD_ODL( Radi_Data* r, int out, int mode  );

Radi_Comp* Get_L_RC( SARL_ptr* t );
Radi_Comp* Get_RC_Num( SARL_ptr* t, int num );
int Write_L_RC_ODL( Radi_Comp* r, int out, int mode  );

Qual_Sum* Get_L_DQS( SARL_ptr* t );
Qual_Sum* Get_DQS_Num( SARL_ptr* t, int num );
int Write_L_DQS_ODL( Qual_Sum* d, int out, int mode  );

Data_Hist* Get_L_DH( SARL_ptr* t );
Data_Hist* Get_DH_Num( SARL_ptr* t, int num );
int Write_L_DH_ODL( Data_Hist* d, int out, int mode  );

Rng_Spec* Get_L_RS( SARL_ptr* t );
Rng_Spec* Get_RS_Num( SARL_ptr* t, int num );
int Write_L_RS_ODL( Rng_Spec* r, int out, int mode  );

Digital_Elev* Get_L_DE( SARL_ptr* t );
Digital_Elev* Get_DE_Num( SARL_ptr* t, int num );
int Write_L_DE_ODL( Digital_Elev* r, int out, int mode  );

Fac_Related* Get_L_FR( SARL_ptr* t );
Fac_Related* Get_FR_Num( SARL_ptr* t, int mode );
int Write_L_FR_ODL( Fac_Related* r, int out, int mode  );



