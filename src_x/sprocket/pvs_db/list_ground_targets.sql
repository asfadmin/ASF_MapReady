#!/ASF/current/bin/Linux/sqsh -i
	/* Use the PVS database */
use pvsdb
go

	/* List all the relevant point targets */
select 
        "'"+rtrim(cs_id)+"'",
        "'"+rtrim(cs_dev_id)+"'",
        "'"+rtrim(cs_maint_date)+"'",
        "'"+rtrim(cs_entry_date)+"'",
        "'"+rtrim(convert(char, cs_dev_type))+"'",
        "'"+rtrim(convert(char, cs_cr_offset))+"'",
        "'"+rtrim(convert(char, cs_pre_fltdir))+"'",
        "'"+rtrim(convert(char, cs_pre_tilt))+"'",
        "'"+rtrim(convert(char, cs_pre_level))+"'",
        "'"+rtrim(convert(char, cs_pre_bsight))+"'",
        "'"+rtrim(convert(char, cs_pre_xrcs))+"'",
        "'"+rtrim(convert(char, cs_post_fltdir))+"'",
        "'"+rtrim(convert(char, cs_post_tilt))+"'",
        "'"+rtrim(convert(char, cs_post_level))+"'",
        "'"+rtrim(convert(char, cs_post_bsight))+"'",
        "'"+rtrim(convert(char, cs_post_xrcs))+"'",
        "'"+rtrim(convert(char, cs_lat))+"'",
        "'"+rtrim(convert(char, cs_long))+"'",
        "'"+rtrim(convert(char, cs_elev))+"'",
        "'"+rtrim(convert(char, cs_magdec))+"'",
        "'"+rtrim(cs_cmnts)+"'",
        "'"+rtrim(opr_name)+"'"
     from 
	gtrg_obj	
     where
	cs_id = ${1} and cs_maint_date > ${2}  
	and cs_maint_date < ${3}
     order by
	cs_dev_id, cs_maint_date desc
go
