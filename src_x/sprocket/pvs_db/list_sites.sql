#!/ASF/current/bin/Linux/sqsh -i

/* Short script to list the valid sites from the pvs db */

        /* Use the PVS database */
use pvsdb
go

select 
	"'"+rtrim(cs_name)+"'", 
	"'"+rtrim(cs_id)+"'", 
	"'"+rtrim(cs_desc)+"'"
     from 
	site_obj 
go
