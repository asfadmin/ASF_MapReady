
/* This sql script requires that you use sqsh version 1.7 */

/* Short script to list the valid sites from the PVS database */

 /* Use the PVS database*/
use pvsdb
go

 /* Grab the site regions from the database */
select 
	"'"+rtrim(cs_name)+"'", 
	"'"+rtrim(cs_id)+"'", 
	"'"+rtrim(cs_desc)+"'"
     from 
	site_obj 
go
