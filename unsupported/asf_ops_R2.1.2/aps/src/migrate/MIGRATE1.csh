#!/bin/csh -f
#
#   Data migration for operational database.  
#
#   [16 minutes on cajun]
#
#   SETUP
#
banner MIGRATE1
date
setenv APSDB larry
setenv Sybase_userid larry
setenv Sybase_password iceland

set SYB = " -U $Sybase_userid -P $Sybase_password "

echo " "
echo "    First, Rename dtk to dtk_old "
echo " "
date
isql -e $SYB << END_OF_HEREDOC
use $APSDB
go
print 'NOTE:  dropping the dtk_old table...'
print 'NOTE:  dropping the dtk_old table...'
print 'NOTE:  dropping the dtk_old table...'
drop table dtk_old
go
sp_rename dtk, dtk_old
go
END_OF_HEREDOC

echo " "
echo "    DONE renaming dtk to dtk_old "
echo " "

echo " "
echo "    NOW create new dtk and stats_calls relation structures "
echo " "
date
isql -e $SYB << END_OF_HEREDOC
use $APSDB
go
/*******************************************************************************
Name:       dtk.crt
 
Purpose:    Destroy old database dtk relation dtk and create a new one.
 
Detailed description:

datatake relation.  this relation holds the current planning activities, 
with the dtkstat field identifying the planning stage for each data-take.  

*******************************************************************************/
if exists (select * from sysobjects where name = "dtk" )
    drop table dtk
go
create table dtk         (
    sat         SatType not null,
    sensor      SensorType not null,
    rev         RevType not null,
    dtkid       DtkidType not null,
    fadtkid     char(20),
    darid       DaridType null,
    actid       ActidType not null,
    ascdsc      AscdscType not null check ( ascdsc in ('A', 'D', '-') ),
    strttime    AsftimeType not null,
    stoptime    AsftimeType not null,
    strtlat     Latitude4Type not null,
    stoplat     Latitude4Type not null,
    nrlat1      Latitude4Type not null,
    nrlon1      Longitude4Type not null,
    farlat1     Latitude4Type not null,
    farlon1     Longitude4Type not null,
    nrlat2      Latitude4Type not null,
    nrlon2      Longitude4Type not null,
    farlat2     Latitude4Type not null,
    farlon2     Longitude4Type not null,
    lookangl    real not null,
    dtkstat     DtkstatType not null,
    proposed_dtkstat    DtkstatType not null check ( proposed_dtkstat in 
                ('   ', '', 'QUE', 'SUB', 'PLN', 'SCH' ) ),
    transid     TransidType not null,
    sitename    SitenameType not null,
    notes       varchar(40) not null,
    dtkdate     AsftimeType not null,
    station_id  StationidType default 'ASF' not null,
    fa_schedule_link    char(20) default '',
    planner_quicklook   YesNoType not null,
    science_quicklook   YesNoType not null,
    submit_time         AsftimeType null, 
    antenna_id          Antenna_idType not null,  

    /* in addition to the usual ASF time, allow a "" value.  */
    fa_strttime         AsftimeType default "" null check ( 
           fa_strttime like 
"199[0-9]:[0-3][0-9][0-9]:[0-2][0-9]:[0-5][0-9]:[0-5][0-9].[0-9 ][0-9][0-9]" 
        or fa_strttime like 
"20[0-5][0-9]:[0-3][0-9][0-9]:[0-2][0-9]:[0-5][0-9]:[0-5][0-9].[0-9][0-9][0-9]"
        or fa_strttime = "" ), 

    /* in addition to the usual ASF time, allow a "" value.  */
    fa_stoptime         AsftimeType default "" null check ( 
           fa_stoptime like 
"199[0-9]:[0-3][0-9][0-9]:[0-2][0-9]:[0-5][0-9]:[0-5][0-9].[0-9 ][0-9][0-9]" 
        or fa_stoptime like 
"20[0-5][0-9]:[0-3][0-9][0-9]:[0-2][0-9]:[0-5][0-9]:[0-5][0-9].[0-9][0-9][0-9]"
        or fa_stoptime = "" ), 

    fa_duration_min     real default 0.0 null check ( fa_duration_min   >= 0.0 ), 
    asf_reduction_min   real default 0.0 not null,

primary key clustered (sat, rev, dtkid) ) 
go

-- now set up an index for quicker retrieves 
-- based on sat, fa_schedule_link - used in dtkm_link_obs_dtks()
create index dtk_sat_fa_schedule_link_index on dtk
    (sat, fa_schedule_link ) 
go

grant select on dtk to aps_reader
go

/*******************************************************************************
Name:       stats_calls.crt
 
Purpose:    Destroy old database stats_calls relation and create a new one.

The stats_calls relation enables the APS Statistics feature to 
avoid reporting the same activity twice.  To do this, it records 
successful statistics reporting to the IMS.  

There are a variety of information calls recorded here.  Some of the 
field values are optional.  See the comments for the type and condition 
fields.  These comments relate to the requirements.  

For some activities, a field might be meaningless, so the value is 
set to zero.  For example, if mask in and out times are being 
reported, dtkid=0.  Or if a SAR sensor activity is being reported, 
then antenna_id=0.  Or if AOS/LOS times are being reported, dtkstat 
is a '' string.  

*******************************************************************************/
if exists (select * from sysobjects where name = "stats_calls" )
    drop table stats_calls
go
create table stats_calls         (
    type        char(3) not null check (type in ('AOS', 'MSK', 'SAR', 'DLK')),
--  type indicates the type of call:  
--      AOS:  AOS/los times                 (dtkid=0, no dtkstat)
--      MSK:  MaSK in and out times         (dtkid=0, no dtkstat)
--      SAR:  SAR sensor data-take times    (antenna_id=0)
--      DLK:  DownLinK times

    condition   char(3) not null check (condition in 
                                               ('SCH', 'RED', 'CAN', 'PLN')),
--  condition indicates the circumstance of the reporting:
--      SCH:  SCHeduled activity
--      RED:  REDuced activity
--      CAN:  CANcelled activity
--      PLN:  PLaNned SAR sensor activity.  

    sat         SatType not null,
    rev         RevType not null,
    dtkid       DtkidType not null check ( dtkid between 0 and 99 ),
    strttime    AsftimeType not null,
    stoptime    AsftimeType not null,
    antenna_id  Antenna_idType not null,
    dtkstat     char(3) not null,       /* can be a '' string.  */
    call_time   AsftimeType not null,

primary key clustered (type, condition, sat, rev, dtkid) ) 
go

grant select on stats_calls to aps_reader
go
dump tran $APSDB with truncate_only
go
select count(*) from dtk_old
go
select count(*) from dtk
go
sp_help dtk
go
sp_help dtk_old
go
END_OF_HEREDOC

echo " "
echo "    DONE creating R2.1.2 stats_calls and dtk relations"
echo " "
echo " "
echo "    Next, insert recs from dtk_old to NEW dtk relation. "
echo "    (later, you drop dtk_old relation)
echo " "
date

isql -e $SYB << END_OF_HEREDOC
use $APSDB
go
print 'Inserting dtks into new dtk relation (takes up to 20 minutes): '
go
insert dtk (
    sat, sensor, rev, dtkid, fadtkid, darid, actid, ascdsc, 
    strttime, stoptime, 
    strtlat, stoplat, 
    nrlat1, nrlon1, farlat1, farlon1, 
    nrlat2, nrlon2, farlat2, farlon2, 
    lookangl, dtkstat, proposed_dtkstat, transid, sitename, notes, dtkdate, 
    station_id, fa_schedule_link, planner_quicklook, science_quicklook, 
    submit_time, antenna_id ) 
    select * from dtk_old
go
dump tran $APSDB with truncate_only
go
select count(*) from dtk
go
print 'NOTE:  Remember to drop the dtk_old table...'
go
END_OF_HEREDOC
date

banner DONE MIGRATE1
