#!/bin/csh -f
set echo
#
#  Initialize a dtk relation to the Operational (old) structure.  
#  The load data into it.  
#
#   [11 minutes on cajun]
#
set DATE_BCP = "3.4.98"

banner INIT.csh
date
isql -e -P iceland -U larry  << END_OF_HEREDOC
use $APSDB
go
dump tran $APSDB with truncate_only
go

/*******************************************************************************
Name:		dtk.crt

Purpose:	Destroy old MPS database relation dtk and create a new one.
MODIFICATION:  add station_id field indicating ground station; 
add csa_schedule_id to hold CSA schedule id.  03-95.

Input:		none.

Output:		APS database relation.


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

	/* CHANGED FIELD NAME FOR WOS requirement.  */
	fa_schedule_link	char(20) default '',
	planner_quicklook   YesNoType not null,
	science_quicklook   YesNoType not null,
	submit_time  		AsftimeType null, 
	antenna_id  		Antenna_idType not null,  

primary key clustered (sat, rev, dtkid)	) 
go

-- now set up an index for quicker retrieves 
-- based on sat, fa_schedule_link - used in dtkm_link_obs_dtks()
create index dtk_sat_fa_schedule_link_index on dtk
	(sat, fa_schedule_link ) 
go

grant select on dtk to aps_reader
go

/*******************************************************************************
Name:       dl2obs.crt

Purpose:    Destroy old MPS database relation dtk_obs_dmp and dl2obs and 
			create a new one.

Input:      none.

Output:     APS database relation dl2obs.


Detailed description:

dl2obs relation.  this relation ties downlinking data-takes in the dtk
relation to the observation (recording or real-time sensing) data-takes 
which are downlinked by it.  The means of tieing them together is 
via the primary key from the dtk relation:  
    sat/rev/dtkid.  

Each record in the dl2obs relation holds the primary key field 
values for a downlinking dtk and an observation data-take that is downlinked 
by it.  

To find a downlink dtk when you know an observation, or to find an observation 
when you know a downlink, you retrieve using the known values to get 
the primary key(s) to the record(s) you want to find.  

To find observations given a downlink, search the dl2obs relation on 
the downlink knowns:
    sat, rev_dl, dtkid_dl 

To find the tape dump that downlinks an observation, search the dl2obs 
relation on observation knowns:
    sat, rev_obs, dtkid_obs

For an observation, there will be only one record to find; thus sat, 
rev_obs, dtkid_obs constitutes a unique key.  The record will 
provide the downlink primary keys.  

For a downlink, there may be more than one record, one for each observation 
on the downlink.  

******************************************************************************/

/*  the dl2obs table replaces the dtk_obs_dmp relation.  */
if exists ( select * from sysobjects where name = "dtk_obs_dmp" )
    drop table dtk_obs_dmp
go
if exists ( select * from sysobjects where name = "dl2obs" )
    drop table dl2obs
go

create table dl2obs         (
    sat            SatType         not null,

--  The observation primary key fields.  The sat value is the same for 
--  both the downlink data-take and the observation data-take:

    rev_obs        RevType         not null,
    dtkid_obs      DtkidType       not null,

--  The downlink primary key fields:  
    rev_dl         RevType         not null,
    dtkid_dl       DtkidType       not null,

--  This primary key is to speed the retrievals to obtain the downlink 
--  for an observation.  it will enforce uniqueness in the combination 
--  of values:

primary key clustered (sat, rev_obs, dtkid_obs ))
go

--  This index is to speed the retrievals to obtain observation records; it 
--  will not enforce uniqueness:

create index dl2obs_index 
    on dl2obs ( sat, rev_dl, dtkid_dl )
go

grant select on dl2obs to aps_reader
go

/*******************************************************************************
Name:       sat_rev_dtkid.crt

Purpose:    Destroy old MPS database relation sat_rev_dtkid and create 
            a new one.  
            Same for dtkid_lock and stored procedure aps_sp_new_dtkid

    IMPORTANT NOTE:  the aps_seq_acct account
    IMPORTANT NOTE:  the aps_seq_acct account
    IMPORTANT NOTE:  the aps_seq_acct account
    
    The aps_seq_acct account needs to be created by the DBA, and the 
    default database for the aps_seq_acct account must be the master 
    database.  

    The aps_seq_acct account is granted execute permission on this 
    stored procedure, at the end of this script.  
    This account is used by other ASF systems to obtain a sequence number.  

Input:      none.

Output:     APS database relation.


Detailed description:

Dtkid values are now to be unique on sat, rev.  We want 
to enforce this rule on every dtk.  This means that if a 
dtk for E2/12345 is created, then deleted, 
and if a new dtk for E2/12345 created, it will not 
get the same dtkid as the one just deleted.  

The stored procedure to enforce this is to be an 
atomic transaction for which the sybase server will 
queue up requests on a first come first basis.  this 
is achieved using an update of the dtkid_lock table at 
the start of a transaction which encloses the steps of 
the stored procedure.  

*******************************************************************************/

reset

/* the table to implement exclusive lockability:  */ 
if exists (select * from sysobjects where name = "dtkid_lock" )
    drop table dtkid_lock
go
create table dtkid_lock ( semaphore   int     not null )
go
grant select on dtkid_lock to aps_reader
go

/* table to record the highest-used dtkid by sat/rev:  */
if exists (select * from sysobjects where name = "sat_rev_dtkid" )
    drop table sat_rev_dtkid
go
create table sat_rev_dtkid         (
    sat         SatType not null,
    rev         RevType not null,
    dtkid       DtkidType not null,
primary key clustered (sat, rev )   ) 
go
grant select on sat_rev_dtkid to aps_reader
go


/*==============================================================================
SQL Stored Procedure:       aps_sp_new_dtkid

Description:    Increment dtkid, starting with 1, for the input sat, rev.
                Returns the new value of dtkid.  

Parameters:     
                input:
                name type       meaning
                sat  char(2)    satellite or platform
                rev  int        revolution or orbit number ( > 0 )

Returns:        value > 0 : new dtkid to use

                type       meaning
                tinyint    new dtkid to use with sat/rev.  


                value < 0 : error code
                return -1001  bad input rev.  
                return -1002  multiple entries in sat_rev_dtkid  
                return -1003  Failed select from sat_rev_dtkid  
                return -1004  dtkid at maximum value  
                return -1005  Failed insert into sat_rev_dtkid  
                return -1006  Invalid dtkid in sat_rev_dtkid table  
                return -1007  Failed update on sat_rev_dtkid  
 
Creator:        Lawrence Stevens

Creation Date:  Mon May  5 12:35:19 PDT 1997
==============================================================================*/

if exists (select * from sysobjects where name = "aps_sp_new_dtkid" )
    drop procedure aps_sp_new_dtkid
go

create procedure aps_sp_new_dtkid
@sat  char(2), 
@rev  int
as

begin

    declare @dtkid tinyint

    /* initialize @dtkid */
    select @dtkid = null

    /* check input rev value:  */
    if @rev <= 0 
    begin
        print "aps_sp_new_dtkid:  Error:"
        print "    Input rev value must be > 0, but value was %1!", @rev
        return -1001   /* bad input rev.  */
    end

    /*
    -- Create an exclusive dtkid lock here, 
    -- using begin tran on an update of a table:
    */

    /*   BEGIN TRANSACTION    */
    begin tran  aps_sp_dtkid_transaction

    begin 
        /* 
        -- the Sybase server will queue up 
        -- requests right here, due to this 
        -- update within this transaction:  
        */
        update dtkid_lock set semaphore = 1
    end

    /* 
    -- TESTING-ONLY:
    -- this is a TESTING-ONLY wait to 
    -- allow multiple requests on this function 
    -- and to check up on the queuing function.  
    */
--  waitfor delay "0:00:10"

    /* select current dtkid using input sat/rev */
    select @dtkid = dtkid from sat_rev_dtkid 
        where sat = @sat and rev = @rev
    if @@rowcount > 1
    begin
        print "aps_sp_new_dtkid:  Error:"
        print 
        "    Multiple entries in sat_rev_dtkid table for sat='%1!' rev=%2!.",
            @sat, @rev
        print "    Please contact DBA."
        rollback tran  aps_sp_dtkid_transaction
        return -1002   /* multiple entries in sat_rev_dtkid  */
    end
    if @@error != 0
    begin
        print "aps_sp_new_dtkid:  Error:"
        print 
            "    Failed select from sat_rev_dtkid table for sat='%1!' rev=%2!.",
            @sat, @rev
        rollback tran  aps_sp_dtkid_transaction
        return -1003  /* Failed select from sat_rev_dtkid  */
    end
    if @dtkid >= 99
    begin
        print "aps_sp_new_dtkid:  Error:"
        print 
"    Dtkid=%3! at maximum for sat='%1!' rev=%2! in sat_rev_dtkid relation.",
            @sat, @rev, @dtkid
        print "    Contact DBA."
        rollback tran  aps_sp_dtkid_transaction
        return -1004   /*  dtkid at maximum value  */
    end

    /* increment dtkid number */
    if @dtkid = null
    begin
        select @dtkid = 1
        insert into sat_rev_dtkid (sat, rev, dtkid)
        values (@sat, @rev, @dtkid)
        if @@error != 0
        begin
            print "aps_sp_new_dtkid:  Error:"
            print 
"    Failed insert into sat_rev_dtkid table for sat='%1!' rev=%2! dtkid=%3!.",
                @sat, @rev,  @dtkid 
            print "    Probably, input sat='%1!' is an illegal value...",
                @sat
            rollback tran  aps_sp_dtkid_transaction
            return -1005   /*  Failed insert into sat_rev_dtkid  */
        end
    end
    else if @dtkid < 1
    begin
        print "aps_sp_new_dtkid:  Error:"
        print 
"    Invalid dtkid=%3! in sat_rev_dtkid table for sat='%1!' rev=%2!. ",
            @sat, @rev, @dtkid  
        print "    Please contact DBA."
        rollback tran  aps_sp_dtkid_transaction
        return -1006   /*  Invalid dtkid in sat_rev_dtkid table  */
    end
    else
    begin
        select @dtkid = @dtkid + 1
        update sat_rev_dtkid 
        set dtkid = @dtkid where sat = @sat and rev = @rev
        if @@error != 0
        begin
            print "aps_sp_new_dtkid:  Error:"
            print 
"    Failed update on sat_rev_dtkid table for sat='%1!' rev=%2! dtkid=%3!.",
                @sat, @rev,  @dtkid
            print "    Please contact DBA."
            rollback tran  aps_sp_dtkid_transaction
            return -1007  /*  Failed update on sat_rev_dtkid  */
        end
    end

    /*   COMMIT TRANSACTION    */
    commit tran  aps_sp_dtkid_transaction

    select @dtkid

    return    /* returns the new dtkid  */
end
go

/* allow the aps_seq_acct to run this stored procedure:  */
grant execute on aps_sp_new_dtkid to aps_seq_acct
go

END_OF_HEREDOC
banner done structure
#
#  COPY FROM FILES TO DB:
#
#  sat_rev_dtkid relation:  
set RELATION = sat_rev_dtkid
echo "bcp $APSDB..$RELATION in bcp.$DATE_BCP.$RELATION  -c -Ularry -Piceland"
date
bcp $APSDB..$RELATION in bcp.$DATE_BCP.$RELATION -c -Ularry -Piceland 
date

#  dl2obs relation:  
set RELATION = dl2obs
echo "bcp $APSDB..$RELATION in bcp.$DATE_BCP.$RELATION  -c -Ularry -Piceland"
date
bcp $APSDB..$RELATION in bcp.$DATE_BCP.$RELATION -c -Ularry -Piceland 
date

#  dtk relation:  
set RELATION = dtk
echo "bcp $APSDB..$RELATION in bcp.$DATE_BCP.$RELATION  -c -Ularry -Piceland"
date
bcp $APSDB..$RELATION in bcp.$DATE_BCP.$RELATION -c -Ularry -Piceland 
date

banner "DONE bcp"
isql -e -P iceland -U larry  << END_OF_HEREDOC
use $APSDB
go
dump tran $APSDB with truncate_only
go
END_OF_HEREDOC
#
#  Initialization Complete.  
#
echo " "
date
echo "Initialization Complete"
echo " "
banner DONE INIT.csh
