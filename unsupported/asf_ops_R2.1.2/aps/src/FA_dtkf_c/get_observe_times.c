#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       get_observe_times.c

Description:    determines the list of time period entries for a 
                DAR/reqq_phase.  

Returns:        the number of these time periods.  

==============================================================================*/
#pragma ident   "@(#)get_observe_times.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/FA_dtkf_c/SCCS/s.get_observe_times.c"

#include <stdio.h>
#include <stdlib.h>         /* for exit() etc. */
#include <string.h>         /* for strcmp() etc. */
#include <timeconv.h>       /* for tc_validate_asf_datetime() etc. */
#include <dapps_defs.h>     /* for ASF_TIME_STR_LENGTH        etc. */



/*==============================================================================
Function:       month_day()

Description:    input:  year, day number within year, starting with 1.  
                output:  month, day.  takes into account leap years.  

Creator:        Brian Griglak

Creation Date:  Tue Aug  6 15:49:07 PDT 1997

==============================================================================*/

/* macro:  = 1 if a leap year:  */
#define LEAP(year) ( (year)%4 == 0 && (year) %100 != 0 || (year)%400 == 0 )

static void month_day(int year, int yearday, int *pmonth, int *pday)
{
    static char days[2][13] = {
      {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
      {0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31} };
    int i, leap;
 
    leap = LEAP(year) ;
    for (i = 1; yearday > days[leap][i]; i++)
        yearday -= days[leap][i];
    *pmonth = i;
    *pday = yearday;
} 
 

/* returns number of time periods calculated.  */
int get_observe_times(
    char *phase_start,   /* input phase time period.  */
    char *phase_end,
    char *dar_start,     /* input dar time period.    */
    char *dar_end,
    int  J1_obs_freq_days,/* input no. days per image during dar time period.*/
    char *observe_time[],  /* array to store all the time brackets     */
                           /* each member is like this:  yyyymmddYYYYMMDD   */
                           /* providing the start and end of time period.   */
    int  observe_time_size )  /* number of members in array observe_time[] */
{

    double  overlap_jd1, overlap_jd2 ;   /* time bracket of dar/phase overlap */

    double phase_jd1, phase_jd2 ;
    double dar_jd1, dar_jd2 ;

    int month, day;
    int good, j;

    /* this data is used to manage the observation periods for REQQ rec.  */
    int     return_code ;
    char    overlap_start_asf[ASF_TIME_STR_LENGTH+1] ;
    char    overlap_end_asf[ASF_TIME_STR_LENGTH+1] ;
    int     year_overlap_start, day_overlap_start ;
    int     year_overlap_end, day_overlap_end ;
    int     year_observe_start, day_observe_start ;
    int     year_observe_end, day_observe_end ;


    good = tc_asf2julian(phase_start, &phase_jd1);
    if (good)
        good = tc_asf2julian(phase_end, &phase_jd2);
    if (good)
        good = tc_asf2julian(dar_start, &dar_jd1);
    if (good)
        good = tc_asf2julian(dar_end, &dar_jd2);
    if (!good)
        return 0;

    /*
    --  what is an overlap?
    --
    --  Definition of Overlap time period; it is the time period which 
    --  is included by both the Dar time period and the reqq phase time 
    --  period:
    -- 
    --  time ->
    -- 
    --    Dar:     |---------|
    --    Phase:          |------|
    --    Overlap:        |--|
    -- 
    --  Another example:
    -- 
    --    Dar:     |-------------------------|
    --    Phase:          |------|
    --    Overlap:        |------|
    */ 

    /* 
    -- check for a time overlap.  if none, OK, return.  
    */
    if( phase_jd2 <= dar_jd1 || phase_jd1 >= dar_jd2 )
        return 1 ;

    /*
    -- there is an overlap.  
    */
    overlap_jd1 = phase_jd1 ;
    overlap_jd2 = phase_jd2 ;

    /* 
    -- shrink overlap time bracket, if necessary, to fit 
    -- DAR time bracket, to complete overlap time bracket 
    -- calculation:  
    */
    if( overlap_jd1 < dar_jd1 )
        overlap_jd1 = dar_jd1 ;
    if( overlap_jd2 > dar_jd2 )
        overlap_jd2 = dar_jd2 ;

    /*
    --  Suppose images are required every 3 days and the Overlap time 
    --  period is 10 days:  
    -- 
    --           .- start of Overlap and first time period.  
    --          /   .- start of second time period, 3 days later.  
    --         |   /   .- start of third time period, 3 days after 2nd.  
    --         |  |   /   .- start of final time period.  
    --         |  |  |   /   .- end of Overlap and final time period.  
    --         |  |  |  |  /
    --         |  |  |  | | 
    --         |--|--|--|-|
    --          1  2  3  4      There are 4 time periods in this case.  
    --         A  B  C  D E
    -- time ->
    --    
    --  The time brackets for each requested observation would be:  
    --
    --  time period     Start time    End time     Index to observe_time[]
    --
    --            1     A             B            0
    --            2     B             C            1
    --            3     C             D            2
    --            4     D             E            3
    */

    /*
    -- The output time periods are given in just date, not 
    -- time.  
    -- Therefore, first convert the overlap time bracket into 
    -- year and day number.  
    -- and then add integer days for each 
    -- successive time period.  
    */
    /* 
    -- note:  we want the time periods to look like this:
    --        in the REQQ file: 
    --        start      end
    -- obs 1  1997 1201  1997 1210
    -- obs 2  1997 1211  1997 1220
    -- obs 3  1997 1221  1997 1230
    --        etc.  
    -- for observations every 10 days.  
    -- 
    -- This is what we don't want:  
    --        start      end
    -- obs 2  1997 1201  1997 1211
    -- obs 2  1997 1211  1997 1221
    -- obs 3  1997 1221  1997 1231
    --
    -- See the difference?  since only days can go into 
    -- the REQQ file, and not times, an end day of 
    -- 1997 12 10 is taken to have a time of 23:59:59, that 
    -- is, the time period includes ALL of the ending day.  
    -- Therefore we don't ALSO want to include this same 
    -- ending day into the next observation time period.  
    -- 
    */

    /* set up start and end year and day:  */
    good = tc_julian2asf(&overlap_jd1, overlap_start_asf);
    if(!good)
    {
        (void) printf(
            "%s(%d):  SERIOUS ERROR:  tc_julian2asf(%f) returned FALSE.\n",
            __FILE__, __LINE__, overlap_jd1  ) ;
        return 0 ;
    }
    return_code = sscanf(overlap_start_asf, "%d:%d:", 
        &year_overlap_start,&day_overlap_start);
    if( return_code !=2 )
    {
        (void) printf("%s(%d):  SERIOUS ERROR:  sscanf(%s) did not decode.\n",
            __FILE__, __LINE__, overlap_start_asf ) ;
        return 0 ;  /* did not decode 2 numbers.  */
    }
    good = tc_julian2asf(&overlap_jd2, overlap_end_asf);
    if(!good)
    {
        (void) printf(
            "%s(%d):  SERIOUS ERROR:  tc_julian2asf(%f) returned FALSE.\n",
            __FILE__, __LINE__, overlap_jd2 ) ;
        return 0 ;
    }
    return_code = sscanf(overlap_end_asf, "%d:%d:", 
        &year_overlap_end, &day_overlap_end);
    if( return_code !=2 )
    {
        (void) printf("%s(%d):  SERIOUS ERROR:  sscanf(%s) did not decode.\n",
            __FILE__, __LINE__, overlap_end_asf ) ;
        return 0 ;  /* did not decode 2 numbers.  */
    }

    year_observe_start = year_overlap_start ;
    day_observe_start  = day_overlap_start ;

    j = 0 ;
    /* 
    -- while always:  to satisfy lint:  
    -- the loop breakout is at the loop bottom:  
    */
    while ( year_observe_start <= year_overlap_end )
    {

        /* 
        -- determine the end of the observation period:  
        -- add the observing time period minus one day.  
        -- for example, if the observing time period is 1 day, 
        -- the time period starts and ends on the SAME day.  
        -- the NEXT time period would start on the NEXT day.  
        */
        year_observe_end = year_observe_start ;
        day_observe_end = day_observe_start + J1_obs_freq_days - 1 ;

        /* 
        -- now check for going over the end of a year during 
        -- the observation period:
        */
        if( day_observe_end > (365 + LEAP(year_observe_end))   )
        {
            /* 
            -- LEAP() is a macro in this file.  
            -- = 1 if a leap year, 0 if not.  
            --
            -- we did go over into the next year:  
            -- must change the year and day.  
            -- note:  change the year_observe_end last!
            */
            day_observe_end -= ( 365 + LEAP(year_observe_end) ) ;
            year_observe_end ++ ;
            
        }
        /* 
        -- set end observation to end of overlap?
        -- check for beyond the overlap 
        -- time period:  
        */
        if(     year_observe_end > year_overlap_end  
            ||  (    year_observe_end == year_overlap_end  
                  && day_observe_end > day_overlap_end )   
          )
        {
            /* 
            -- end of observation is beyond end of overlap.  
            -- re-set the end of observation to EQUAL it.  
            */
            year_observe_end = year_overlap_end ;
            day_observe_end  = day_overlap_end ;
        }

        if( j >= observe_time_size )
        {
            /* protecting against array overrun:  */
            (void) fprintf(stderr, 
            "%s(%d):  too few members allocated for observe_time[] in call:\n", 
                __FILE__, __LINE__ ) ;
            (void) fprintf(stderr, "  phase_start = %s\n", phase_start ) ;
            (void) fprintf(stderr, "  phase_end = %s\n", phase_end ) ;
            (void) fprintf(stderr, "  dar_start = %s\n", dar_start ) ;
            (void) fprintf(stderr, "  dar_end = %s\n", dar_end ) ;
            (void) fprintf(stderr, "  J1_obs_freq_days = %d\n", 
                J1_obs_freq_days ) ;
            (void) fprintf(stderr, "  observe_time_size = %d\n", 
                observe_time_size ) ;
            return 0 ;
        }

        /* write the start entry:     */
        month_day(year_observe_start, day_observe_start, &month, &day);
        (void) sprintf(observe_time[j], "%d%02d%02d", 
            year_observe_start, month, day);
            
        /* complete the entry with end info and increment j  */
        month_day( year_observe_end, day_observe_end, &month, &day);
        (void) sprintf(observe_time[j++] + 8, "%d%02d%02d", 
            year_observe_end, month, day);

        /* 
        -- determine the start info 
        -- for the next observe period:  
        -- add the observing time period.  
        -- for example, if the observing time period is 1 day, 
        -- the next time period starts one day later.  
        */
        day_observe_start += J1_obs_freq_days ;

        /* 
        -- now check for going into the next year:  
        */
        if( day_observe_start > (365 + LEAP(year_observe_start))   )
        {
            /* 
            -- LEAP() is a macro in this file.  
            -- = 1 if a leap year, 0 if not.  
            --
            -- we DID go over into the next year:  
            -- must change the year and day.  
            -- note:  change the year_observe_end last!
            */
            day_observe_start -= ( 365 + LEAP(year_observe_start) ) ;
            year_observe_start ++ ;
            
        }

        /* 
        -- BREAK OUT OF LOOP?   
        */

        /* 
        -- is start time after end of overlap?
        -- check:  
        */
        if(     year_observe_start > year_overlap_end  
            ||  (    year_observe_start == year_overlap_end  
                  && day_observe_start > day_overlap_end )   
          )
        {
            /* 
            -- start of observation is beyond end of overlap.  
            -- BREAK OUT OF LOOP:  
            */
            break ;
        }

    }

    /* 
    -- at this point, j is the number of 
    -- time periods.  
    */

    return j ;

}
