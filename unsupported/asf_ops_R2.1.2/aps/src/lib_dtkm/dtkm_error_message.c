#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   dtkm_error_message.c

Description:    contains the error messages

Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_error_message.c	5.2 98/03/03 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_error_message.c"



/*==============================================================================
Function:       dtkm_error_message.c

Description:    decodes a negative error code into a string.  
                The macro DTKM_ERROR_MESSAGE() decodes a negative 
                return code into one of these strings.  

Creator:        Lawrence Stevens

Creation Date:  Sat Oct 28 08:52:26 PDT 1995

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/

char    *dtkm_error_message[] =
{
    "zero is not an error code",
    "DTKM_ERROR_ACTIVITY_GT_1_RECORD",                                /*   -1 */
    "DTKM_ERROR_ACTIVITY_ILLEGAL",                                    /*   -2 */
    "DTKM_ERROR_ACTIVITY_NOT_FOUND",                                  /*   -3 */
    "DTKM_ERROR_ANTENNA_DOWN_TIMES_LIST_NOT_EMPTY",                   /*   -4 */
    "DTKM_ERROR_ANTENNA_DOWN_TIMES_LIST_NOT_INITIALIZED",             /*   -5 */
    "DTKM_ERROR_ANTENNA_ID_IS_ZERO_NO_PRIORITY",                      /*   -6 */
    "DTKM_ERROR_ANTENNA_ID_LE_ZERO" ,                                 /*   -7 */
    "DTKM_ERROR_ANTENNA_NOT_FOUND_IN_LIST" ,                          /*   -8 */
    "DTKM_ERROR_ANTENNA_PREFERENCE_NOT_FOUND_FOR_DTK_PROPOSAL",       /*   -9 */
    "DTKM_ERROR_ANTENNA_PRIORITY_NOT_FOUND_FOR_DTK_PROPOSAL",         /*  -10 */
    "DTKM_ERROR_ANTENNA_REC_NOT_FOUND_FOR_DTK_PROPOSAL",              /*  -11 */
    "DTKM_ERROR_APS_READER_SESSION_COULD_NOT_BE_OPENED",              /*  -12 */
    "DTKM_ERROR_ATTEMPTING_TO_DELETE_DTK",                            /*  -13 */
    "DTKM_ERROR_ATTEMPT_TO_DELETE_NONEXISTING_DTK",                   /*  -14 */
    "DTKM_ERROR_ATTEMPT_TO_UPDATE_NONEXISTING_DTK",                   /*  -15 */
    "DTKM_ERROR_ATTEMPT_TO_UPDATE_PRIMARY_KEY",                       /*  -16 */
    "DTKM_ERROR_BAD_ACTID_VALUE",                                     /*  -17 */
    "DTKM_ERROR_BAD_ASCDSC_VALUE",                                    /*  -18 */
    "DTKM_ERROR_BAD_DTKSTAT_VALUE",                                   /*  -19 */
    "DTKM_ERROR_BAD_FARLAT1_VALUE",                                   /*  -20 */
    "DTKM_ERROR_BAD_FARLAT2_VALUE",                                   /*  -21 */
    "DTKM_ERROR_BAD_FARLON1_VALUE",                                   /*  -22 */
    "DTKM_ERROR_BAD_FARLON2_VALUE",                                   /*  -23 */
    "DTKM_ERROR_BAD_NRLAT1_VALUE",                                    /*  -24 */
    "DTKM_ERROR_BAD_NRLAT2_VALU",                                     /*  -25 */
    "DTKM_ERROR_BAD_NRLON1_VALUE",                                    /*  -26 */
    "DTKM_ERROR_BAD_NRLON2_VALUE",                                    /*  -27 */
    "DTKM_ERROR_BAD_REV_VALUE",                                       /*  -28 */
    "DTKM_ERROR_BAD_SAT_SENSOR_VALUES",                               /*  -29 */
    "DTKM_ERROR_BAD_SAT_STATION_ID_VALUE",                            /*  -30 */
    "DTKM_ERROR_BAD_SCHEDULE_ID_FOR_R1_DTK",                          /*  -31 */
    "DTKM_ERROR_BAD_SENSOR_VALUE",                                    /*  -32 */
    "DTKM_ERROR_BAD_STOPLAT_VALUE",                                   /*  -33 */
    "DTKM_ERROR_BAD_STOPTIME_VALUE",                                  /*  -34 */
    "DTKM_ERROR_BAD_STRTLAT_VALUE",                                   /*  -35 */
    "DTKM_ERROR_BAD_STRTTIME_VALUE",                                  /*  -36 */
    "DTKM_ERROR_BAD_TRANSID_VALUE",                                   /*  -37 */
    "DTKM_ERROR_BUMPED_DTK_WAS_DELETED_OR_REJECTED",                  /*  -38 */
    "DTKM_ERROR_BUMPS_LIST_NOT_EMPTY",                                /*  -39 */
    "DTKM_ERROR_BUMPS_LIST_NOT_INITIALIZED",                          /*  -40 */
    "DTKM_ERROR_CANNOT_IDENTIFY_DTK_TO_UPDATE",                       /*  -41 */
    "DTKM_ERROR_COMPUTING_TIME_DIFF",                                 /*  -42 */
    "DTKM_ERROR_CONCURS_LIST_NOT_EMPTY",                              /*  -43 */
    "DTKM_ERROR_CONCURS_LIST_NOT_INITIALIZED",                        /*  -44 */
    "DTKM_ERROR_CONFLICTS_LIST_NOT_EMPTY",                            /*  -45 */
    "DTKM_ERROR_CONFLICTS_LIST_NOT_INITIALIZED",                      /*  -46 */
    "DTKM_ERROR_DAR_NOT_FOUND_FROM_DTK_DARID",                        /*  -47 */
    "DTKM_ERROR_DB_QUERY_FAILED",                                     /*  -48 */
    "DTKM_ERROR_DELETING_DTK_RECORD_WHEN_CHANGING_SENSOR",            /*  -49 */
    "DTKM_ERROR_DELETION_DURING_UPDATE",                              /*  -50 */
    "DTKM_ERROR_DTKID_LT_ZERO",                                       /*  -51 */
    "DTKM_ERROR_DTKID_TOO_BIG",                                       /*  -52 */
    "DTKM_ERROR_DTK_CONCURS_LIST_HAS_NEITHER_0_NOR_1_MEMBERS",        /*  -53 */
    "DTKM_ERROR_DTK_LIST_IS_MIXED_WITH_DIFFERENT_ANTENNAS",           /*  -54 */
    "DTKM_ERROR_DTK_NOT_INSERTED",                                    /*  -55 */
    "DTKM_ERROR_DTK_NOT_INSERTED_DURING_UPDATE",                      /*  -56 */
    "DTKM_ERROR_DTK_SAT_LIST_NOT_EMPTY",                              /*  -57 */
    "DTKM_ERROR_DTK_SAT_LIST_NOT_INITIALIZED",                        /*  -58 */
"DTKM_ERROR_DTK_SHOULD_BE_DEL_REJ_INV_QUE_SUB_REQ_PLN_OR_SCH_STATUS", /*  -59 */
    "DTKM_ERROR_DTK_UPDATES_LIST_NOT_EMPTY",                          /*  -60 */
    "DTKM_ERROR_DTK_UPDATES_LIST_NOT_INITIALIZED",                    /*  -61 */
    "DTKM_ERROR_EXPANDING_TIME_BRACKET",                              /*  -62 */
    "DTKM_ERROR_FARLAT1_AND_FARLAT2_EQUAL_ZERO",                      /*  -63 */
    "DTKM_ERROR_FARLON1_AND_FARLON2_EQUAL_ZERO",                      /*  -64 */
    "DTKM_ERROR_FIELD_ACTID_NOT_SET",                                 /*  -65 */
    "DTKM_ERROR_GT_1_DTK_DELETED",                                    /*  -66 */
    "DTKM_ERROR_GT_1_DTK_DELETED_DURING_UPDATE",                      /*  -67 */
    "DTKM_ERROR_GT_1_DTK_REC_WITH_SAME_SAT_SENSOR_REV_DTKID",         /*  -68 */
    "DTKM_ERROR_GT_ONE_ANTENNA_USED_ON_SAME_PASS",                    /*  -69 */
    "DTKM_ERROR_ILLEGAL_CONF_STATUS_IN_ACTIV_CONF_RELATION",          /*  -70 */
    "DTKM_ERROR_INPUT_ANTENNA_LIST_NOT_INITIALIZED",                  /*  -71 */
    "DTKM_ERROR_INPUT_DTK_LIST_EMPTY",                                /*  -72 */
    "DTKM_ERROR_INPUT_DTK_LIST_NOT_EMPTY",                            /*  -73 */
    "DTKM_ERROR_INPUT_DTK_LIST_NOT_INITIALIZED",                      /*  -74 */
    "DTKM_ERROR_INPUT_DTK_WITH_DTKID_LT_ZERO",                        /*  -75 */
    "DTKM_ERROR_INPUT_NULL_POINTER_FOR_VALUE",                        /*  -76 */
    "DTKM_ERROR_INSERTING_DTK_WITH_EXISTING_PRIMARY_KEY" ,            /*  -77 */
    "DTKM_ERROR_IN_CODE_DTK_CREATED_BUMPS",                           /*  -78 */
    "DTKM_ERROR_IN_CODE_IN_dtkm_check_same_sat_conflicts",            /*  -79 */
    "DTKM_ERROR_IN_CODE_dtkm_activities_conflict",                    /*  -80 */
    "DTKM_ERROR_IN_CODE_dtkm_extract_conflicts_and_parallels_else",   /*  -81 */
    "DTKM_ERROR_IN_CODE_dtkm_extract_conflicts_and_parallels_if",     /*  -82 */
    "DTKM_ERROR_IN_CODE_dtkm_j1_equipment_status_check",              /*  -83 */
    "DTKM_ERROR_IN_CREATING_SAME_SAT_TIME_BRACKET",                   /*  -84 */
    "DTKM_ERROR_IN_DUPLICATING_DTK_RECORD_INTO_LIST",                 /*  -85 */
    "DTKM_ERROR_IN_MAKING_RETRIEVE_TIME_BRACKET",                     /*  -86 */
    "DTKM_ERROR_IN_MOVING_RECORDS_TO_OTHER_LIST",                     /*  -87 */
    "DTKM_ERROR_IN_MOVING_REC_TO_OTHER_LIST",                         /*  -88 */
    "DTKM_ERROR_LIST_NOT_INITIALIZED",                                /*  -89 */
    "DTKM_ERROR_LOWEST_DTKID_NOT_FOUND",                              /*  -90 */
    "DTKM_ERROR_LT_ZERO_ELEMENTS_IN_LIST",                            /*  -91 */
    "DTKM_ERROR_NE_1_DARS_FOUND_FROM_DTK_DARID",                      /*  -92 */
    "DTKM_ERROR_NO_ANTENNAS_FOUND_IN_ANTENNA_PREF_TABLE" ,            /*  -93 */
    "DTKM_ERROR_NO_CVRG_MUST_RUN_CREATE_NOMINAL_COVERAGE",            /*  -94 */
    "DTKM_ERROR_NO_RECORDS_IN_ANTENNA_PREF_RELATION",                 /*  -95 */
    "DTKM_ERROR_NO_RECORDS_IN_ANTENNA_PRIORITY_RELATION",             /*  -96 */
    "DTKM_ERROR_NO_RECORDS_IN_ANTENNA_RELATION",                      /*  -97 */
    "DTKM_ERROR_NO_RECS_IN_ACTIVITIES_RELATION",                      /*  -98 */
    "DTKM_ERROR_NO_STOPTIME",                                         /*  -99 */
    "DTKM_ERROR_NO_STRTTIME",                                         /* -100 */
    "DTKM_ERROR_NRLAT1_AND_NRLAT2_EQUAL_ZERO",                        /* -101 */
    "DTKM_ERROR_NRLON1_AND_NRLON2_EQUAL_ZERO",                        /* -102 */
    "DTKM_ERROR_NULL_ANTENNA_LIST_PTR" ,                              /* -103 */
    "DTKM_ERROR_NULL_DTK_2B_COMBINED",                                /* -104 */
    "DTKM_ERROR_NULL_DTK_PROPOSAL",                                   /* -105 */
    "DTKM_ERROR_NULL_DTK_RESULT_RECORD",                              /* -106 */
    "DTKM_ERROR_NULL_OUTPUT_DTK_LIST",                                /* -107 */
    "DTKM_ERROR_NULL_PROPOSED_DTK_SEG",                               /* -108 */
    "DTKM_ERROR_NULL_RECORD" ,                                        /* -109 */
    "DTKM_ERROR_OUTPUT_ACCEPTED_DTK_LIST_IS_NOT_EMPTY",               /* -110 */
    "DTKM_ERROR_OUTPUT_ACCEPTED_DTK_LIST_NOT_INITIALIZED",            /* -111 */
    "DTKM_ERROR_OUTPUT_CON_DTK_LIST_IS_NOT_EMPTY",                    /* -112 */
    "DTKM_ERROR_OUTPUT_CON_DTK_LIST_NOT_INITIALIZED",                 /* -113 */
    "DTKM_ERROR_OUTPUT_DELETED_DTK_LIST_IS_NOT_EMPTY",                /* -114 */
    "DTKM_ERROR_OUTPUT_DELETED_DTK_LIST_NOT_INITIALIZED",             /* -115 */
    "DTKM_ERROR_OUTPUT_ERROR_DTK_LIST_IS_NOT_EMPTY",                  /* -116 */
    "DTKM_ERROR_OUTPUT_ERROR_DTK_LIST_NOT_INITIALIZED",               /* -117 */
    "DTKM_ERROR_OUTPUT_OMISSION_DTK_LIST_IS_NOT_EMPTY",               /* -118 */
    "DTKM_ERROR_OUTPUT_OMISSION_DTK_LIST_NOT_INITIALIZED",            /* -119 */
    "DTKM_ERROR_OUTPUT_OTHER_SAT_DTKS_LIST_IS_NOT_EMPTY",             /* -120 */
    "DTKM_ERROR_OUTPUT_OTHER_SAT_DTKS_LIST_NOT_INITIALIZED",          /* -121 */
    "DTKM_ERROR_OUTPUT_REJECTED_DTK_LIST_IS_NOT_EMPTY",               /* -122 */
    "DTKM_ERROR_OUTPUT_REJECTED_DTK_LIST_NOT_INITIALIZED",            /* -123 */
    "DTKM_ERROR_OUTPUT_SAME_SAT_DTKS_LIST_IS_NOT_EMPTY",              /* -124 */
    "DTKM_ERROR_OUTPUT_SAME_SAT_DTKS_LIST_NOT_INITIALIZED",           /* -125 */
    "DTKM_ERROR_PADDING_ANTENNA_TIME_BRACKET",                        /* -126 */
    "DTKM_ERROR_PADDING_SAME_SAT_TIME_BRACKET",                       /* -127 */
    "DTKM_ERROR_PARALLELS_LIST_NOT_EMPTY",                            /* -128 */
    "DTKM_ERROR_PARALLELS_LIST_NOT_INITIALIZED",                      /* -129 */
    "DTKM_ERROR_PROCESSING_BUMPED_DTKS",                              /* -130 */
    "DTKM_ERROR_OBSERVATION_DTK_HAS_ANTENNA_ID_NE_ZERO",              /* -131 */
    "DTKM_ERROR_SAME_PASS_LIST_NOT_EMPTY",                            /* -132 */
    "DTKM_ERROR_SAME_PASS_LIST_NOT_INITIALIZED",                      /* -133 */
    "DTKM_ERROR_SAT_DOES_NOT_HAVE_A_RECORDER",                        /* -134 */
    "DTKM_ERROR_SAT_DOWN_TIMES_LIST_NOT_EMPTY",                       /* -135 */
    "DTKM_ERROR_SAT_DOWN_TIMES_LIST_NOT_INITIALIZED",                 /* -136 */
    "DTKM_ERROR_SAT_NOT_A1",                                          /* -137 */
    "DTKM_ERROR_SAT_NOT_E1",                                          /* -138 */
    "DTKM_ERROR_SAT_NOT_E2",                                          /* -139 */
    "DTKM_ERROR_SAT_NOT_J1",                                          /* -140 */
    "DTKM_ERROR_SAT_NOT_R1",                                          /* -141 */
    "DTKM_ERROR_SAT_NOT_R1_FOR_dtkm_r1_update_sensor",                /* -142 */
    "DTKM_ERROR_SENSOR_AND_ACTID_DONT_MATCH",                         /* -143 */
    "DTKM_ERROR_SIMILARS_LIST_IS_EMPTY",                              /* -144 */
    "DTKM_ERROR_SIMILARS_LIST_NOT_EMPTY",                             /* -145 */
    "DTKM_ERROR_SIMILARS_LIST_NOT_INITIALIZED",                       /* -146 */
    "DTKM_ERROR_STATUS_IS_NEITHER_SCH_NOR_PLN_WHEN_COMBINING_DTKS",   /* -147 */
    "DTKM_ERROR_STATUS_NOT_QUE_PLN_REQ_OR_SCH",                       /* -148 */
    "DTKM_ERROR_STOPTIME_LE_STRTTIME",                                /* -149 */
    "DTKM_ERROR_STRTLAT_AND_STOPLAT_EQUAL_ZERO",                      /* -150 */
    "DTKM_ERROR_TIMES_NOT_WITHIN_REV",                                /* -151 */
    "DTKM_ERROR_UNKNOWN_NORMAL_RETURN_CODE_FROM_DTKM_PROCESS_DTK",    /* -152 */
    "DTKM_ERROR_UNKNOWN_RETURN_CODE_FROM_dtkm_check_bumpability",     /* -153 */
    "DTKM_ERROR_UNKNOWN_RETURN_CODE_FROM_dtkm_update_best_antenna",   /* -154 */
    "DTKM_ERROR_UNKNOWN_SAT",                                         /* -155 */
    "DTKM_ERROR_UNKNOWN_SAT_IN_dtkm_check_transid",                   /* -156 */
    "DTKM_ERROR_UNLINKING_RECORD",                                    /* -157 */
    "DTKM_ERROR_NO_MASKINOUT_RECS_FOUND_IN_REV",                      /* -158 */
"DTKM_ERROR_MASKINOUT_PROBLEM_PLEASE_RUN_aps_crt_nom_mask_FOR_DTK",   /* -159 */
    "DTKM_ERROR_CONVERTING_ET2ASF",                                   /* -160 */
    "DTKM_ERROR_REC_NEITHER_IN_NOR_OUT_IN_MASKINOUT",                 /* -161 */
    "DTKM_ERROR_NE_1_INMASK_EVENT_IN_REV",                            /* -162 */
    "DTKM_ERROR_NULL_ANTENNA_DOWN_TIMES_REC",                         /* -163 */
    "DTKM_ERROR_ANTENNA_DOWN_TIMES_REC_NOT_INSERTED",                 /* -164 */
    "DTKM_ERROR_IN_MASKINOUT_RELATION",                               /* -165 */
    "DTKM_ERROR_DOWNLINK_DTK_HAS_NO_TIME_IN_MASK",                    /* -166 */
    "DTKM_ERROR_DOWNLINK_DTK_NOT_ENTIRELY_WITHIN_STATION_PASS",       /* -167 */
    "DTKM_ERROR_UNEXPECTED_RETURN_CODE_FROM_dtkm_in_station_mask",    /* -168 */
    "DTKM_ERROR_MUST_INCREASE_MAX_POSSIBLE_SATS",                     /* -169 */
    "DTKM_ERROR_IN_CODE_dtkm_aps_does_cvrg4sat",                      /* -170 */
    "DTKM_ERROR_COL_NUMBER_GT_NUM_DTK_COLS",                          /* -171 */
    "DTKM_ERROR_BAD_SAT_STATION_ID_ANTENNA_ID_COMBINATION",           /* -172 */
    "-173 unknown error code.  ",
    "DTKM_ERROR_BAD_ACTID_AGENCY_VALUE",                              /* -174 */
    "DTKM_ERROR_UNKNOWN_SAT_IN_dtkm_check_agency",                    /* -175 */
    "DTKM_ERROR_NO_INCLUSION_PERIOD_FOR_SATELLITE",                   /* -176 */
    "DTKM_ERROR_DUPLICATE_DTK_KEYS",                                  /* -177 */
    "DTKM_ERROR_OUTPUT_DTK_LIST_IS_NOT_EMPTY",                        /* -178 */
    "DTKM_ERROR_dl2obs_rec_NOT_INSERTED",                             /* -179 */
    "DTKM_ERROR_DTK_IS_NOT_A_DOWNLINK",                               /* -180 */
    "DTKM_ERROR_GT_1_DMP_LINKED_TO_OBS_DTK",                          /* -181 */
    "DTKM_ERROR_DTK_IS_A_DOWNLINK",                                   /* -182 */
    "DTKM_ERROR_INSERTING_DTK__DTKID_NOT_UNIQUE_TO_SAT_REV",          /* -183 */
    "DTKM_ERROR_NO_STATION_REC_FOR_DTK_SAT",                          /* -184 */
    "DTKM_ERROR_NULL_DTK_DOWNLINK_RECORD",                            /* -185 */
    "DTKM_ERROR_BAD_SUBMIT_TIME_VALUE",                               /* -186 */
    "DTKM_ERROR_A1_REALTIME_SENSOR_DTKSTAT_NOT_INV",                  /* -187 */
    "DTKM_ERROR_TIME_INTERVAL_LT_SAT_INCLUSION_PERIOD",               /* -188 */
    "DTKM_ERROR_SETTING_ZERO_VALUE",                                  /* -189 */
    "DTKM_ERROR_DB_UPDATE_FAILED_ON_DL2OBS_RELATION",                 /* -190 */
    "DTKM_ERROR_DB_DELETE_FAILED_ON_DL2OBS_RELATION",                 /* -191 */
    "DTKM_ERROR_INPUT_DTK_IS_NOT_RDL",                                /* -192 */
    "DTKM_ERROR_NO_SATSENSOR_RECORDS_FOUND",                          /* -193 */
    "DTKM_ERROR_IN_CODE_SEE_STDERR_FOR_CODE_LOCATION",                /* -194 */
    "DTKM_ERROR_DUPLICATING_DTK_LIST",                                /* -195 */
    "DTKM_ERROR_SETTING_SENSOR_AND_ACTID_VALUES",                     /* -196 */
    "DTKM_ERROR_SETTING_VALUES_IN_LLIST",                             /* -197 */
    "DTKM_ERROR_SAT_NOT_SUPPORTED_BY_FUNCTION",                       /* -198 */
    "DTKM_ERROR_DIFFERENT_SATS_IN_INPUT_DTK_LLIST",                   /* -199 */
    "DTKM_ERROR_INPUT_LLIST_MUST_BE_ALL_REALTIME_OBSERVATIONS",       /* -200 */
    "DTKM_ERROR_DUPLICATING_DTK_RECORD",                              /* -201 */
    "DTKM_ERROR_DTK_NOT_UPDATED",                                     /* -202 */
    "DTKM_ERROR_DETERMINING_PHASE_FOR_DTK",                           /* -203 */
    "DTKM_ERROR_COPYING_DTK_RECORDS_TO_LLIST",                        /* -204 */
    "DTKM_ERROR_BAD_SAT_VALUE",                                       /* -205 */
    "DTKM_ERROR_BAD_DTKID_VALUE",                                     /* -206 */
    "DTKM_ERROR_DTK_INSERT_REQUEST_DTK_HAS_DTKID_NE_ZERO",            /* -207 */
    "DTKM_ERROR_DTK_PROPOSAL_DTKID_GT_ZERO_AND_NOT_IN_DB",            /* -208 */
    "DTKM_ERROR_GETTING_NEW_DTKID",                                   /* -209 */
    "DTKM_ERROR_DTK_IS_NEITHER_OBSERVATION_NOR_DOWNLINK",             /* -210 */
    "DTKM_ERROR_DB_UPDATE_FAILED_ON_DTK_RELATION",                    /* -211 */
    "DTKM_ERROR_INCREASE_CHECK_LIST_SIZE_in_dtkm_fix_dl2obs",         /* -212 */
    "DTKM_ERROR_PLANNER_QL_ARG_VALUE_NOT_Y_OR_N",                     /* -213 */
    "DTKM_ERROR_SCIENCE_QL_ARG_VALUE_NOT_Y_OR_N",                     /* -214 */
    "DTKM_ERROR_IN_TIME_CONVERSION",                                  /* -215 */
    "-216 error string unknown.  Update dtkm_error_message.c", 
    "-217 error string unknown.  Update dtkm_error_message.c", 
    "-218 error string unknown.  Update dtkm_error_message.c", 
    "-219 error string unknown.  Update dtkm_error_message.c", 
    "-220 error string unknown.  Update dtkm_error_message.c", 
    "-221 error string unknown.  Update dtkm_error_message.c", 
    "-222 error string unknown.  Update dtkm_error_message.c", 
    "-223 error string unknown.  Update dtkm_error_message.c", 
    "-224 error string unknown.  Update dtkm_error_message.c", 
    "-225 error string unknown.  Update dtkm_error_message.c", 
    " END OF THE LIST!!!       "
} ;
