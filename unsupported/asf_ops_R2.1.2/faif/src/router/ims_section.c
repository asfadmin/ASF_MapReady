               if (*fpmfpair->file_name != NULL)
               {
               dataset = (char *)util_do_malloc(sizeof(char)*
                          (strlen(platform) + 1
                          + strlen(fpmfpair->dataset_suffix) + 1)) ;
               strcpy(dataset, platform) ;
               strcat(dataset, " ") ;
               strcat(dataset, fpmfpair->dataset_suffix) ;
               }
               else
               {
                  dataset = (char *)util_do_malloc(sizeof(char)*
                             + strlen(fpmfpair->dataset_suffix)) ;
                  strcpy(dataset, fpmfpair->dataset_suffix) ;
               }

               /* Rename the data file (not the PMF), by appending .D to its name.
	       */

               if (*fpmfpair->file_name != NULL)
               {
               strcpy(temp_old_rename, config->FA_receptdir) ;
               strcat(temp_old_rename, "/") ;

               if (fpmfpair->orig_not_tran != TRUE)
                  strcat(temp_old_rename, "tran/") ;

               strcat(temp_old_rename, fpmfpair->file_name) ;
               strcpy(temp_new_rename, temp_old_rename) ;
               strcat(temp_new_rename, ".D") ;

               statur = rename(temp_old_rename, temp_new_rename) ;
               if (statur != 0) 
               {
                  syslog(LOG_ERR,
                   "Failure trying to rename file=%s\n", temp_old_rename);
                  syslog(LOG_ERR,
                   "to new name=%s\n", temp_new_rename);
	          fpmfpair = NEXT(file_pmf_list, pfptr) ;
	          break ;
	       }
               else /* If rename was successful, change internal names also. */
	       {
                  strcat(fpmfpair->file_name, ".D") ;
                  strcat(filerecp->file_name, ".D") ;
	       }
               }

               /* Define the directory from which ims will obtain the files */

               ims_directory = (char *) util_do_malloc(sizeof(char)*
                          (strlen(config->FA_receptdir) + 1 + 4 + 1)) ;

               strcpy(ims_directory, config->FA_receptdir) ;

               strcat(ims_directory, "/") ;

               strcat(ims_directory, "PMF/") ;

               if (*fpmfpair->file_name != NULL)
               {
               syslog(LOG_NOTICE,
                "ims_archive for file = %s, PMF = %s\n",
                fpmfpair->file_name, fpmfpair->PMF_file_name) ;
               }
               else
               {
                  syslog(LOG_NOTICE,
                   "ims_archive for file = %s", fpmfpair->PMF_file_name) ;
               }

               syslog(LOG_NOTICE,
                "platform = %s, dataset_suffix= %s\n",
                platform, fpmfpair->dataset_suffix) ;

     /* The PMF and the original file must both be in the same directory.
        Move the "recept" file down to the /PMF directory. */

               if (*fpmfpair->file_name != NULL)
               {
               strcpy(temp_old_rename_m, config->FA_receptdir) ;
               strcat(temp_old_rename_m, "/") ;

               if (fpmfpair->orig_not_tran == FALSE)
                  strcat(temp_old_rename_m, "tran/") ;

               strcat(temp_old_rename_m, fpmfpair->file_name) ;

               strcpy(temp_new_rename_m, ims_directory);
               strcat(temp_new_rename_m, fpmfpair->file_name) ;

               statur = rename(temp_old_rename_m, temp_new_rename_m) ;
               if (statur != 0) 
               {
                  syslog(LOG_ERR,
                   "Failure trying to rename recpt file=%s to PMFdir\n",
                     temp_old_rename);
	          fpmfpair = NEXT(file_pmf_list, pfptr) ;
	          break ;
	       }
	       }

     /* Send the pair to IMS */
               if (*fpmfpair->file_name != NULL)
               {
               status = ims_archive_FAIF_file(programName,
                config->FA_configfile, 
                ims_directory, fpmfpair->file_name,
                platform, dataset) ;
	       }
               else
               {
                  status = ims_archive_FAIF_PMF(programName,
                   config->FA_configfile, 
                   ims_directory, fpmfpair->PMF_file_name,
                   platform, dataset) ;
	       }

               free(ims_directory) ;

     /* Put the recpt file back where it belongs */
               if (*fpmfpair->file_name != NULL)
               {
               statur = rename(temp_new_rename_m, temp_old_rename_m) ;

               dot_D = (char *) strstr(filerecp->file_name, ".D") ;
               if (dot_D != NULL) strcpy(dot_D, "\0\0") ;

               if (statur != 0) 
               {
                  syslog(LOG_ERR,
                   "Failure trying to rename recpt file=%s to receptdir/\n", 
                     fpmfpair->file_name);
	          fpmfpair = NEXT(file_pmf_list, pfptr) ;
	          break ;
	       }
	       }

               dot_D = (char *) strstr(fpmfpair->file_name, ".D") ;
               if (dot_D != NULL) strcpy(dot_D, "\0\0") ;

               rename(temp_new_rename, temp_old_rename) ;

               free(platform) ;
               free(dataset) ;

   /* Check if files were already logged in log file
   -- Update logged flag field appropriately
   */
         bak_log_file = (char *)util_do_malloc(sizeof(char)*
                         (strlen(config->FA_receptdir) + 1
                         + 4 + strlen("bak.log") + 1)) ;
         strcpy(bak_log_file, config->FA_receptdir);
         strcat(bak_log_file, "/log/bak.log");

         logged_flag = check_bak_logged(fpmfpair->file_name, bak_log_file) ;

         if (strcmp(filerecp->flight_agency, CSA_STR) == 0) logged_flag = FALSE;

         if (logged_flag != TRUE)
         {
            /* Update log to reflect newly routed file
            */
            status_l = assign_filerec_ftime(filerecp);
            if ((status_l = update_bak_log(bak_log_file, fpmfpair->file_name,
                 filerecp->time_received, filerecp->time_forwarded)) == ERROR)
            {
               syslog(LOG_ERR, "WARNING, Error in bak_log operation\n") ;
	       break ;
            }

            /* Copy file to destination directory
            */
            strcpy(file_name_tmp, filerecp->file_name);
            strcpy(filerecp->file_name, fpmfpair->file_name);
            if ((status_l = route_file_to_bak_dir(filerecp, config)) == ERROR)
            {
               syslog(LOG_ERR, 
                "WARNING, Error in copy to back-up dirs operation\n") ;
            }
            strcpy(filerecp->file_name, file_name_tmp);
         }
         free (bak_log_file);

	       if (status != OK)
	       {
                  status = ERROR ;
                  if (*fpmfpair->file_name != NULL)
                  {
		  syslog(LOG_ERR, 
		     "ERROR, Unable to archive file %s to IMS.  Keeping file in reception area.\n",
		     fpmfpair->file_name) ;
                  }
                  else
                  {
                     syslog(LOG_ERR, 
		      "ERROR, Unable to archive file %s to IMS.  Keeping file in reception area.\n",
		     fpmfpair->PMF_file_name) ;
		  }
	       }
	       else
	       {
               if (*fpmfpair->file_name != NULL)
               {
		  syslog(LOG_NOTICE, 
		     "NOTICE:  Archived file %s to IMS.  Deleting file from reception area.\n",
		     fpmfpair->file_name) ;

                  /* Routing and translation completed.
	          -- OK to remove file from incoming dir
	          */
                  remove_file_dir = (char *)util_do_malloc(sizeof(char)*
                          (strlen(config->FA_receptdir) + 1 + 5 + 1)) ;
                  strcpy(remove_file_dir, config->FA_receptdir) ;
                  strcat(remove_file_dir, "/") ;
                  if (fpmfpair->orig_not_tran == FALSE)
                     strcat(remove_file_dir, "tran/") ;

                  status = update_incoming_dir(remove_file_dir, 
	 				 fpmfpair->file_name) ;
                  free  (remove_file_dir) ;

                  if (status == ERROR)
                     syslog(LOG_ERR,
                      "WARNING, Incoming directory update error\n") ;
	       }
               else
               {
		  syslog(LOG_NOTICE, 
		     "NOTICE:  Archived file %s to IMS.\n",fpmfpair->PMF_file_name) ;
	       }

     /* delete the PMF file */
               remove_file_name = (char *)util_do_malloc(sizeof(char)*
                          (strlen(config->FA_receptdir) + 5
                          + strlen(fpmfpair->PMF_file_name) + 1)) ;
               strcpy(remove_file_name, config->FA_receptdir) ;
               strcat(remove_file_name, "/PMF/") ;
               strcat(remove_file_name, fpmfpair->PMF_file_name) ;
               remove(remove_file_name) ;
               free  (remove_file_name) ;

               }

               fpmfpair = NEXT(file_pmf_list, pfptr) ;
            }

            /* Free structures
            */
            DEL_ALL(file_pmf_list) ;
	    break ;

         default:
            sprintf(logmsg, 
               "WARNING, Unrecognized file.  Unable to route %s\n",
                filerecp->file_name) ;
            syslog(LOG_ERR, logmsg) ;
            filerecp = NEXT(filerec_llist, ptr) ;
	    continue ;
