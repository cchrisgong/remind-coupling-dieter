# |  (C) 2006-2020 Potsdam Institute for Climate Impact Research (PIK)
# |  authors, and contributors see CITATION.cff file. This file is part
# |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
# |  AGPL-3.0, you are granted additional permissions described in the
# |  REMIND License Exception, version 1.0 (see LICENSE file).
# |  Contact: remind@pik-potsdam.de
############## Define function: .copy.fromlist #########################

.copy.fromlist <- function(filelist,destfolder) {
  if(is.null(names(filelist))) names(filelist) <- rep("",length(filelist))
  for(i in 1:length(filelist)) {
    if(!is.na(filelist[i])) {
      to <- paste0(destfolder,"/",names(filelist)[i])
      if(!file.copy(filelist[i],to=to,recursive=dir.exists(to),overwrite=T))
        cat(paste0("Could not copy ",filelist[i]," to ",to,"\n"))
    }
  }
}

############## Define function: runsubmit #########################

submit <- function(cfg, restart = FALSE, stopOnFolderCreateError = TRUE) {
  
  if(!restart) {
    # Generate name of output folder and create the folder
    date <- format(Sys.time(), "_%Y-%m-%d_%H.%M.%S")
    cfg$results_folder <- gsub(":date:", date, cfg$results_folder, fixed = TRUE)
    cfg$results_folder <- gsub(":title:", cfg$title, cfg$results_folder, fixed = TRUE)
    # Create output folder
    cat("   Creating results folder",cfg$results_folder,"\n")
    if (!file.exists(cfg$results_folder)) {
      dir.create(cfg$results_folder, recursive = TRUE, showWarnings = FALSE)
    } else if (!cfg$force_replace) {
      couldnotdelete <- paste0("Results folder ",cfg$results_folder," could not be created because it already exists")
      if (stopOnFolderCreateError) {
        stop(couldnotdelete, ".")
      } else if (! all(grepl("^log*.txt", list.files(cfg$results_folder)))) {
        stop(couldnotdelete, " and it contains not only log files.")
      } else {
        message(couldnotdelete, " but it contains only log files.")
      }
    } else {
      cat("    Deleting results folder because it already exists:",cfg$results_folder,"\n")
      unlink(cfg$results_folder, recursive = TRUE)
      dir.create(cfg$results_folder, recursive = TRUE, showWarnings = FALSE)
    }
    
    # remember main folder
    cfg$remind_folder <- getwd()
    
    # Save the cfg (with the updated name of the result folder) into the results folder. 
    # Do not save the new name of the results folder to the .RData file in REMINDs main folder, because it 
    # might be needed to restart subsequent runs manually and should not contain the time stamp in this case.
    filename <- paste0(cfg$results_folder,"/config.Rdata")
    cat("   Writing cfg to file",filename,"\n")
    save(cfg,file=filename)
    
    # Copy files required to configure and start a run
    filelist <- c("prepare_and_run.R" = "scripts/start/prepare_and_run.R",
                  ".Rprofile" = ".Rprofile")
    .copy.fromlist(filelist,cfg$results_folder)
    
    # Do not remove .RData files from REMIND main folder because they are needed in case you need to manually restart subsequent runs.
  }
  
  # Change to run folder
  setwd(cfg$results_folder)
  on.exit(setwd(cfg$remind_folder))
  
  # send prepare_and_run.R to cluster 
  cat("   Executing prepare_and_run.R for",cfg$results_folder,"\n")
  if(cfg$slurmConfig=="direct") {
    log <- format(Sys.time(), paste0(cfg$title,"-%Y-%H-%M-%S-%OS3.log"))
    system("Rscript prepare_and_run.R")
  } else {
    system(paste0("sbatch --job-name=",cfg$title," --output=log.txt --mail-type=END --comment=REMIND --wrap=\"Rscript prepare_and_run.R \" ",cfg$slurmConfig))
    Sys.sleep(1)
  }
    
  return(cfg$results_folder)
}
