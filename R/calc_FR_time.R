#' Calculate Fermentation Time
#'
#' Calculates the fermentation time for a minimal biogas fermentation data set
#'
#'
#' @param standardReport a data.frame with at least one column representing a series of time stemps (%y-%m-%d %H:%M:%S)
#' @param time_col numeric. indicates which column of standardReport is used to calculate the fermentation time
#' @param ... further arguments passed to `difftime`
#'
#' @returns A data.frame with an additional `$time` column
#'
#'@examples
#'# import example biogas fermentation data
#'stRep<-import_standard_record(
#'       ipath = base::system.file("extdata","Fermentation_B.tsv",package = "BGFanalyzeR"),
#'       header=TRUE,
#'       dec=".",
#'       sep="\t")
#'
#'# calculating the fermentation time adds a new column ($time) to the input data.frame
#'stRep_FRT<-calc_FR_time(stRep,1,units="hours")
#'
#' @export
#'


# calc_FR_time() ####
calc_FR_time=function(standardReport,time_col,...){
  standardReport$time=NA

  for (i in c(1:nrow(standardReport))){
    standardReport$time[i]=difftime(standardReport[i,time_col],standardReport[1,time_col],...)

  }

  return(standardReport)
}
