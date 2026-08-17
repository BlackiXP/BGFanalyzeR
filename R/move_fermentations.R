#' Reorientate fermentations of a `BGF` by time
#'
#' The function allows to remove data points of individual or all fermetations based on the fermetation/ observation time ('time' column in `BiGasData` layer of a `BGF`).
#' The user can either remove data points later than a specified 'time' value, or substract a specified value from each value in the 'time' column and afterwards optionally remove the resulting times < 0.
#'
#' @param x a `BGF`
#' @param value a `numeric`; either the time before or after which data points should be removed (or shifted)
#' @param mode a `character`. Can be 'all' to apply the trimming to all fermentations of a `BGF`. Otherwise each element in `mode` must refer to a fermentation label (e.g. 'mode = c("R1","R3","R4","R15")' to trim fermentation R1, R3, R4 and R15)
#' @param left_end `logic`; default `TRUE`. Should the fermentation/ observation time be shifted towards the left end? If `FALSE` data points later than the selected 'value' will be removed
#' @param cut_left `logic`; default `TRUE`. Should data points with a negative fermentation/ observation times be removed?
#'
#' @returns a `BGF`
#'
#' @examples
#' # create example BGF
#' myBGF <- from_AMPTSV2_report(
#'         ReactorLayout = c("2*Meso","Cellulose","2*S1 ctrl","2*S1 7d","2*S1 4d",
#'                           "2*S2 ctrl","2*S2 4d","2*S2 6d"),
#'         BlankLabel = "Meso",
#'         name = "myBGF",
#'         ProcessTemp = 42,
#'         path = base::system.file("extdata","AMPTSV2.csv",package = "BGFanalyzeR"),
#'         InocToSubRatio = 2,
#'         feedback = TRUE)
#'
#' # BioGasData has 735 rows
#' nrow(myBGF$BioGasData)
#'
#' # remove all data before day 30
#' myBGF <- trim_FR_time(myBGF,value= 30)
#'
#' # BioGasData has now only 285 rows
#' nrow(myBGF$BioGasData)
#'
#' # remove all data after day 15
#' myBGF <- trim_FR_time(myBGF,value= 15,left_end=FALSE,cut_left=FALSE)
#'
#' # BioGasData has now only 225 rows
#' nrow(myBGF$BioGasData)
#'
#' @export
#'


# trim_FR_time() ####
trim_FR_time=function(x,value,mode="all",left_end=TRUE,cut_left=TRUE){
  if(isFALSE(class(x)=="BGF")){ # check if 'x' is class basic_BGF
    stop("'x' must be class 'BGF'!",
         call. = FALSE)}

  if(length(mode)==1) {if(mode=="all") lvls=levels(x$BioGasData$reactor) else lvls=mode}else{lvls=mode}

  newData<-x$BioGasData[,c("reactor","time")]

  pos=list()

  for(i in lvls){
    pos[[{{i}}]]=as.numeric(rownames(subset(newData,newData$reactor=={{i}})))
  }


  if(left_end==TRUE){

    for(i in names(pos)){

      times=subset(newData,newData=={{i}})


      for(j in c(1:nrow(times))){

        x$BioGasData[pos[[i]][j],"time"]=times$time[j]-value

      }}

    if(cut_left==TRUE) x$BioGasData<-subset(x$BioGasData,x$BioGasData$time>=0)

  }else{

    for(i in c(1:nrow(x$BioGasData))){

      if(x$BioGasData$reactor[i]%in%lvls){
        if(x$BioGasData$time[i]>=value){
          x$BioGasData$reactor[i]=NA
        }}

    }

    x$BioGasData<-subset(x$BioGasData,is.na(x$BioGasData$reactor)==F)

  }

  return(x) # return modified list

}
