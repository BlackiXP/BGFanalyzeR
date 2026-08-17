#' Constructor to set up a Bio Gas Fermentation object
#'
#' Builds a BGF object from input parameters.
#'
#' This function serves as a base constructor to create a new BGF object.
#' The constructor first checks if all required input variables have the correct data type, then a minimal BGF object is created thereof.
#' All BGFs object share the structure of such a minimal BGF object, e.g. a list with at least three elements:
#'
#' ExpParam: A list that stores information on the BGF object itselfe, as well as meta data all fermentaitons of the object have in common. For example, the input variales name, ProcessTemp, InocToSubRatio and MeasurementType are stored as elements in this list
#'
#' metaData: A matrix with one row for each fermentation of the BGF object. It is created from the input variables ReactorLayout and BlankLabel. The first column of that matrix provides the ReactorLayout of the corresponding fermentation/ row. The second holds the information whether this layout is the specified BlankLabel or not. The third column enables to classify a fermentation/ row as excluded. Excluded fermentations are not used for yield statistics and can be hidden in plots
#'
#' BioGasData: A matrix in which experimental data of each fermentation is stored. Upon object creation, all values are set NA. Initially, seven columns are created but further column can be added via dedicated methods or R base syntax.
#'
#' @param name A character vector specifying the name of the new BGF object
#' @param ProcessTemp The process temperature of the fermentation(s) to be stored in the BGF object
#' @param InocToSubRatio The inoculum to substrate ratio (= inoculation strength) of the fermentation(s) to be stored in the BGF object
#' @param ReactorLayout A character vector providing the reactor layout, e.g. the grouping factor used for plotting and yield calculation of fermetation(s) in a BGF object
#' @param BlankLabel A character string indicating which group in ReactorLayout is the inoculum used for net gas/ yield calculation
#' @param MeasurementType An optional character string indicating the measuremet type of the BGF object to be created
#'
#'
#' @returns An R object of class BGF
#'
#' @export


# Constructor for a class "BGF" object ####
new_BGF <- function(name,
                               ProcessTemp,InocToSubRatio,
                               ReactorLayout,BlankLabel,MeasurementType=NA){
  stopifnot(is.character(name)) # check if 'name' is a character
  stopifnot(is.character(ReactorLayout)) # check if 'ReactorLayout' is a character
  stopifnot(is.character(BlankLabel)) # check if 'BlankLabel' is a character
  stopifnot(is.numeric(InocToSubRatio)) # check if 'InocToSubRatio' is numeric


  data<-structure(list( # build the empty object
    # 'ExpParam' is a list to store experimental data
    ExpParam=list("name"=name,"InocToSubRatio"=InocToSubRatio,"ProcessTemp"=ProcessTemp,"MeasurementType"=MeasurementType),
    # 'metaData' is a dataframe to store experimental (reactor specific) meta data
    metaData=data.frame(Layout=factor(ReactorLayout,levels=unique(ReactorLayout)),
                           Blank=(ReactorLayout%in%BlankLabel),
                           Excluded=F,
                           row.names = paste0("R",c(1:length(ReactorLayout)))),
    # 'BioGasData' to store any kind of gas measurement related data
    BioGasData=data.frame(matrix(nrow = length(ReactorLayout),ncol = 7,dimnames = list(c(1:length(ReactorLayout)),c("reactor","time","product","production","net_product","yield","rel_production"))))#,
    # 'BMP_data' to store the final output e.g. BMP's/ max flow per layout
    # BMP_data=data.frame(matrix(nrow = length(unique(ReactorLayout)),ncol = 6,dimnames = list(unique(ReactorLayout),c("bmp","sd_bmp","max_flow","sd_max_flow","time_max_flow","sd_time_max_flow"))))
    ))

  class(data)<-"BGF"

  return(data)
}






