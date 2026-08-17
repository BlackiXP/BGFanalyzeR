#' Validation of data integrity
#'
#' A set of two functions, that allow to validate and re-establish data integrity of a `BGF`.
#'
#' The function `validate_BGF` can be used to check whether an object is a `BGF` or not.
#' It will raise an error if an object does not fulfill the criteria of a `BGF`.
#'
#' @param x a `BGF`
#'
#' @returns a `BGF`
#'
#' @examples
#' # create an example BGF
#' myBGF <- new_BGF("myBGF",52,2,LETTERS[1:15],"A","manuel")
#'
#' # check if 'myBGF' fulfills the criteria of being a BGF
#' validate_BGF(myBGF)
#'
#' @export
#'

# Validator for a class "BGF" object ####
validate_BGF <- function(x){
  stopifnot(is.list(x$ExpParam))
  stopifnot(is.data.frame(x$metaData))
  stopifnot(is.data.frame(x$BioGasData))


 #  RL <- as.character(get_ReactorLayout(x)) # extracts 'ReactorLayout'
 # # BL <- as.character(get_BlankLabel(x)) # extracts 'BlankLabel'
 #  MT <- get_MeasurementType(x) # extracts 'MeasurmentType'

  # if(isFALSE(BL%in%RL)){ # check if 'BlankLabel' is part of 'ReactorLayout'
  #   stop("'BlankLabel' argument not found in 'ReactorLayout' specifications!",
  #        call. = FALSE)
  # }

  # if(!is.na(get_MeasurementType(x))){if(isFALSE(MT%in%c(NA,"manual","AMPTSV2"))){ # check if 'MeasurmentType' was specified correctly!
  #   stop("'MeasurementType' must either be 'NA', 'manual' or 'AMPTSV2'!", # if not stop function and raise an error
  #        call. = FALSE)
  # }}

  return(x)
}

#' @rdname validate_BGF
#'
#' @details
#' The function `update_BGF` checks and updates several parameters, so that the internal logic of a `BGF` is consistent.
#' First of all, it checks if all all fermentations in the `metaData`-layer occur in the `BioGasData`-layer of a `BGF` and vice versa.
#' It than adds a row to the `metaData`-layer, if it detects any fermentation in the `BioGasData`-layer, that is not yet represented in the `metaData`layer.
#'
#' In the opposite case, a fermentation has an entry in the `metaData` but not in the `BioGasData`-layer, the user can decide whether this fermentation should be removed from the `metaData`-layer or not.
#'
#' The function will also remove rows that have `NA`in the 'reactor' column from the `BioGasData`-layer, sort observations in that layer by 'reactor' and 'time' columns and finally generates new valid row names representing the new row number of an observation in the reordered `BioGasData`-layer.
#'
#' @param delete_missing_BGD `logic`; defaults to `FALSE`. If `TRUE` fermentations/ rows in the `metaData`-layer with no data in the `BioGasData`-layer will be removed from the `metaData`-layer.
#'
#' @examples
#' # print myBGF
#' myBGF
#'
#' # updating the BGF will remove observations from `BioGasData`-layer
#' myBGF <- update_BGF(myBGF)
#'
#' # print myBGF again
#' myBGF
#'
#' @export
#'


# update_BGF() ####
update_BGF=function(x,delete_missing_BGD=FALSE){

  rn_metaData <- row.names(x$metaData) # get rownames of metaData

  BGD_empty <- grep(T,is.na(x$BioGasData$reactor)) # get position of all NA's in reactor column of BioGasData
  BGD_levels <- levels(x$BioGasData$reactor) # get the levels of the reactor column in BioGasData

  while (length(BGD_empty)>0) { # remove lines where 'reactor' is NA from BioGasData
    x$BioGasData<-x$BioGasData[-BGD_empty[1],]
    BGD_empty <- grep(T,is.na(x$BioGasData$reactor)) # get position of all remaining NA's in reactor column of BioGasData
  }

  if(isFALSE(all(rn_metaData%in%BGD_levels)) | isFALSE(all(BGD_levels%in%rn_metaData))){ # check if all elements of rn_metaData are present in BGD_levels and vice versa
    missing_metaData<-subset(BGD_levels,!BGD_levels%in%rn_metaData) # if not, find the missing metaData rows
    x$metaData[missing_metaData,]=NA # add missing rows with an NA in each cell added

    for(i in c("Layout","Blank","Excluded")){ # then eliminate NA's in the specified standard metaData columns of a BGF
      for(j in c(1:nrow(x$metaData))){ # check for each row of metaData ...

      if(is.na(x$metaData[{{j}},{{i}}])==TRUE){ # ... if column 'i' has an NA at row 'j' (= cond 1)
        if({{i}}=="Layout"){ # if cond 1 is TRUE, check if 'i' is 'Layout' ... (= cond 2)
          x$metaData[,{{i}}]=as.character(x$metaData[,{{i}}]) # ... if cond 2 is also TRUE, ...
          x$metaData[{{j}},{{i}}]=rownames(x$metaData)[j] # ...  generate a new value for 'Layout'
          x$metaData[,{{i}}]=as.factor(x$metaData[,{{i}}])
        }else{
          x$metaData[{{j}},{{i}}]=FALSE # if cond 2 is FALSE, correct the NA at this position to the boolean FALSE
        }

      }

      }}

    if(delete_missing_BGD==T){ # check if rows in metaData with no associated data in BioGasGata should be removed
      rn_metaData <- row.names(x$metaData) # get rownames of metaData again
      BGD_levels <- levels(x$BioGasData$reactor) # get the levels of the reactor column in BioGasData again

      missing_BGD<-subset(rn_metaData,!rn_metaData%in%BGD_levels) # get which rows in metaData are missing in BioGasData' reactor column
      remove<-match(missing_BGD,rownames(x$metaData)) # get their position in metaData
      if(length(remove)>0){ # if at least 1 exists
        x$metaData<-x$metaData[-c(remove),] # remove it
        }
    }
  }

  x$BioGasData<-dplyr::distinct(x$BioGasData) # keep only distinct observations in BioGasData

  x$BioGasData$reactor=factor(x$BioGasData$reactor,levels=row.names(x$metaData)) # re-establish link between 'BioGasData' and 'metaData'

  x<-cols_to_numeric(x,vec = c(2:7))
  
  x$BioGasData<-x$BioGasData[order(x$BioGasData$reactor,x$BioGasData$time),] # order 'x$BioGasData' by '$reactor' and 'time'

  if(nrow(x$BioGasData)>0) rownames(x$BioGasData)<-c(1:nrow(x$BioGasData)) # generate valid row names for x$BioGasData

  x$metaData$Layout <- factor(x$metaData$Layout,levels = unique(x$metaData$Layout))

  return(x) # return the updated BGF
}
