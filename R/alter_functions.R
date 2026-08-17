#' Alter data within a `BGF`
#'
#' A set of functions that allow to alter data in each layer of a `BGF`.
#'
#' The two functions `alter_whatever` and `alter_BG_measurement` can be used to change information in each layer of a `BGF`.
#' The function `alter_whatever` can be used to change data in the `ExpParam` or `metaData` layer.
#'
#' @inheritParams add_whatever
#' @param value,measurement either a `character`, `numeric` or `logic`. The desired new value
#' @param ID defaults to `NULL`, for `alter_whatever` can be a `character` or `integer` referring to row names in the `metaData` layer is. For `alter_BG_measurement` it can be an `integer` referring to a row in the `BioGasData` layer of a `BGF`
#'
#' @returns a `BGF`
#'
#' @examples
#' # create an example BGF
#' myBGF <- from_AMPTSV2_report(
#'         ReactorLayout = c("2*Blank","Cellulose","3*neg ctrl","3*FR1","3*FR2","3*FR3"),
#'         BlankLabel = "Blank",
#'         name = "Test",
#'         InocToSubRatio=2,
#'         ProcessTemp = 52,
#'         path = base::system.file("extdata","AMPTSV2.csv",package = "BGFanalyzeR")
#'         )
#'
#' # change name to 'newName'
#' myBGF <- alter_whatever(myBGF,"ExpParam","name","newName",feedback=TRUE)
#'
#' # exclude reactor 5,6 and 7
#' myBGF <- alter_whatever(myBGF,"metaData","Excluded",TRUE,c("R5","R6","R7"),feedback=TRUE)
#'
#' # exclude all reactors
#' myBGF <- alter_whatever(myBGF,"metaData","Excluded",TRUE)
#'
#' @export

# alter_whatever() ####
alter_whatever=function(x,layer,what,value,ID=NULL,feedback=F){
  if(isFALSE(class(x)=="BGF")){ # check if 'x' is class BGF
    stop("'x' must be class 'BGF'!",
         call. = FALSE)
  }

  if(isFALSE(is.character(layer))){ # check if 'layer'  is a  character
    stop("'layer' must be a character!",
         call. = FALSE)
  }

  if(isFALSE(is.character(what))){ # check if 'what' is a character
    stop("'what' must be a character!",
         call. = FALSE)
  }

  opts<-names(x) # get names of 'x'

  if(isFALSE(layer%in%opts)){ # check if 'layer' is among the names of 'x'
    sol=NULL # an empty vector to store possible solutions
    for (i in opts) {
      sol=paste0(sol,i,"\n") # fill the names of of 'x' in 'sol' and add a linebreak
    }
    stop(paste0("'layer' must be one of:","\n",sol), # end the function call with an error message
         call. = FALSE)
  }

  opts<-names(x[[{layer}]]) # get names of 'x'

  if(isFALSE(what%in%opts)){ # check if 'what' is among the names of 'x[[layer]]'
    sol=NULL # an empty vector to store possible solutions
    for (i in opts) {
      sol=paste0(sol,i,"\n")# fill the names of of 'layer' in 'sol' and add a linebreak
    }
    stop(paste0("'what' must be one of:","\n",sol), # end the function call with an error message
         call. = FALSE)
  }

  if(layer=="ExpParam")x[[{layer}]][[what]]<-value # alter entry 'what' of x$ExpParam
  if(layer=="metaData"){
    if(is.null(ID)==T)x[[{layer}]][,what]<-value # alter vector 'what' of x$metaData or
    if(is.null(ID)==F)x[[{layer}]][ID,what]<-value} # alter cell x$metaData[ID,what]

  return(x) # return  modified x

}

#' @rdname alter_whatever
#'
#' @param reactor_id a `character` identifying a the reactor/ fermentation whose entry in `BioGasData` needs to be changed
#' @param time_id a `numeric` specifying the time that has passed since fermentation start for the choosen reactor
#' @param col a `character` specifying the name of the column of the `BioGasData` layer that needs to be changed
#'
#' @details
#' The function `alter_BG_measurement` changes a single entry in the `BioGasData` layer of the `BGF`.
#' This entry can be selected by providing the name of the reactor for which a measurement should be changed, together with the time after fermentation start when the measurement was acquired.
#' Alternatively, the row number in the `BioGasData` layer of that measurement can be provided to 'ID'.
#'
#' @examples
#' # change the 'product' column of reactor 1 at time 0 to 7777
#' myBGF <- alter_BG_measurement(myBGF,"R1",0,"production",7777)
#'
#' #  do the same for reactor 2 but index via 'ID'
#' myBGF <- alter_BG_measurement(myBGF,ID=2,col="production",measurement=7777)
#'
#' @export
#'


# alter_BG_measurement() ####
alter_BG_measurement=function(x,reactor_id,time_id,col,measurement,ID=NULL,feedback=F){
  if(isFALSE(class(x)=="BGF")){ # check if 'x' is class BGF
    stop("'x' must be class 'BGF'!",
         call. = FALSE)
  }

  if(isFALSE(col%in%names(x[["BioGasData"]]))){ # check if 'col' is among the names of x$BioGasData
    sol=NULL # create an empty vector for possible solutions for 'col'
    for (i in names(x[["BioGasData"]])) {
      sol=paste0(sol,i,"\n") # copy the names of x$BioGasData into 'sol' and add a linebreak
    }
    stop(paste0("'col' must be one of:","\n",sol), # end function and raise an error
         call. = FALSE)
  }

  if(isTRUE(is.null(ID))){ # if no 'ID' was specified
    ID_found=rownames(subset(x[["BioGasData"]],x[["BioGasData"]]$reactor=={{reactor_id}}&x[["BioGasData"]]$time=={{time_id}})) # search for the correct ID
    x[["BioGasData"]][{{ID_found}},{{col}}]=measurement # then alter the specified measurement value

  }else{
    x[["BioGasData"]][{{ID}},{{col}}]=measurement # or directly alter the specified measurement value if 'ID' was initially provided
  }

  return(x) # return modified x

}

