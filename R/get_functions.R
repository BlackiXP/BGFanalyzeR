#' Extract information from a BGF
#'
#' A set of functions that can be used to extract various information from a `BGF`.
#'
#' The functions `get_MeasurementType`, `get_ReactorLayout`, `get_BlankLabel`,`get_Excluded` and `get_blanks` are wrapper for `get_whatever` and extract frequently needed information from a `BGF`.
#'
#' The function `get_whatever` internally calls `get_layer` to extract any layer from a `BGF`, and subsequently return only a single `list` entry or a `data.frame` column.
#'
#' The function `get_yield_summary` returns a `data.frame` if a yield summary was generated for the `BGF` via `summarize_yield`.
#'
#' @param x a `BGF`
#' @param layer a layer of a `BGF`; either 'ExpParam', 'metaData' or 'BioGasData'
#' @inheritParams save_BGF
#'
#' @returns `get_layer` returns a either a `data.farme` or `list`
#'
#' @examples
#' # create an example BGF
#' myBGF<-BGF(LETTERS[1:5],"A","myBGF",52,2,"manuel")
#'
#' # extract 'ExpParam' layer
#' ExpParam <-get_layer(myBGF,"ExpParam",TRUE)
#'
#' # extract 'metaData' layer
#' metaData <-get_layer(myBGF,"metaData",TRUE)
#'
#' # extract 'BioGasData' layer
#' BioGasData <-get_layer(myBGF,"BioGasData",TRUE)
#'
#' @export
#'


# get_layer() ####
get_layer=function(x,layer,feedback=F){
  if(isFALSE(class(x)=="BGF")){ # check if 'x' is class basic_BGF
    stop("'x' must be class 'BGF'!",
         call. = FALSE)
  }

  if(isFALSE(is.character(layer))){ # check if 'layer' is a character
    stop("'layer' must be a character!",
         call. = FALSE)
  }

  opts<-names(as.list(x))
   # names(x) # get names of 'x'

  if(isFALSE(layer%in%opts)){ # check if 'layer' is among the names of 'x'
    sol=NULL # an empty vector to store possible solutions
    for (i in opts) {
      sol=paste0(sol,i,"\n") # fill the names of of 'x' in 'sol' and add a linebreak
    }
    stop(paste0("'layer' must be one of:","\n",sol), # end the function call with an error message
         call. = FALSE)
  }

  out <- x[[layer]] # extract 'layer'

  # give feedback
  if(feedback==T) print(paste0(layer," extracted from ",x[["ExpParam"]][["name"]],"!"),quote=F) # give feedback

  return(out) # return value of x[["var"]][[what]]

}

#' @rdname get_layer
#'
#' @param what a `character` referring either to a list entry of the `ExpParam` layer, or a column of the `metaData` or `BioGasData` layer of a `BGF`
#'
#' @returns `get_whatever` returns a either a `data.farme` column or `list` entry
#'
#' @examples
#' # extract 'name'-attribute
#' get_whatever(myBGF,"ExpParam","name")
#'
#' @export
#'


## get_whatever() ####
get_whatever=function(x,layer,what,feedback=F){
  if(isFALSE(class(x)=="BGF")){ # check if 'x' is class basic_BGF
    stop("'x' must be class 'BGF'!",
         call. = FALSE)
  }

  if(isFALSE(is.character(layer))){ # check if 'layer' is a character
    stop("'layer' must be a character!",
         call. = FALSE)
  }

  if(isFALSE(is.character(what))){ # check if 'what' is a character
    stop("'what' must be a character!",
         call. = FALSE)
  }

  opts<-names(as.list(x))
    #as.character(subset(names(x),names(names(x))!="BGF_NAME"))
    #names(x) # get names of 'x'

  if(isFALSE(layer%in%opts)){ # check if 'layer' is among the names of 'x'
    sol=NULL # an empty vector to store possible solutions
    for (i in opts) {
      sol=paste0(sol,i,"\n") # fill the names of of 'x' in 'sol' and add a linebreak
    }
    stop(paste0("'layer' must be one of:","\n",sol), # end the function call with an error message
         call. = FALSE)
  }

  var <- get_layer(x,layer,F) # extract 'layer'

  opts<-names(var) # get names of '$var'

  if(isFALSE(what%in%opts)){ # check if 'what' is among the names of '$var'
    sol=NULL # an empty vector to store possible solutions
    for (i in opts) {
    sol=paste0(sol,i,"\n") # fill the names of of 'x' in 'sol' and add a linebreak
    }
    stop(paste0("'what' must be one of:","\n",sol), # end the function call with an error message
         call. = FALSE)
  }

  out <- var[[what]] # extract the desired value of '$var' as a character

  # give feedback
  if(feedback==T) print(paste0(paste0(x[["ExpParam"]][["name"]],":",layer)," has '",what,"': ",out),quote=F) # give feedback

  return(out) # return value of x[["var"]][[what]]
}

#' @rdname get_layer
#'
#' @returns `get_MeasurementType` returns a `character`
#'
#' @examples
#' # extract 'MeasurmentType'-attribute
#' get_MeasurementType(myBGF)
#'
#' @export
#'

### get_MeasurementType() ####
get_MeasurementType=function(x,feedback = F){
  out <- get_whatever(x,"ExpParam","MeasurementType") # apply 'get_whatever' with 'layer="ExpParam"' and 'what="MeasurementType"' pre set

  # give feedback
  if(isTRUE(feedback)){print(paste(x[["ExpParam"]][["name"]],"has 'MeasurementType':"),quote=F)
    print(out)} # give feedback

  return(out) # return 'MeasurementType' of 'x'
}

#' @rdname get_layer
#'
#' @returns `get_ReactorLayout` returns a `factor`
#'
#' @examples
#' # extract the reactor layout
#' get_ReactorLayout(myBGF)
#'
#' @export
#'


### get_ReactorLayout() ####
get_ReactorLayout=function(x,feedback=F){
  out <- get_whatever(x,"metaData","Layout") # apply 'get_whatever' with 'layer="metaData"' and 'what="Layout"' pre set

  # give feedback
  if(feedback==T) print(paste(x[["ExpParam"]][["name"]],"has 'ReactorLayout':"),quote=F)

  return(out) # return 'ReactorLayout' as factor
}

#' @rdname get_layer
#'
#' @returns `get_BlankLabel` returns a `factor`
#'
#' @examples
#' # extract the Blank label
#' get_BlankLabel(myBGF)
#'
#' @export
#'


### get_BlankLabel() ####
get_BlankLabel=function(x,feedback=F){
  metaData <- get_layer(x,"metaData") # extract meta data

  out <- dplyr::pull(.data =  dplyr::filter(.data = metaData,.data$Blank==T),.data$Layout) # extract BlankLabel from metaData

  if(!length(out)>0){
    stop("'BlankLabel' argument not found in 'ReactorLayout' specifications!",
         call. = FALSE)
  }

  if(feedback==T){print(paste0(x[["ExpParam"]][["name"]]," has 'BlankLabel':"),quote=F)
    print(out)} # give feedback

  return(out) # return 'BlankLabel' as factor
}

#' @rdname get_layer
#'
#' @returns `get_Excluded` returns a `character`
#'
#' @examples
#' # Exclude reactor 1 and 5
#' myBGF$metaData$Excluded[c(1,5)] <- TRUE
#'
#' # extract the excluded reactors
#' get_Excluded(myBGF)
#'
#' @export
#'

### get_Excluded() ####
get_Excluded=function(x,feedback=F){
  out<-get_whatever(x,"metaData","Excluded",feedback = feedback) # apply 'get_whatever()' with 'layer="metaData"' and 'what="Excluded"' pre set

  out<-grep(T,out) # identify the position of excluded reactors

  out<-rownames(x[["metaData"]])[out] # generate a reactor number for each excluded reactor

  return(out) # return the reactor nr of excluded reactors
}

#' @rdname get_layer
#'
#' @returns `get_blanks` returns a `character`
#'
#' @examples
#' # extract names of blank reactors
#' get_blanks(myBGF)
#'
#' @export
#'

### get_blanks() ####
get_blanks=function(x,feedback=F){
  out<-get_whatever(x,"metaData","Blank",feedback = feedback) # apply 'get_whatever()' with 'layer="metaData"' and 'what="Blank"' pre set

  out<-grep(T,out) # identify the position of blank reactors

  out<-rownames(x[["metaData"]])[out] # generate a reactor number for each blank reactor

  return(out) # return the reactor nr of blank reactors
}

#' @rdname get_layer
#'
#' @param Excluded `logic` if `TRUE`, excluded reactors are not included in the summary. Default = `FALSE`
#'
#' @returns `get_yield_summary` returns a `data.frame`
#'
#' @examples
#' # create a second example BGF
#' myBGF2<-from_AMPTSV2_report(
#'         ReactorLayout = c("2*Blank","Cellulose","3*neg ctrl","3*FR1","3*FR2","3*FR3"),
#'         BlankLabel = "Blank",
#'         name = "myBGF2",
#'         InocToSubRatio = 2,
#'         ProcessTemp = 52,
#'         path = base::system.file("extdata","AMPTSV2.csv",package = "BGFanalyzeR"))
#'
#' # extract yield summary
#' get_yield_summary(myBGF2)
#'
#' @export
#'


# get_yield_summary() ####
get_yield_summary=function(x,Excluded=F,feedback=T){
  if(isFALSE(class(x)=="BGF")){ # check if 'x' is class basic_BGF
    stop("'x' must be class 'BGF'!",
         call. = FALSE)
  }

  out<-x$metaData[,c("Layout","yield","sd_yield","production","sd_production","time_production","sd_time_production","Excluded")]

  if(isFALSE(Excluded)) out<-subset(out,out$Excluded==F)

  out[,c(2:7)]<-as.data.frame(lapply(out[,c(2:7)], as.numeric))

  out<-dplyr::reframe(out,yield=mean(.data$yield),sd_yield=mean(.data$sd_yield),production=mean(.data$production),sd_production=mean(.data$sd_production),time_production=mean(.data$time_production),sd_time_production=mean(.data$sd_time_production),.by = .data$Layout)

  rownames(out)<-out[,1]

  out=out[,c(2:7)]


  if(isTRUE(feedback)) print(out)

  return(out)
}
