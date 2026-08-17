#' Add data to a `BGF`
#'
#' A set of functions to add data to each layer of a `BGF`.
#'
#' There are five functions that can be useful to add data to `BGF`.
#' The first is `add_whatever`, which allows to add data either to the `ExpParam` or the `metaData` layer of a `BGF`.
#' The functions `add_ExpParam` and `add_metaData` are wrapper functions that internally call `add_whatever` with the 'layer'
#'
#' @inheritParams get_layer
#' @param layer a `character` referring a layer of a `BGF`; either 'ExpParam' or 'metaData'
#' @param what a `character` referring either to a list entry of the `ExpParam` layer, or a column of the `metaData` layer of a `BGF`
#' @param lab a `character` providing the name for the new data
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
#' # add a start date to 'ExpParam'
#' myBGF <- add_whatever(myBGF,"ExpParam",what = c("Start date"="2026-05-04 12:00:00"))
#'
#' # add organic total solutes measurements to 'metaData'
#' myBGF <- add_whatever(
#'         x = myBGF,
#'         layer = "metaData",
#'         what = c("oTS"=c(2.3,2.3,98.8,4.5,4.4,4.6,3.5,3.6,3.5,3.8,3.9,4,3.5,3.6,3.8))
#'         )
#'
#' @export
#'

# add_whatever() ####
add_whatever=function(x,layer,what,lab="newData",feedback=F){
  if(isFALSE(class(x)=="BGF")){ # check if 'x' is class BGF
    stop("'x' must be class 'BGF'!",
         call. = FALSE)
  }

  if(isFALSE(is.character(layer))){ # check if 'layer' is a character
    stop("'layer' must be a character!",
         call. = FALSE)
  }

  # if(isFALSE(is.character(what))){ # check if 'what' is a character
  #   stop("'what' must be a character!",
  #        call. = FALSE)
  # }

  opts<-names(x) # get names of 'x'

  if(isFALSE(layer%in%opts)){ # check if 'layer' is among the names of 'x'
    sol=NULL # an empty vector to store possible solutions
    for (i in opts) {
      sol=paste0(sol,i,"\n") # fill the names of of 'x' in 'sol' and add a linebreak
    }
    stop(paste0("'layer' must be one of:","\n",sol), # end the function call with an error message
         call. = FALSE)
  }

  if(layer=="ExpParam")x[[layer]]<-c(x[[layer]],what) # append the list if 'layer' is "ExpParam"
  if(layer=="metaData")x[[layer]][,lab]=what # add a new column if 'layer' is "metaData"

  # give feedback
  if(feedback==T){
    print(paste0(x[["ExpParam"]][["name"]],":",layer," was modified...",quote=F))
    print(what)
    print("... was added!",quote=F)
  }

  validate_BGF(x) # check integrity of x

  return(x) # return modified x

}

#' @rdname add_whatever
#' @details For `add_ExpParam` argument 'what' is expected to be a named vector of length 1.
#' The name of the vector will be kept as label of a list entry in the `ExpParam` layer and the vectors value will be the value of that list entry.
#'
#' @examples
#' # add a end date to 'ExpParam'
#' myBGF<-add_ExpParam(x = myBGF,what =  c("End date"="2026-06-04 12:00:00"))
#'
#' @export
#'


## add_ExpParam() ####
add_ExpParam=function(x,what,feedback=F){
  x<-add_whatever(x = x,layer = "ExpParam",what = what,feedback = feedback) # apply 'add_whatever()' with 'layer="ExpParam"'

  validate_BGF(x) # check integrity of x

  return(x) # return modified x
}

#' @rdname add_whatever
#' @details For `add_metaData` argument 'what' is expected vector of the same length as number of rows in the `metaData` layer.
#' It will copy that vector to the `metaData` layer as a new column.
#' That column can be named using the 'lab' argument.
#' Otherwise a standard name is created.
#' The function checks if the chosen name already exists in `metaData` and will eventually generate a new name.
#'
#' @examples
#' # add total solutes measurement to 'metaData'
#' myBGF<-add_metaData(
#'         x = myBGF,
#'         what =  c(3.3,3.3,99.8,6.5,6.6,6.6,6.5,6.6,6.5,6.8,6.9,6,6.5,6.6,6.8),
#'         lab="TS")
#'
#' @export
#'

## add_metaData() ####
add_metaData=function(x,what,makeCol=TRUE,lab="newData",feedback=F){
  if(isFALSE(class(x)=="BGF")){ # check if 'x' is class BGF
    stop("'x' must be class 'BGF'!",
         call. = FALSE)
  }

  evaluName <- length(grep(lab,names(x[["metaData"]]))) # evaluate if 'lab' is already part of names(x$metaData)

  #if(evaluName>0) tmp_name <- paste(lab,evaluName+1) else tmp_name=lab # generate a name for the new metaData column

  if(isTRUE(makeCol)){if(evaluName>0) tmp_name <- paste(lab,evaluName+1) else tmp_name=lab # generate a name for the new metaData column
  }else{tmp_name=lab}
  #x<-add_whatever(x = x,layer = "metaData",what = what,feedback = feedback) # apply 'add_whatever()' with 'layer="metaData" pre set
  x<-add_whatever(x = x,layer = "metaData",what = what,lab = tmp_name,feedback = feedback) # apply 'add_whatever()' with 'layer="metaData" pre set

   #names(x[["metaData"]])[length(names(x[["metaData"]]))]<-tmp_name # adjust the name for the new data

  validate_BGF(x) # check integrity of x

  return(x) # return modified x

}

#' @rdname add_whatever
#' @details The function `add_BG_measurement` adds a single measurement value to an existing column of the `BioGasData` layer.
#' To  this end the user must specify to which reactor of the `BGF` the measurement belongs to (argument 'reactor') and at which time the measurement was taken (argument 'time').
#' The argument 'col' is the name or position of the column in the `BioGasData` layer to which the measurement should be added.
#'
#' @param reactor a `character` referring to a reactor/ fermentation of a `BGF`; in case of `add_BG_parameter` must have the same length as 'parameter'. If 'parameter' is a `data.frame` 'reactor' must have length 1 and specifies the reactor the measurements belong to
#' @param time  for `add_BG_measurement`, a `numeric` specifying the time that has passed since fermentation start; for `add_BG_parameter` either a `integer` indicating the position of the 'time' column in argument 'parameter' or a `numeric` of the same length as argument 'parameter' specifying the time that has passed since fermentation start (see Details)
#' @param col either a `character` or `numeric` referring to an existing column of the `BioGasData` layer of a `BGF`
#' @param measurement a `numeric`; measurement value to be added to the `BioGasData` layer of a `BGF`
#'
#' @examples
#' # add a new 'product' gas measurement for reactor 'R1' at time '49' (days) after fermentation start
#' myBGF<-add_BG_measurement(x = myBGF,reactor = "R1",time = 49,col = "product",measurement = 7777)
#'
#' @export
#'

# add_BG_measurement() ####
add_BG_measurement=function(x,reactor,time,col,measurement,feedback=F){
  if(isFALSE(class(x)=="BGF")){ # check if 'x' is class BGF
    stop("'x' must be class 'BGF'!",
         call. = FALSE)
  }
  # add a new measurement to x$BioGasData
  x[["BioGasData"]][nrow(x[["BioGasData"]])+1,c("reactor","time",{{col}})]=c(reactor,time,measurement)

  for(i in nrow(x[["BioGasData"]])){ # for each row in x$BioGasData
    while(is.na(x[["BioGasData"]][1,1])==T){ # check if the first cell is NA
      x[["BioGasData"]]<-x[["BioGasData"]][-1,] # if true this row is removed
      rownames(x[["BioGasData"]])=c(1:nrow(x[["BioGasData"]]))}} # rownames are restored

  return(x) # return modified x

}

#' @rdname add_whatever
#' @details The function `add_BG_parameter` allows to add a new data column to the `BioGasData` layer of a `BGF`.
#' It is very useful if not all measurements for a fermentation can be imported from the same standard record, e.g. if exhaust gas volumes are measured by one device, while the gas composition is registered by another.
#'
#'
#' @param parameter either a `data.frame` with at least two columns or a `numeric` carrying the data to be added
#' @param value defaults is `NULL`; must be an `integer` referring to the measurement data column in 'parameter' if 'parameter' is a `data.frame`
#' @param name defaults is `NULL`; can be a `character` specifying the label of the new column being created in the `BioGasData` layer of a `BGF```
#' @param makeCol `logic`, default is `TRUE`. Should a new column be created? If `FALSE` the data will be added to an existing column named `name`
#' @param cut_zero `logic`, default is `TRUE`. If `FALSE` measurements with a negative time (measurements that were acquired before the fermentation started) will NOT be removed from `BioGasData`
#' @param interpolate_missing `logic`, default is `TRUE`. Indicating if missing values between to measurements should be interpolated
#' @param default_start a `numeric`, a default measurement value that is assumed in the begin of the fermentation for the parameter to be added
#'
#' @examples
#' # create a second example BGF from a standard record with exhaust gas data
#' myBGF2<-from_standard_record(
#'         ReactorLayout = "A",
#'         ProcessTemp = 80,
#'         InocToSubRatio = .1,
#'         path = base::system.file("extdata","Fermentation_A.tsv",package = "BGFanalyzeR"),
#'         time_col = 1,
#'         product_col = 3)
#'
#' # import another standard record that provides additional information on the 'BioGasData' layer
#' Gas_comp<-import_standard_record(
#'         ipath = base::system.file(
#'             "extdata",
#'             "gasq_A.tsv",
#'             package = "BGFanalyzeR"
#'          ),
#'         mkFRTime = "2025-01-15 17:00:00",
#'         FRTime_col = 1,
#'         units = "hours")
#'
#' # add the gas composition data to the BGF and interpolate missing values
#' myBGF2<-add_BG_parameter(
#'         x = myBGF2,
#'         parameter = Gas_comp,
#'         reactor = "R1",
#'         time = 3,
#'         value = 2,
#'         name = "H2",
#'         cut_zero = TRUE,
#'         interpolate_missing = TRUE)
#'
#' @export
#'

# add_BG_parameter() ####
add_BG_parameter=function(x,parameter,reactor,time=NULL,value=NULL,name=NULL,makeCol=TRUE,cut_zero=TRUE,interpolate_missing=TRUE,default_start=0){
  if(isFALSE(class({{x}})=="BGF")){ # check if 'x' is class BGF
    stop("'x' must be class 'BGF'!",
         call. = FALSE)
  }

  if(isTRUE(is.data.frame({{parameter}}))){ # check if 'parameter' is a data frame

    if(isTRUE(is.null({{time}}))){
      stop("If 'parameter' is a data frame 'time' must be a numeric indicating the position of the fermentation time column of 'parameter'!",
           call. = FALSE)
    }

    if(isTRUE(is.null({{value}}))){
      stop("If 'parameter' is a data frame 'value' must be a numeric indicating the position of the value column of 'parameter'!",
             call. = FALSE)
    }

    stopifnot(is.numeric({{time}})) # check if 'time' is a numeric
    stopifnot(is.numeric({{value}})) # check if 'value' is a numeric

    if(isTRUE(is.character({{reactor}}))){
      reactor<-rep({{reactor}},nrow({{parameter}}))
    }

    if(isTRUE(is.numeric({{reactor}}))){
      reactor=parameter[,{{reactor}}]
    }

    positions<-as.numeric(unique(stats::na.omit(match(x$BioGasData$time,parameter[,{{time}}])))) # check if values of 'time' match  values in 'x$BioGasData$time'

    time=parameter[,{{time}}] # overwrite 'time' with 'parameter[,time]'
    value=parameter[,{{value}}] # overwrite 'value' with 'parameter[,value]'
    #value=value[-which(time==x$BioGasData$time[positions])]
    #time=time[-which(time==x$BioGasData$time[positions])]




  }else{ # otherwise treat parameter as vector
    if(isFALSE(length(parameter)==length(time))){
      stop("If 'parameter' is a vector, 'time' must be a vector of the same length!",
           call. = FALSE)
    }

    if(isFALSE(length(parameter)==length(reactor))){
      stop("If 'parameter' is a vector, 'reactor' must be a vector of the same length!",
           call. = FALSE)
    }

    positions<-as.numeric(unique(stats::na.omit(match(x$BioGasData$time,time))))

    value=parameter

  }

  if(isTRUE(is.null(name))){ # check if a name for the new parameter was specified and ...
    name="new_Param" # ... generate one if not

  }

  if(isTRUE(makeCol)){
  i_name=0 # loop counter for while loop

  while (isTRUE(name%in%names(x$BioGasData))) { # as long as 'name' is in 'names(x$BioGasData)'
    i_name=i_name+1 # increase loop counter
    name<-gsub("_[0-9]","",name) # cut the name ending if matches to loop associated pattern
    name<-paste0(name,"_",i_name+1) # create a new name
  }

  x$BioGasData[,{{name}}]=NA # create a new column in ' x$BioGasData' named 'name'
  }

  if(length(positions)>0){ # if 'length(positions)' is greater 0
    for(i_pos in positions){ # copy the respective entry in 'value' for each element in 'positions' to 'x$BioGasData[{{i_pos}},{{name}}]'
      x$BioGasData[{{i_pos}},{{name}}]=value[{{i_pos}}]
    }

    # add the remaining 'reactor', 'time', 'value' pairs at the end of 'x$BioGasData'
    x$BioGasData[c(nrow(x$BioGasData):nrow(x$BioGasData)+length(value[-positions])),]=NA
    x$BioGasData[c((nrow(x$BioGasData)-(length(value[-positions])-1)):nrow(x$BioGasData)),"reactor"]=reactor[-positions]
    x$BioGasData[c((nrow(x$BioGasData)-(length(value[-positions])-1)):nrow(x$BioGasData)),"time"]=time[-positions]
    x$BioGasData[c((nrow(x$BioGasData)-(length(value[-positions])-1)):nrow(x$BioGasData)),{{name}}]=value[-positions]

  }else{ # or if 'length(positions)' is NOT greater 0, just add the data at the end of 'x$BioGasData'

    x$BioGasData[c(nrow(x$BioGasData):nrow(x$BioGasData)+length(value)),]=NA
    x$BioGasData[c((nrow(x$BioGasData)-(length(value)-1)):nrow(x$BioGasData)),"reactor"]=reactor
    x$BioGasData[c((nrow(x$BioGasData)-(length(value)-1)):nrow(x$BioGasData)),"time"]=time
    x$BioGasData[c((nrow(x$BioGasData)-(length(value)-1)):nrow(x$BioGasData)),{{name}}]=value

  }

  x$BioGasData<-dplyr::arrange(x$BioGasData,time) # sort the data by time

  x<-update_BGF(x) # update_BGF() to ensure object integrity

  if(isTRUE(cut_zero)){ # if cut_zero is true, exclude entries with 'time' < 0 from 'x$BioGasData'
    x$BioGasData<-subset(x$BioGasData,x$BioGasData$time>=0)
  }

  if(isTRUE(is.na(x$BioGasData[which(x$BioGasData$reactor==unique({{reactor}}))[1],{{name}}]))){ # correct the first entry in 'x$BioGasData[1,{{name}}]' to 'default_start' if it is 'NA'
    x$BioGasData[which(x$BioGasData$reactor==unique({{reactor}}))[1],{{name}}]=default_start
  }

  if(isTRUE(interpolate_missing)){
    # if(isTRUE(is.na(subset(x$BioGasData,x$BioGasData$reactor=={{reactor}})[1,{{name}}]))){
    #   el_id <- which(x$BioGasData$reactor==unique({{reactor}}))[1]
    #   x$BioGasData[el_id,{{name}}] <- 0
    # }
    x$BioGasData[,{{name}}]<-zoo::na.approx(x$BioGasData[,{{name}}],na.rm=F)
    x$BioGasData[,{{name}}]<-zoo::na.locf(x$BioGasData[,{{name}}])
  }

  x<-update_BGF(x) # update_BGF() to ensure final object integrity

  return(x)
}

