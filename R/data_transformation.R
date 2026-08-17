#' Data transformation functions
#'
#' A set of functions that allow to calculate and summarize biogas yield and production on the one hand, or allow to remove certain data points of a fermentation based on the fermentation/ observation time.
#'
#' The function `calc_yield` allows to calculates a biogas yield (volume per mass) based on the 'netGas' column of the `BioGasData` layer of a `BGF`.
#' In addition, a reference mass for each fermentation must be present in the `metaData` layer of the respective `BGF.`
#' If a fermentation is classified as 'Blank', this mass will not be used.
#'
#' @param x a `BGF`
#' @param pos a `integer` or `character` referencing the column with reference masses in the `metaData` layer
#' @param feedback `logic`; if `TRUE`, the function will print a feedback to the console
#'
#' @importFrom rlang .data
#'
#' @returns a `BGF`
#'
#' @examples
#' # create an example BGF
#' myBGF <- BGF(
#'         ReactorLayout = c("2*Meso","Cellulose","2*S1 ctrl","2*S1 7d","2*S1 4d",
#'                           "2*S2 ctrl","2*S2 4d","2*S2 6d"),
#'         BlankLabel = "Meso",
#'         name = "myBGF",
#'         ProcessTemp = 42,
#'         MeasurementType = "AMPTSV2")
#'
#' # add data to BGF
#' myBGF <- add_bmp_measurement(
#'         x = myBGF,
#'         path = base::system.file("extdata","AMPTSV2.csv",package = "BGFanalyzeR"))
#'
#' # correct data
#' myBGF <- cols_to_numeric(myBGF)
#' myBGF <- close_gaps(myBGF)
#'
#' # calculate netGas
#' myBGF <- netGas(myBGF)
#'
#' # print the BGF; the yield was not calculated yet
#' myBGF
#'
#' # calculate yield
#' myBGF <- calc_yield(myBGF)
#'
#' # summarize yield
#' myBGF <- summarize_yield(myBGF)
#'
#' # print the BGF again; now the yield was calculated
#' myBGF
#'
#' @export
#'

# calc_yield() ####
calc_yield=function(x,pos=6,feedback=F){
  if(isFALSE(class(x)=="BGF")){ # check if 'x' is class basic_BGF
    stop("'x' must be class 'BGF'!",
         call. = FALSE)
  }

  meta<-x[["metaData"]] # extract 'metaData'

  mSample<-as.numeric(dplyr::pull(meta,pos)) # extract the masses of samples within each reactor as speciied in 'pos' of 'metaData'
  names(mSample)<-rownames(meta) # name the masses by the reactor names


  for(i in c(1:length(x[["BioGasData"]]$net_product))){ # calculate the BMPs for each obs in 'BioGasData'
    if(as.numeric(mSample[x[["BioGasData"]]$reactor[i]])!=0){ # but first check  if the obs belongs to a sample or blank reactor
    x[["BioGasData"]]$yield[i]<-c(as.numeric(x[["BioGasData"]]$net_product[i])/mSample[x[["BioGasData"]]$reactor[i]]) # if it is a sample calculate the BMP based on the extracted sample mass
    }else{
      x[["BioGasData"]]$yield[i]<-c(as.numeric(x[["BioGasData"]]$net_product[i])/1) # if  it is a blank don't use extracted masses (avoids division by 0)
    }
  }

  # give feedback
  if(isTRUE(feedback)) print(paste0("Biocehmical methane potentials calculated (",x$ExpParam$name,")..."),quote=F)

  return(x) # return modified x

}

#' @rdname calc_yield
#'
#' @details
#' The function `summarize_yield` summarizes the 'production' and 'yield' column of a `BioGasData` layer at a chosen time.
#' The summary will be transferred to the `metaData` layer of the same `BGF`.
#' It contains the information of what was the mean yield, the may production and when did max production occur for each reactor layout.
#' If several fermentations share the same reactor layout standard deviations for the those three parameters are calculated as well.
#'
#' @param timep a `numeric`. The fermentation/ observation time at which to calculate the yield. Can be the `character` string 'auto' instead (default). In this case the final time is automatically chosen
#'
#' @export
#'

# summarize_yield() ####
summarize_yield=function(x,timep="auto",feedback=F){
  if(isFALSE(class(x)=="BGF")){ # check if 'x' is class basic_BGF
    stop("'x' must be class 'BGF'!",
         call. = FALSE)
  }

  if(timep=="auto"){
    timep=NULL
    for(q in levels(x$BioGasData$reactor)) timep[{{q}}]=subset(x$BioGasData,x$BioGasData$reactor=={{q}})$time[length(subset(x$BioGasData,x$BioGasData$reactor=={{q}})$time)] # if 'timep' is 'auto' the last time will be  extracted
  }

  BGP<-as.data.frame(matrix(nrow = nrow(x$metaData),ncol = 7,
                            dimnames = list("rownames"=rownames(x$metaData),
                                            "colnames"=c("Label","yield","sd_yield","production","sd_production","time_production","sd_time_production"))))


  for(i in rownames(BGP)){
    BGP[i,c("Label","yield","production","time_production")]=c(as.character(x$metaData[{{i}},"Layout"]),
                                                               as.numeric(subset(x$BioGasData,x$BioGasData$reactor=={{i}}&x$BioGasData$time==timep[{{i}}])$yield),
                                                               as.numeric(max(subset(x$BioGasData,x$BioGasData$reactor=={{i}})$production,na.rm=TRUE)),
                                                               as.numeric(subset(x$BioGasData,x$BioGasData$reactor=={{i}})$time[which(subset(x$BioGasData,x$BioGasData$reactor=={{i}})$production==max(subset(x$BioGasData,x$BioGasData$reactor=={{i}})$production,na.rm=TRUE))]))

  }

  BGP[c(2:7)] <- apply(BGP[c(2:7)],2,as.numeric)

  out <- dplyr::reframe(.data = dplyr::group_by(.data = BGP,.data$Label),
                        yield_n=mean(.data$yield,na.rm=TRUE),
                        sd_yield_n=stats::sd(.data$yield,na.rm=TRUE),
                        production_n=mean(.data$production,na.rm=TRUE),
                        sd_production_n=stats::sd(.data$production,na.rm=TRUE),
                        time_production_n=mean(.data$time_production,na.rm=TRUE),
                        sd_time_production_n=stats::sd(.data$time_production,na.rm=TRUE))

  out <- as.data.frame(out[c(2:7)])
  rownames(out)<-as.character(levels(as.factor(BGP[,1])))

  for(i in rownames(BGP)){
    BGP[i,c("sd_yield","sd_production","sd_time_production")]=out[BGP[i,"Label"],c("sd_yield_n","sd_production_n","sd_time_production_n")]

  }

  # Groups<-x$metaData$Layout # get the reactor 'Layout' as specified in 'BioGasData' to get a grouping vector
  # names(Groups)<-rownames(x$metaData) # name layouts by respective reactors
  #
  # flow_data<-cbind(dplyr::select(.data = x$metaData,1),matrix(nrow =nrow(x$metaData),ncol = 2,dimnames = list(rownames(x$metaData),c("max_production","time_max_production")))) # create a matrix for flow data extraction
  #
  # for(i in rownames(flow_data)){
  #   data<-subset(x$BioGasData,x$BioGasData$reactor=={{i}})
  #   flow_data[i,c("max_production","time_max_production")]=c(max(data$production),subset(data,data$production==max(data$production))$time)
  # }
  #
  # flow_data<-dplyr::reframe(.data = dplyr::group_by(.data = flow_data,.data$Layout),production=.data$max_production,sd_production=stats::sd(.data$max_production),time_production=.data$time_max_production,sd_time_production=stats::sd(.data$time_max_production))
  #
  # # 'BMP_data' to store the final output e.g. BMP's/ max flow per layout
  # BMP_data=data.frame(matrix(nrow = length(get_ReactorLayout(x)),ncol = 2,dimnames = list(rownames(x$metaData),c("yield","sd_yield")#,"max_production","sd_max_production","time_max_production","sd_time_max_production")
  #                                                                                                                                )))
  #
  # for(i in c(1:nrow(BMP_data))){ # for each group
  #   # subset the data to only have data of the chosen layout
  #   data<-subset(x$BioGasData,x$BioGasData$reactor%in%names(Groups)[grep(Groups[grep(i,rownames(x$metaData))],Groups)])
  #   data_2<-subset(data,data$time==timep) # further subset the data to have only obs at 'timep'
  #
  #   # calculate mean BMP and it's sd for each layout; also extract max flow and calculate the respective sd
  #   BMP_data[i,]=c(round(subset(data_2,data_2$reactor==names(Groups[i]))$yield,2),round(stats::sd(data_2$yield,na.rm=T),2))
  # } ### Flow implementation still needs a workover !!!!! (09.01.2026)
  #
  # for(i in names(BMP_data)){
  #   x<-add_metaData(x,as.character(BMP_data[,{{i}}]),{{i}})
  # }
  #
  # flow_data<-as.data.frame(flow_data)
  #
  # for(i in names(flow_data)[2:5]){
  #   x<-add_metaData(x,as.character(flow_data[,{{i}}]),{{i}})
  # }

  for(i in names(BGP)[-1]){
    x<-add_metaData(x,as.character(BGP[,{{i}}]),FALSE,{{i}})
  }

  # give feedback
  if(isTRUE(feedback)) print(paste0("Yield summarized (",x$ExpParam$name,")..."),quote = F)

  return(x) # return modified x

}

#' @rdname calc_yield
#'
#' @details
#' The function `relative_production` calculates the relative biogas production ((accumulated biogas volume at time)/(final accumulated biogas volume)).
#' It uses the values of the 'production' column in the `BioGasData` layer of a `BGF` and stores its results in the 'rel_production' column.
#'
#' @examples
#' # calculate relative production
#' myBGF <- relative_production(myBGF,TRUE)
#'
#' @export
#'


# relative_production() ####
relative_production=function(x,feedback=F){
  if(isFALSE(class(x)=="BGF")){ # check if 'x' is class basic_BGF
    stop("'x' must be class 'BGF'!",
         call. = FALSE)
  }

  df<-x$BioGasData # extract BioGasData from input

  for (q in levels(x$BioGasData$reactor)){
  set <- subset(df,df$reactor=={{q}})

  vmax <- subset(set,set$time==max(set$time))$product
  names(vmax) <- subset(set,set$time==max(set$time))$reactor

  for(i in which(x$BioGasData$reactor=={{q}})){
    x$BioGasData[i,"rel_production"]=round((x$BioGasData$product[i]/vmax[as.character(x$BioGasData$reactor[i])])*100,2)
  }

  }

  # give feedback
  if(isTRUE(feedback)) print(paste0("Relative production rate calculated (",x$ExpParam$name,")..."),quote = F)

  return(x) # return modified x

}

#' @rdname calc_yield
#'
#' @details
#' The function `calculate_flow_from_volume` uses the 'product' column of a `BGF`s `BioGasData` layer and generates values for the 'production' column.
#' In particular, it takes the 'product' value at each time and subtracts the previous 'product' value, specifically for each fermentation.
#' This reflects the amount of product formed in between time_now and time_previous and has the unit 'volume/time'.
#' However, if the 'product' column does not contain volumetric biogas data (e.g. gravimetric biogas data instead) the unit of the production column can be different (e.g. 'mass/time').
#' The first 'product' value is subtracted by itself.
#'
#' @examples
#' # create another example BGF
#' myBGF2 <- from_standard_record(
#'         ReactorLayout = "A",
#'         ProcessTemp = 80,
#'         InocToSubRatio = .1,
#'         path = base::system.file("extdata","Fermentation_A.tsv",package = "BGFanalyzeR"),
#'         time_col = 1,
#'         product_col = 3)
#'
#' # calculate production
#' myBGF2 <- calculate_flow_from_volume(myBGF2)
#'
#' @export
#'

# calculate_flow_from_volume ####
calculate_flow_from_volume=function(x){

  newData<-x$BioGasData[,c("reactor","time","product")]

  pos=list()

  for(i in levels(newData$reactor)){
    pos[[{{i}}]]=as.numeric(rownames(subset(newData,newData$reactor=={{i}})))
  }

  for(i in names(pos)){
    flow=subset(newData,newData=={{i}})

    for(j in c(1:nrow(flow))){

      if(j==1){
        x$BioGasData[pos[[i]][j],"production"]=flow$product[j]-flow$product[j]
      }else{
        x$BioGasData[pos[[i]][j],"production"]=flow$product[j]-flow$product[j-1]
      }
    }
  }


  return(x) # return modified list

}





