#' Advanced `BGF` visualization
#'
#' A set of functions that allow advanced data visualization employing `ggplot2` and `plotly`.
#' The two main functions are `bgf_plot`, which is a high end wrapper to produce frequently needed plots and `plot_curve`, which is offers more freedom during plot creation.
#'
#' The main function for advanced data visualization purposes is `bgf_plot`.
#' It is a wrapper that successively calls `plot_product_curve`, `plot_production_curve`, `plot_rel_production_curve`, `plot_netProduct_by_Layout`, `colplot_yield`  and `boxplot_yield`.
#' It stores the output of each function in a `list` which is returned
#'
#' @param x a `BGF`
#' @param type a `character` indicating which plots should be produced. Possible values are 'product', 'netProduct', 'production', 'relProduction','yield_col' or 'yield_box, which yield in a product line plot, a net product line plot, a production line plot, a relative production line plot, a yield col plot or a yield box plot, respectively. It is possible to use any combination of valid types (e.g. 'type = c("product","production","yield_box")'). The default selection is 'all', which will cause the function to produce all plots.
#' @param ... further arguments passed to `plot_curve`, `plot_netProduct_by_Layout`, `colplot_yield` or `boxplot_yield`
#'
#' @importFrom rlang .data
#'
#' @returns either a single `ggplot2` object or a `list` of `ggplot2` objects
#'
#' @examples
#' # create an example BGF
#' myBGF <- from_AMPTSV2_report(
#'         ReactorLayout = c("2*Meso","Cellulose","2*S1 ctrl","2*S1 7d","2*S1 4d",
#'                           "2*S2 ctrl","2*S2 4d","2*S2 6d"),
#'         BlankLabel = "Meso",
#'         name = "myBGF",
#'         ProcessTemp = 40,
#'         InocToSubRatio = 2,
#'         path = base::system.file("extdata","AMPTSV2.csv",package = "BGFanalyzeR"))
#'
#'
#' # generate all plots for the BGF
#' Plots <- bgf_plot(myBGF)
#'
#' # print 'product' curve
#' Plots[["product_curve"]]
#'
#' # print 'net product' curve
#' Plots[["net_product_curve"]]
#'
#' # print 'production' curve
#' Plots[["production_curve"]]
#'
#' # print 'relative production' curve
#' Plots[["rel_production_curve"]]
#'
#' # print 'yield' colplot
#' Plots[["yield_col"]]
#'
#' # print 'yield' boxplot
#' Plots[["yield_box"]]
#'
#' @export
#'
#'

# end user function for plot creation ####
bgf_plot=function(x,type="all",...){

  BGF_Plots <- list() # a list to store plots

  if(type%in%c("all","product")){
    BGF_Plots[["product_curve"]] <- plot_product_curve(x,...)
  }

  if(type%in%c("all","netProduct")){
    BGF_Plots[["net_product_curve"]] <- plot_netProduct_by_Layout(x,...)
  }

  if(type%in%c("all","production")){
    BGF_Plots[["production_curve"]] <- plot_production_curve(x,...)
  }

  if(type%in%c("all","relProduction")){
    BGF_Plots[["rel_production_curve"]] <- plot_rel_production_curve(x,...)
  }

  if(type%in%c("all","yield_col")){
    BGF_Plots[["yield_col"]] <- colplot_yield(x,...)
  }

  if(type%in%c("all","yield_box")){
    BGF_Plots[["yield_box"]] <- boxplot_yield(x,...)
  }

  if(length(BGF_Plots)==1) BGF_Plots <- BGF_Plots[[1]]

  return(BGF_Plots)
}

#' @rdname bgf_plot
#'
#' @details
#' The second function, that allows advanced data visualization is `plot_curve`.
#' It can be used to draw a line plot from data in the `BioGasData`-layer of a `BGF`.
#' In this plot, the 'time' column representing the fermentation/ observation time will be on the x-axis.
#' The y-axis can be any other column of the `BioGasData`-layer and is specified via the 'what' argument.
#' Furthermore, it is required to provide a grouping variable in the 'color' argument.
#' If that grouping variable has more than 24 levels, the 'col' argument must be changed as in the default setting only 24 colors are available for plotting.
#'
#' There exist several wrapper functions, that internally call `plot_curve` exist.
#' These are:
#'
#' @param what the name of a (`numeric`) column in the `BioGasData`-layer of a BGF. Must be specified without quotes
#' @param color the name another column in the `BioGasData`-layer holding a grouping variable
#' @param col a vector with colors to be used to differentiate between levels of the grouping variable. In the default setting, 24 colors are provided
#' @param coltitle the title of the plot legend. default ='Reactor:'
#' @param col_names default is `NULL`. If specified, a `character` with the same length as levels in the 'color' argument is expected. It can be used change the default labels in the legend
#' @param title default is `NULL`. If specified, a title for the plot can be created
#' @param subtitle default is `NULL`. If specified, a subtitle for the plot can be created
#' @param xlab can be used to change the default title of the x-axis. Default is [waiver][ggplot2::waiver]
#' @param ylab can be used to change the default title of the y-axis. Default is [waiver][ggplot2::waiver]
#' @param keep_excluded `logic`, default is `TRUE`. Should fermentations that are marked as 'Excluded' in the `metaData`-layer be shown in the plot?
#' @param interaction `logic`, default is `FALSE`. If `TRUE` the plot will be created using `plotly` as plot engine instead of `ggplot2`
#'
#' @examples
#'
#' # in the following examples the same plots are produced by plot_curve and the respective wrappers
#' # plot 'product' curve
#' plot_curve(
#'         x = myBGF,
#'         what = product,
#'         color = reactor,
#'         ylab = "raw exaust gas volume",
#'         xlab = paste0("observation time [",myBGF$ExpParam$timeScale,"]"))
#' plot_product_curve(myBGF)
#' bgf_plot(myBGF,type="product")
#'
#' # plot 'production' curve
#' plot_curve(
#'         x = myBGF,
#'         what = production,
#'         color = reactor,
#'         ylab = "raw exaust gas flow",
#'         xlab = paste0("observation time [",myBGF$ExpParam$timeScale,"s]"))
#' plot_production_curve(myBGF)
#' bgf_plot(myBGF,type="production")
#'
#' # plot 'rel_production' curve(s)
#' plot_curve(
#'         x = myBGF,
#'         what = rel_production,
#'         color = reactor,
#'         ylab = "remaining gas production [%]",
#'         xlab = paste0("observation time [",myBGF$ExpParam$timeScale,"s]"))
#' plot_rel_production_curve(myBGF) # Note: this curve is inverted so it starts at 100 %
#' bgf_plot(myBGF,type="relProduction")
#'
#' @export
#'

# plot_curve() ####
plot_curve=function(x,what,color,col=BGFanalyzeR::BGF_defaultcolors,coltitle="Reactor:",col_names=NULL,title=NULL,subtitle=NULL,xlab=ggplot2::waiver(),ylab=ggplot2::waiver(),keep_excluded=TRUE,interaction=FALSE){

  df<-x$BioGasData # get the BioGasData

  # check if excluded reactors should appear in the plot
  if(isFALSE(keep_excluded)) df<-subset(df,!df$reactor%in%row.names(subset(x$metaData,x$metaData$Excluded==T)))
  if(isFALSE(keep_excluded)) print(paste0("Removed excluded reactors ",paste(row.names(subset(x$metaData,x$metaData$Excluded==T)),collapse = ", ")," before plotting..."),quote=F)

  if(isFALSE(interaction)){
    out<- # bulid the plot
    ggplot2::ggplot(data = df,
                    mapping=ggplot2::aes(x=.data$time,y={{what}},color={{color}}))+
    ggplot2::geom_line(key_glyph = ggplot2::draw_key_rect)+
    ggplot2::scale_color_manual(values = col,label=rownames(x$metaData))+
    ggplot2::labs(title=title,subtitle = subtitle,x=xlab,y=ylab,color=coltitle)+
    ggplot2::theme_bw()+
    ggplot2::theme(legend.position = "bottom")

    # check if 'col_names' was specified and change color labels appropriately
    if(isFALSE(is.null(col_names))) out<-out + ggplot2::scale_color_manual(values = col,label=col_names)

  }else{
    out<- # bulid the plot
      ggplot2::ggplot(data = df,
                      mapping=ggplot2::aes(x=.data$time,y={{what}},color={{color}}))+
      ggplot2::geom_line()+
      ggplot2::scale_color_manual(values = col,label=rownames(x$metaData))+
      ggplot2::labs(title=title,subtitle = subtitle,x=xlab,y=ylab,color=coltitle)+
      ggplot2::theme_bw()+
      ggplot2::theme(legend.position = "bottom")

    # check if 'col_names' was specified and change color labels appropriately
    if(isFALSE(is.null(col_names))) out<-out + ggplot2::scale_color_manual(values = col,label=col_names)

    # make the plot interactive if desired
    out<-plotly::ggplotly(out)

    }

 # print(out) # print the plot

  return(out) # return the  plot

}

#' @rdname bgf_plot
#'
#' @details
#' \itemize{
#'    \item `plot_product_curve`: Produces a 'product' line  plot. Argument 'what' set to 'product' and argument 'color' set to 'reactor'.  Can be called via `bgf_plot` when 'type = "product"' or 'type = "all"'. In the later case, the plot is stored in a `list` with the label 'product_curve'.
#'  }
#'
#' @export
#'

##  plot_product_curve() ####
plot_product_curve=function(x,...){

  if(isTRUE(is.null(x$ExpParam$timeScale))) x<-add_ExpParam(x,c("timeScale"="default"))

  out<-plot_curve(x,what = .data$product, color = .data$reactor,ylab = "raw exaust gas volume",xlab = paste0("observation time [",x$ExpParam$timeScale,"]"),...) # apply 'plot_curve()' with certain pre settings to plot a volume curve per reactor

  return(out)
}

#' @rdname bgf_plot
#'
#' @details
#' \itemize{
#'    \item `plot_production_curve`: Produces a 'production' line  plot. Argument 'what' set to 'production' and argument 'color' set to 'reactor'.  Can be called via `bgf_plot` when 'type = "production"' or 'type = "all"'. In the later case, the plot is stored in a `list` with the label 'production_curve'.
#'  }
#'
#' @export
#'

##  plot_production_curve() ####
plot_production_curve=function(x,...){

  if(isTRUE(is.null(x$ExpParam$timeScale))) x<-add_ExpParam(x,c("timeScale"="default"))

  out<-plot_curve(x,what = .data$production, color = .data$reactor,ylab = "raw exaust gas flow",xlab = paste0("observation time [",x$ExpParam$timeScale,"s]"),...) # apply 'plot_curve()' with certain pre settings to plot a flow curve per reactor

  return(out)
}

#' @rdname bgf_plot
#'
#' @details
#' \itemize{
#'    \item `plot_rel_production_curve`: Produces an inverted 'rel_production' line  plot. Argument 'what' set to 'c(100-rel_production)' and argument 'color' set to 'reactor'. Can be called via `bgf_plot` when 'type = "relProduction"' or 'type = "all"'. In the later case, the plot is stored in a `list` with the label 'rel_production_curve'
#'  }
#'
#' @export
#'

##  plot_rel_production_curve() ####
plot_rel_production_curve=function(x,...){

  if(isTRUE(is.null(x$ExpParam$timeScale))) x<-add_ExpParam(x,c("timeScale"="default"))

  out<-plot_curve(x,what = c(100-.data$rel_production), color = .data$reactor,ylab = "remaining gas production [%]",xlab = paste0("observation time [",x$ExpParam$timeScale,"s]"),...) # apply 'plot_curve()' with certain pre settings to plot a production curve per reactor

  return(out)
}

#' @rdname bgf_plot
#'
#' @details
#' The function `plot_netProduct_by_Layout` plots mean net gas curves per layout.
#' It will extract 'net_product' values from the `BioGasData`-layer and calculates a mean for each 'Layout' specified in the `metaData`-layer at each 'time' before drawing the line plot.
#'
#' Can be called via `bgf_plot` when 'type = "netProduct"' or 'type = "all"'.
#' In the later case, the plot is stored in a `list` with the label 'net_product_curve'.
#'
#' @examples
#' # plot mean net product by reactor layout
#' plot_netProduct_by_Layout(myBGF)
#'
#' @export
#'

# plot_netProduct_by_Layout() ####
plot_netProduct_by_Layout=function(x,col=BGFanalyzeR::BGF_defaultcolors[2+3*c(0:7)],coltitle="Layout:",col_names=NULL,title=NULL,subtitle=NULL,xlab=paste0("observation time [",x$ExpParam$timeScale,"s]"),ylab="net exhaust gas volume",keep_excluded=TRUE,interaction=FALSE){
  if(isTRUE(is.null(x$ExpParam$timeScale))) x<-add_ExpParam(x,c("timeScale"="default"))

  df<-x$BioGasData # get the BioGasData

  df$Layout=NA

  for( i in c(1:nrow(df))){
    df$Layout[{{i}}] <- as.character(x$metaData[df$reactor[{{i}}],"Layout"])
  }

  df$Layout<- factor(df$Layout,levels = levels(x$metaData$Layout))

  # check if excluded reactors should appear in the plot
  if(isFALSE(keep_excluded)) df<-subset(df,!df$reactor%in%row.names(subset(x$metaData,x$metaData$Excluded==T)))
  if(isFALSE(keep_excluded)) print(paste0("Removed excluded reactors ",paste(row.names(subset(x$metaData,x$metaData$Excluded==T)),collapse = ", ")," before plotting..."),quote=F)

  df<-dplyr::reframe(.data = df,mean_product=mean(.data$net_product),low=mean(.data$net_product)-stats::sd(.data$net_product),up=mean(.data$net_product)+stats::sd(.data$net_product),.by = c(.data$Layout,.data$time))

  out<- # bulid the plot
    ggplot2::ggplot(data = df,
                    mapping=ggplot2::aes(x=.data$time,y=.data$mean_product,color=.data$Layout))+
  #  ggplot2::geom_pat(df,mapping = ggplot2::aes(x=time,ymin=low,ymax=up),fill="grey")+
    ggplot2::geom_line(key_glyph=ggplot2::draw_key_rect)+
    ggplot2::scale_color_manual(values = col)+
    ggplot2::labs(title=title,subtitle = subtitle,x=xlab,y=ylab,color=coltitle)+
    ggplot2::theme_bw()+
    ggplot2::theme(legend.position = "bottom")


  # check if 'col_names' was specified and change color labels appropriately
  if(isFALSE(is.null(col_names))){ out<-out + ggplot2::scale_color_manual(values = col,label=col_names)
}
  # check if interaction is true and make the plot interactive if desired
  if(isTRUE(interaction)) out<-plotly::ggplotly(out)

 # print(out) # print the plot

  return(out) # return the  plot

}

#' @rdname bgf_plot
#'
#' @details
#' The function `colplot_yield` produces a col plot with reactor layouts on the x-axis and mean 'yield' values per layout on the y-axis.
#' It can be used if a yield  summary was transferred to the `metaData`-layer using [summarize_yield].
#' It will generate a colplot with reactor layouts on the x-axis and mean yield values per layout on the y-axis.
#' With the default settings, fermentations that are marked as 'Excluded' in the `metaData`-layer will not be included in the plot.
#' The standard deviation around the mean value will be calculated and drawn as an [errorbar][ggplot2::geom_errorbar] if possible.
#'
#' Can be called via `bgf_plot` when 'type = "yield_col"' or 'type = "all"'.
#' In the later case, the plot is stored in a `list` with the label 'yield_col'.
#'
#' @param hide default is `NULL`. Can be a `character` specifying reactor layouts to hide from the plot
#' @param Excluded `logic`; default is `FALSE`. Should fermentations that are marked as 'Excluded' in the `metaData`-layer appear in the plot?
#' @param yield_label `logic`; default is `FAlSE`. Should the mean yield be shown as a label on the plot?
#' @param yield_label_pos a `numeric` specifying the position of the yield label in the plot
#' @param yield_unit a `character` specifying the unit of the yield. The default is 'Nml/gVS', which is read 'norm milliliter pre gram of volatile solutes'
#'
#' @examples
#' # create a col plot of the yield
#' colplot_yield(myBGF)
#' bgf_plot(myBGF,type="yield_col")
#'
#' @export
#'

# colplot_yield() ####
colplot_yield=function(x,hide=NULL,Excluded=FALSE,col=BGFanalyzeR::BGF_defaultcolors[2+3*c(0:7)],coltitle="Layout:",col_names=NULL,title=NULL,subtitle=NULL,yield_label=FALSE,yield_label_pos=100,yield_unit="Nml/gVS",interaction=FALSE){

  df <-get_yield_summary(x,Excluded,F)
  df$Layout=rownames(df)
  try({df<-subset(df,df$Layout!=unique(get_BlankLabel(x)))},silent = TRUE)

  if(isFALSE(is.null(hide)))df<-subset(df,!df$Layout%in%hide)

  out<-ggplot2::ggplot(data = df,ggplot2::aes(x=.data$Layout,y=.data$yield,fill=.data$Layout))+
    ggplot2::geom_col()+
    ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin=.data$yield-.data$sd_yield,ymax=.data$yield+.data$sd_yield))+
    ggplot2::scale_fill_manual(values = col)+
    ggplot2::labs(title=title,subtitle = subtitle,x=NULL,y="Biogas yield",fill=coltitle)+
    ggplot2::theme_bw()+
    ggplot2::theme(legend.position = "bottom")

  # check if 'col_names' was specified and change color labels appropriately
  if(isFALSE(is.null(col_names))) out<-out + ggplot2::scale_color_manual(values = col,label=col_names)

  if(isTRUE(yield_label)) out <- out + ggplot2::geom_label(ggplot2::aes(y=yield_label_pos,label=paste(round(.data$yield,2),yield_unit,sep = "\n")),fill="white")

   # check if interaction is true and make the plot interactive if desired
  if(isTRUE(interaction)) out<-plotly::ggplotly(out)


#  print(out) # print the plot

  return(out) # return the  plot
}

#' @rdname bgf_plot
#'
#' @details
#' The function `boxplot_yield` produces a box plot with reactor layouts on the x-axis and and 'yield' values per layout on the y-axis.
#' The argument 'timep' can be used to specify how many observations will be used to draw the boxes.
#' In the default setting, 'all', all avalilabel observations for a layout will be used.
#' Alternatively, to select the final observations 'timep = "max"' can be set.
#' Furthermore any `numeric` that matches a value in the 'time' column of the `BioGasData`-layer can be passed to 'timep' to select the specific 'yield' value at that time.
#'
#' Can be called via `bgf_plot` when 'type = "yield_box"' or 'type = "all"'.
#' In the later case, the plot is stored in a `list` with the label 'yield_box'.
#'
#' @param timep a `numeric` specifying the time point at which to calculate the  yield for the individual reactor layouts. Alternatively, using the `character` expressions 'all' will draw the box over all values in the 'yield' and 'max' will draw the box using the final 'yield' value
#'
#' @examples
#' # create a box plot of the 'yield'
#' boxplot_yield(myBGF)
#' bgf_plot(myBGF,type="yield_box")
#'
#' # create a box plot of the final 'yield'
#' boxplot_yield(myBGF,timep="max")
#' bgf_plot(myBGF,type="yield_box",timep="max")
#'
#' @export
#'

# boxplot_yield() ####
boxplot_yield=function(x,timep="all",hide=NULL,Excluded=FALSE,col=BGFanalyzeR::BGF_defaultcolors[2+3*c(0:7)],coltitle="Layout:",col_names=NULL,title=NULL,subtitle=NULL,yield_label=FALSE,yield_label_pos=100,yield_unit="Nml/gVS",interaction=FALSE){

  df<- x$BioGasData
  df$Layout=NA

  layouts <- dplyr::pull(.data = x$metaData,1)
  names(layouts)<-rownames(x$metaData)

  for(i in c(1:nrow(df))){
    df$Layout[i]=as.character(layouts[as.character(df$reactor)[i]])

  }

  try({df<-subset(df,df$Layout!=unique(get_BlankLabel(x)))},silent = TRUE)

  if(isFALSE(Excluded)) df<-subset(df,!df$reactor%in%get_Excluded(x))

  if(isFALSE(is.null(hide)))df<-subset(df,!df$Layout%in%hide)

  if(isTRUE(timep=="max")) timep=max(df$time)

  if(isTRUE(timep=="all")) timep=NULL

  if(isFALSE(is.null(timep))) df<-subset(df,df$time==timep)

  if(isTRUE(yield_label)) df2<-dplyr::mutate(.data = dplyr::group_by(.data = df,.data$Layout),mean_bgp=mean(.data$yield,na.rm = T))

  out<-ggplot2::ggplot(data = df)+
    ggplot2::geom_boxplot(mapping = ggplot2::aes(x=.data[["Layout"]],y=.data[["yield"]],fill=.data[["Layout"]]))+
    ggplot2::scale_fill_manual(values = col)+
    ggplot2::labs(title=title,subtitle = subtitle,x=NULL,y="Biogas yield",fill=coltitle)+
    ggplot2::theme_bw()+
    ggplot2::theme(legend.position = "bottom")

  # check if interaction is true and make the plot interactive if desired
  if(isTRUE(interaction)) out<-plotly::ggplotly(out)

  if(isTRUE(yield_label)){
    df2<-dplyr::reframe(.data = dplyr::group_by(.data = df2,.data$Layout),mBGP=unique(.data$mean_bgp))

    out <- out + ggplot2::geom_label(data = df2,mapping=ggplot2::aes(x=.data$Layout,y=.data$yield_label_pos,label=paste(round(.data$mBGP,2),yield_unit,sep = "\n")),fill="white")
    }

 # print(out)

  return(out)

}









