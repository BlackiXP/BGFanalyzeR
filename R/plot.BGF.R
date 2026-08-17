#' Plot a Biogas Fermentation
#'
#' Default plot method for a BGF.
#'
#' Creates a raw exhaust gas volume plot of the fermentations stored in a BGF object.
#'
#' @param x An object of class BGF
#' @param xlab,ylab,main `character` strings specifying the title of the x-axis, the y-axis or the plot itself, respectively.
#' @inheritParams base::plot
#' @returns A plot of exhaust gas volumes
#'
#'
#' @export
#'

plot.BGF=function(x,xlab="Time",ylab="Volume [Nml]",main=paste0("Raw exhaust gas volume plot of ",x$ExpParam$name),...){

  Biogas<-x$BioGasData

  Biogas$pcol=NA
  for(i in c(1:nrow(Biogas))) Biogas$pcol[i]=BGFanalyzeR::BGF_defaultcolors[as.numeric(Biogas$reactor)[i]]

  base::plot(x=Biogas$time,
       y=Biogas$product,
       col=Biogas$pcol,
       xlab = xlab,
       ylab = ylab,
       main = main,...)
  graphics::legend("bottomright",legend = unique(Biogas$reactor),ncol = 5,pch = 15,col=Biogas$pcol)
}
