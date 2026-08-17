#' Calculate an inoculation matrix
#'
#' The function calculates an inoculation matrix based on the choosen inoculum-to-substrate ratio (`InocToSubRatio`) of a BGF.
#' To this end, the reactor volume and the concentrations of organics in inoculum and substrate should be known.
#' Most commonly, this concentration is provided as total solutes, organic total solutes or chemical oxygen demand of inoculum and substrate.
#'
#' The function `calc_inoc_matrix_from_metaData` is meant to build a bridge between data analysis in R and experimental work in the lab.
#' Furthermore, this function should support the design of real world biogas batch-fermentations, so it is usually used on a `BGF` before the data of the `BioGasData` layer has been generated.
#'
#' The `BGF` this function is used on should have the concentration of organics of inoculum and substrate in its `metaData` layer.
#' In a biogas fermentation, the term 'inoculum' refers to the source of biogas producing organisms, while the term 'substrate' refers to the source material these organisms produce the biogas from.
#'
#' To achieve high comparability in between experiments, fermentations should be started based on the same inoculum-to-substrate ratio.
#' Furthermore, if the concentration of organics in inoculum and substrate are known, the inoculum-to-substrate ratio can be used to calculate the amount of biogas produced from the inoculum and substrate fraction of a biogas reactor.
#'
#' This allows the calculation of substrate specific biogas potentials.
#'
#' @param x a `BGF`
#' @param col either a `character` or an `integer` specifying which column of the `metaData` layer provides information about the organics concentration of the fermentations.
#' @param reactor a `numeric` providing the total filling volume or mass of the liquid phase of the biogas reactors; default = 400
#' @param ISRatio default = `NULL`; if specified a `numeric` is expected, providing the inoculum to substrate ratio of a biogas fermentation. If left to the default, a value is extracted from the `ExpParam` layer of the `BGF`
#' @param VSInoc NOT WORKING default = `NULL`; can be specified if the `BGF` does not contain any 'Blanks' (`get_blanks` returns 'R'). If specified, either a `integer` or `character` is expected that refers to the row in the `metaData` layer from which to take the organics concentration of the inoculum. If left the default, the same organics concentration is assumed for inoculum and substrat
#' @param subset default = `NULL`; if specified, a `character` vector is expected, declaring a subset of fermentation by their respective row name within the `metaData` layer of the `BGF`
#' @param digits argument passed to [round]; default = 2
#'
#' @return a `data.frame`; the inoculation matrix to set up a biogas fermentation (series)
#'
#' @examples
#' # create an example BGF
#' myBGF<-BGF(
#'         ReactorLayout = c("2*Blank","Cellulose","3*neg ctrl","3*FR1","3*FR2","3*FR3"),
#'         BlankLabel = "Blank",
#'         name = "myBGF",
#'         ProcessTemp = 52,
#'         InocToSubRatio = 2,
#'         MeasurementType = "manuel")
#'
#' # add organic total solutes measurement for each biogas fermentation to 'metaData' layer
#' myBGF<-add_metaData(
#'         x = myBGF,
#'         what = c(3.63,3.63,98,1.65,1.65,1.65,1.76,1.76,1.76,1.57,1.57,1.57,1.68,1.68,1.68),
#'         lab = "oTS")
#'
#' # calculate ionoculation matrix for all or a subset of fermentations
#' InocMatrix<-calc_inoc_matrix_from_metaData(myBGF,"oTS")
#' InocMatrix_subset<-calc_inoc_matrix_from_metaData(myBGF,"oTS",subset = c("R5","R12","R9"))
#'
#' # create a second example BGF without a 'Blank'
#' myBGF2<-BGF(LETTERS[1:5],"Blank","myBGF2",80,.1,"manuel")
#'
#' # add organic total solutes to 'metaData' layer
#' myBGF2<-add_metaData(myBGF2,what = c(3.63,2.9,3.1,4.19,1.53),lab = "oTS")
#' InocMatrix_2<-calc_inoc_matrix_from_metaData(myBGF2,4,2000)
#' InocMatrix_2_fixed<-calc_inoc_matrix_from_metaData(myBGF2,4,2000,VSInoc=5)
#'
#' @export

# calc_inoc_matrix_from_metaData() #####
calc_inoc_matrix_from_metaData=function(x,col,reactor=400,ISRatio=NULL,VSInoc=NULL,subset=NULL,digits=2){
  if(isFALSE(class(x)=="BGF")){ # check if 'x' is class BGF
    stop("'x' must be class 'BGF'!",
         call. = FALSE)
  }

  if(isTRUE(is.null(ISRatio))) ISRatio=x$ExpParam$InocToSubRatio

  if(isTRUE(is.character(col))) col<-grep(col,names(x$metaData))

  if(isTRUE(is.null(subset))){
    InocMatrix<-data.frame(matrix(nrow = 2,ncol = nrow(x$metaData),dimnames = list(row_names=c("Inoculum","Substrate"),col_names=row.names(x$metaData))))
  }else{
    if(is.character(subset)) subset=match(subset,row.names(x$metaData))

    InocMatrix<-data.frame(matrix(nrow = 2,ncol = length(subset),dimnames = list(row_names=c("Inoculum","Substrate"),col_names=row.names(x$metaData)[subset])))
  }


  if(isFALSE(all(get_blanks(x)=="R"))){
    VSblank<-mean(subset(x$metaData[,col],x$metaData[,"Blank"]==TRUE))

  for(i in colnames(InocMatrix)){
    if(isTRUE(i%in%get_blanks(x))){
      InocMatrix["Inoculum",{{i}}]=reactor
      InocMatrix["Substrate",{{i}}]=0
    }else{
      InocMatrix["Inoculum",{{i}}]=round(((reactor*ISRatio)*x$metaData[{{i}},col])/(VSblank+ISRatio*x$metaData[{{i}},col]),digits = digits)
      InocMatrix["Substrate",{{i}}]=round(reactor-InocMatrix["Inoculum",{{i}}],digits = digits)
    }
  }}else{
    for(i in colnames(InocMatrix)){
      if(isTRUE(is.null(VSInoc))) VSInoc=x$metaData[{{i}},col] else VSInoc=x$metaData[{{VSInoc}},col] # move outside for loop; improve VSInoc selection!!!

        InocMatrix["Inoculum",{{i}}]=round(((reactor*ISRatio)*x$metaData[{{i}},col])/(VSInoc+ISRatio*x$metaData[{{i}},col]),digits = digits)
        InocMatrix["Substrate",{{i}}]=round(reactor-InocMatrix["Inoculum",{{i}}],digits = digits)
      }
    }



  return(InocMatrix)
}
