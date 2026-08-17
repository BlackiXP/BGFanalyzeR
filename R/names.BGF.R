#' #' Get the names of a `BGF`
#' #'
#' #' Default `names` method for a BGF.
#' #'
#' #' Creates a `character`.
#' #'
#' #' @param x An object of class `BGF`
#' #' @returns a `character`
#' #'
#' #' @examples
#' #' # create an example BGF
#' #' myBGF <- BGF("A", name = "myBGF" )
#' #'
#' #'
#' #' @export
#' #'
#'
#' names.BGF=function(x){
#'
#'   name1<-x$ExpParam$name
#'
#'   name2<-names(x$ExpParam)
#'  # name2<-subset(name2,name2!="name")
#'
#'   name3 <- names(x$metaData)
#'
#'   name4 <- names(x$BioGasData)
#'
#'   name <- c(name1,name2,name3,name4)
#'
#'   names(name)<-c("BGF_NAME",
#'                  paste0(rep("ExpParam",length(name2)),"_",c(1:length(name2))),
#'                  paste0(rep("metaData",length(name3)),"_",c(1:length(name3))),
#'                  paste0(rep("BioGasData",length(name4)),"_",c(1:length(name4))))
#'
#'   #print(name[1])
#'
#'   return(name)
#' }
#'
