#' Data correction functions
#'
#' A set of functions that provide shortcuts for frequently used operations conducted on a `BGF`.
#' For instance, they allow to specify the reactor layout in a formula like manner or to change a the type of any column of a `BGF`'s `metaData` or `BioGasData` layer to `numeric`.
#'
#' The function `correct_RLayout` allows to write 'ReactorLayout=c("3`*`Blank","3`*`Cellulose","3`*`Treatment A")' instead of 'ReactorLayout=c("Blank","Blank","Blank","Cellulose","Cellulose","Cellulose","Treatment A","Treatment A","Treatment A")' or 'ReactorLayout=c(rep("Blank",3),rep("Cellulose",3),rep("Treatment A",3))'.
#'
#' The function will recognize the '\*' and expects an `integer` on the left side specifying how often the right side should be repeated.
#'
#' It is useful when dealing with replicates among the fermentations of a `BGF`, as it can be used to add redundant information to the 'metaData' layer of a (see examples).
#'
#' @param lo a `character` specifying a reactor layout (via the 'ReactorLayout' argument of certain `BGFanalyzeR` functions)
#'
#' @returns a `character` (`correct_RLayout`)
#'
#' @examples
#' # define a reactor layout
#' RL <- c("3*Blank","3*Cellulose","3*Treatment A")
#'
#' # correct reactor layout
#' RL_cor <- correct_RLayout(RL)
#'
#' RL_cor # print corrected layout
#'
#' @export
#'


# correct_RLayout ####
correct_RLayout=function(lo){
  out<-NULL # creates an empty output variable

  for(i in lo){

    if(grepl("\\*",i)){ # detects if the character '*' is part of x;
      # if TRUE, a correction is applied
      out<-c(out,rep(
        strsplit(i,"\\*")[[1]][2], # spit each element of 'x' at its '*'  and repeat the second part;
        as.numeric(strsplit(i,"\\*")[[1]][1]))) # as specified in the first part
    }else{out<-c(out,i)} # else just paste together all elements of x
  }

  return(out) # return output
}

#' @rdname correct_RLayout
#'
#' @details
#' The function `cols_to_numeric` allows to change the type of any column in the `metaData` or `BioGasData` layer of a `BGF` to `numeric`.
#' It is possible to apply this change to several columns of the same layer at once.
#'
#' @param x a `BGF`
#' @param vec either a `character` of column names or an `integer` indicating the positions of columns in the chosen `layer`
#' @param layer a `character` specifying a layer of a `BGF`. Can be 'metaData' or 'BioGasData'
#'
#' @returns a `BGF` (`cols_to_numeric`)
#'
#' @examples
#' # example code
#' myBGF <- BGF(RL)
#'
#' # add dry total solutes concentration to 'metaData'
#' myBGF<-add_metaData(x = myBGF,what = correct_RLayout(c("3*2.9","3*98.7","3*8.4")),lab="TS")
#'
#' # change the new 'TS' column in 'metaData' layer to numeric
#' myBGF <- cols_to_numeric(myBGF,"TS","metaData")
#'
#' @export
#'

# cols_to_numeric() ####
cols_to_numeric=function(x,vec=c(2:4),layer="BioGasData"){
  if(isFALSE(class(x)=="BGF")){ # check if 'x' is class basic_BGF
    stop("'x' must be class 'BGF'!",
         call. = FALSE)
  }

  for(i in vec){ # convert each column of 'layer' specified in 'vec' to numeric
    x[[{{layer}}]][,i]=as.numeric(x[[{{layer}}]][,i])
  }

  return(x)

}
