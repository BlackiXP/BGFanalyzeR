#' Data interpolation algorithm
#'
#' A function that allows to interpolate missing values in a `numeric` vector of a `data.frame`.
#' It requires a second column in the `data.frame` representing time, as well as a third column representing grouping information.
#'
#' The function `bgf_interpolation` takes a `data.frame` as input and returns a modified version of it as output.
#' In particular, it interpolates missing values in a `numeric` vector (argument 'x') based on a second vector (argument 't') without missing values.
#'
#' It is possible to stop the interpolation (argument 'end'), if no valid value in 'x' occurs after a missing value, or to use linear extrapolation to fill terminal `NA`'s.
#' The slope of the extrapolation is based on the last valid 'x' value and the \eqn{n^{th}} valid 'x' value before (argument 'offset').
#'
#' Furthermore, if linear extrapolation leads to e.g. negative gas concentrations or volumes, it is possible to replace these wrong values with any replacement (argument 'sub_zero')
#'
#' Interpolation and extrapolation are done specifically for groups within the data (argument 'group')
#'
#' @param df a `data.frame`
#' @param x an `integer` specifying the position of the vector with issing values that should be interpolated
#' @param t an `integer` specifying the position of the vector with time data over which to interpolate
#' @param group an `integer` specifying the position of a grouping vector within the input data
#' @param end `logic`; default is `TRUE`. Should interpolation end if the last valid 'x' value does not match the final 't' value?
#' @param sub_zero default is `NULL`. Can be a `numeric` which will be used to replace all (interpolated) values in 'x' that are below '0'.
#' @param offset an `integer`; default is 1. Can be used to specify the offset (distance between valid 'x' values) that is used during terminal data extrapolation (argument 'end = FALSE'). With the default setting ('offset = 1') the last valid 'x' value and 1 value before that are selected.
#'
#' @returns a `data.frame`
#'
#' @examples
#' # create an example data.frame
#' gap_data=data.frame(
#'               time=as.numeric(rep(c(1:30),3)),
#'               value=c(0,rep(NA,3),7,14,26,44,72,rep(NA,8),314,350,
#'                       377,rep(NA,5),471,rep(NA,4),0,rep(NA,7),100,
#'                       127,163,rep(NA,4),217,rep(NA,5),267,rep(NA,4),
#'                       0,rep(NA,3),0,rep(NA,3),5,rep(NA,3),12,rep(NA,3),
#'                       32,rep(NA,3),35,rep(NA,3),33,rep(NA,3),28,rep(NA,3),
#'                       5,NA),
#'               cat=c(rep("exGas",30),rep("exGas_2",30),rep("cBioG",30)))
#'
#' # interpolate and extrapolate missing values, replace values below '0' with '0'
#' closed_data<- bgf_interpolation(gap_data,2,1,3,end = FALSE,sub_zero = 0)
#'
#' # inspect the differences
#' plot(value ~ time,data=gap_data,col=as.factor(cat))
#' legend(legend = levels(as.factor(gap_data$cat)),col=c(1,2,3),x = "topleft",pch=15)
#'
#' plot(value ~ time,data=closed_data,col=as.factor(cat))
#' legend(legend = levels(as.factor(closed_data$cat)),col=c(1,2,3),x = "topleft",pch=15)
#'
#' @export
#'



# interpolation() ####
bgf_interpolation=function(df,x,t,group,end=TRUE,sub_zero=NULL,offset=1){

  for(q in levels(as.factor(df[,group]))){
    level_df=subset(df,df[,group]=={{q}})


#  x_valid=subset(df,is.na(df[,x])==FALSE) # create a data frame of valid 'x' values
  x_valid=subset(level_df,is.na(level_df[,x])==FALSE) # create a data frame of valid 'x' values

  t_fin=level_df[nrow(level_df),t]
  r_t_fin<-which(level_df[,t]==t_fin)

  t_fin_x=x_valid[nrow(x_valid),t] # identify the 'time' of the last valid 'x' values
  r_t_fin_x<-which(level_df[,t]==t_fin_x)

  for(i in level_df[,t]){ # interpolate over 'df[,t]' (time)
    r_t_i=which(level_df[,t]=={{i}}) # get row nr of current observation
    r_t_i_df=as.numeric(rownames(level_df)[which(level_df[,t]=={{i}})]) # get row nr of current observation

    if(isFALSE(is.na(level_df[r_t_i,x]))){ # check if 'df[r_t_i_df,x]' is NA
      x_1=level_df[r_t_i,x] # if not select 'level_df[r_t_i,x]' as 'x_1'

      if(r_t_i<nrow(level_df)) r_t_n=r_t_i+1 # select the next row

      if(r_t_i<nrow(level_df)) x_n=level_df[r_t_n,x] # get 'x' of the next row

      if(isFALSE(r_t_i>=r_t_fin_x)){ # check if 'r_t_i' is 'r_t_fin_x'

        while(isTRUE(is.na(x_n))){ # go to the next row if 'r_x_n' is NA
          r_t_n=r_t_n+1
          x_n=level_df[r_t_n,x]
        }

        if(isTRUE((r_t_i+1)!=r_t_n)){ # check if a gap between 't_i' and 't_n' exists

          m=x_n-x_1 # calculate slope for linear interpolation
          t_1=level_df[r_t_i,t]
          t_n=level_df[r_t_n,t]

          for (j in c((r_t_i+1):(r_t_n-1))) {
            t_2=level_df[j,t]

            level_df[j,x]=((t_2-t_1)/(t_n-t_1))*m+level_df[r_t_i,x]
            df[as.numeric(rownames(level_df)[j]),x]=level_df[j,x]
          }

        }}else{
          if(isFALSE(end)){
            if(r_t_i<=nrow(level_df)){
              if(isTRUE(is.na(level_df[which(level_df[,t]==t_fin),x]))){
                m=x_1-level_df[(r_t_i-offset),x] # calculate slope for linear interpolation
                t_1=level_df[(r_t_i-offset),t]
                t_n=level_df[r_t_i,t]

                if(t_n!=t_fin){
                  r_t_fin=which(level_df[,t]==t_fin)

                  for (j in c((r_t_i+1):(r_t_fin))) {
                    t_2=level_df[j,t]
                    level_df[j,x]=((t_2-t_1)/(t_n-t_1))*m+level_df[r_t_i,x]
                    df[as.numeric(rownames(level_df)[j]),x]=level_df[j,x]

                  }}}}}}}}}

  if(isFALSE(is.null(sub_zero))){
    df[which(df[,x]<0),x]=sub_zero
  }

  return(df)
}
