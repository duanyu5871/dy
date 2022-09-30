#' @importFrom stringr str_trim
#' @importFrom stringr str_length
#' @importFrom stringr str_extract
#' @importFrom stringr str_split_fixed
#' @export
dy_as_numeric<-function(x,allowN=1,allowBeginZero=T,...) {
  UseMethod("dy_as_numeric")
}

#' @export
dy_as_numeric.character<-function(x,allowN=1,allowBeginZero=T,...) {
  x<-str_trim(x)
  n_before<-sum(is.na(x))
  n_after<-sum(is.na(as.numeric(x)))
  if (n_before==n_after) {
    number_x<-as.numeric(x)
    if (all(x==number_x, na.rm =T)) {
      x<-number_x
      print(paste("已转换"))
    }
  } else {
    x<-dy_string_to_numeric(x,allowN,...)  ### 保守方案，仅允许出现最多一种前缀或后缀
  }
  return(x)
}

#' @export
dy_as_numeric.matrix<-function(x,allowN=1,allowBeginZero=T,...){
  dy_as_numeric.default(x,...)
}

#' @export
dy_as_numeric.data.frame<-function(x,allowN=1,allowBeginZero=T,...){
  dy_as_numeric.default(x,...)
}

#' @export
dy_as_numeric.default<-function(x,allowN=1,allowBeginZero=T,...){
  if (("tbl_df") %in% class(x)) {
    warning("A tibble was given, which influnce string regulation, and it will be transformed into data.frame, you can transform it back later by youself",immediate. = T)
  }
  x<-as.data.frame(x)
  old_opt<-getOption("warn")
  options(warn =-1)
  for (i in 1:ncol(x)){
    #if (all(x[,i]==as.numeric(x[,i]),na.rm = T)) x[,i]<-as.numeric(x[,i])
    if (is.numeric(x[,i])) {
      next()
    }
    x[,i]<-str_trim(x[,i])
    n_before<-sum(is.na(x[,i]))
    n_after<-sum(is.na(as.numeric(x[,i])))
    if (n_before==n_after) {
      number_x<-as.numeric(x[,i])
      if (all(x[,i]==number_x, na.rm =T)) {
        x[,i]<-number_x
        print(paste("已转换: i=",i,",",names(x)[i]))
      }
    } else {
      x[,i]<-dy_string_to_numeric(x[,i],allowN,i,names(x)[i],...)  ### 保守方案，仅允许出现最多一种前缀或后缀
    }
  }
  options(warn=old_opt)
  return(x)
}
