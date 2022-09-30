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
dy_as_numeric.double<-function(x,allowN=1,allowBeginZero=T,...){
  print("你想干嘛？")
  return(x)
}

#' @export
dy_as_numeric.integer<-function(x,allowN=1,allowBeginZero=T,...){
  print("你想干嘛？")
  return(x)
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

dy_string_to_numeric<-function(x,allowN=1,varIndex=NULL,varName=NULL,allowBeginZero=T){
  ### 字符串转数值，允许出现少量同质的前后缀笔误
  x<-str_trim(x)  ### 去除前后空格
  lenString<-str_length(x)   ### 原字符串长度
  strNumber<-str_extract(x,"[0-9]{1,}[.]?[0-9]*")  ### 提取其中符合数值的部分
  lenNumber<-ifelse(is.na(str_length(strNumber)),0,str_length(strNumber))   ### 数值字符串长度
  lenPro<-lenNumber/lenString  ### 数值字符串占原字符串长度比
  if (sum(lenPro>=0.5)>length(x)*0.8) {  ### 占比至少50%的元素超过80%
    restString<-str_split_fixed(x,strNumber,2)  ### 提取数值字符串后剩余部分
    restBeginStr<-names(table(restString[,1]))  ### 剩余前方 出现的类型
    restEndStr<-names(table(restString[,2]))    ### 剩余后方 出现的类型

    if (length(restBeginStr)<=allowN+1 & length(restEndStr)<=allowN+1) {
      if (allowBeginZero) {
        strZero<-substr(strNumber,0,2) ### 数字开头为0
        if (any(strZero=="00")){
          x<-strNumber
        } else {
          x<-as.numeric(strNumber)
        }
      } else {
        x<-as.numeric(strNumber)
      }
      flagTrans<-"已转换"
    } else {
      flagTrans<-"未转换"
    }
    print(paste(paste0(flagTrans,":"),
                ifelse(is.null(varIndex),"",paste("i=",varIndex,",")),
                ifelse(is.null(varName),"",varName),
                "It might be number"))
    print(c("begin:",restBeginStr))
    print(c("end:",restEndStr))
  }
  return(x)
}
