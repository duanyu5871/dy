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
