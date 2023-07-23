#check blanks
check_blanks<- function(data){
  apply(data, 1,function(x) {length(which(is.na(x)))})/ncol(data)*100
}

#compare the cleaning log and the raw dataset
old_new <- function(data_raw, 
                    data_clean, 
                    cleaning_log, ### has to be 1 line, the cleaning log has to be splited
                    variable,
                    uuid_raw,
                    uuid_clean,
                    uuid_cleaning_log) {
  if(cleaning_log[[variable]] %in% names(data_clean) & 
     cleaning_log[[uuid_cleaning_log]] %in% data_clean[[uuid_clean]]){
    row_raw <- which(data_raw[[uuid_raw]] == cleaning_log[[uuid_cleaning_log]])
    value_raw <- if(cleaning_log[[variable]] %in% names(data_raw)) {
      data_raw[row_raw, cleaning_log[[variable]]]
    } else {
      "new column created"
    }
      
    row_clean <- which(data_clean[[uuid_clean]] == cleaning_log[[uuid_cleaning_log]])
    value_clean <- data_clean[row_clean, cleaning_log[[variable]]]
    return_value <- data.frame(value_raw,
                               value_clean, 
                               binding = paste0( cleaning_log[[uuid_cleaning_log]], "-/-", cleaning_log[[variable]]))
    names(return_value) <- c("value_raw", "value_clean", "binding")
  } else {
    return_value <- data.frame(value_raw = NULL,
                               value_clean = NULL,
                               binding = NULL)
  }
  
  return(return_value)     
  
}

#small function utils
type_convert_ch_silent <- function(x) bind_cols(lapply(x,as.character))

#compare the clean and raw dataset
compare_datasets<-function(
  draw, # dataset
  uuid_r, # uuid column name in dataset
  dclean, # cleaning log data
  uuid_c # uuid column name in log
){  
  draw<-draw %>% as.data.frame %>% type_convert_ch_silent()
  dclean<-dclean %>% as.data.frame %>% type_convert_ch_silent()
  
  # remove caps from headings
  draw$uuid<-tolower(draw[[uuid_r]])
  dclean$uuid<-tolower(dclean[[uuid_c]])
  names(draw)<-tolower(names(draw))
  names(dclean)<-tolower(names(dclean))
  
  log<-data.frame(
    uuid=c(),
    question.name=c(),
    old.value=c(),
    new.value=c()
  )
  
  #no uuid match
  nomatchindex<-which(!draw$uuid%in%dclean$uuid)
  if(length(nomatchindex)>0){
    no_match<-data.frame(
      uuid=dclean$uuid[nomatchindex],
      question.name="all",
      comments="no matching uuid in dataset")
  } else {no_match<-log}
  #no var names march
  
  varremovedindex<-which(!names(draw)%in% names(dclean))
  if(length(varremovedindex)>0){
    varremoved<-data.frame(
      uuid="all",
      question.name=names(draw)[varremovedindex],
      comments="variable removed from the clean dataset"
    )} else {varremoved<-log} 
  
  varaddedindex<-which(!names(dclean)%in% names(draw))
  if(length(varremovedindex)>0){
    varadded<-data.frame(
      uuid="all",
      question.name=names(draw)[varaddedindex],
      comments="variable added to the clean dataset"
    )} else {varadded<-log}
  
  
  log<-bind_rows(log,no_match,varremoved,varadded)  
  
  uuidlist<-dclean$uuid[which(draw$uuid%in%dclean$uuid)]
  varlist<-names(draw)[names(draw)%in% names(dclean)]
  varlist<-varlist[which(varlist!="uuid")]
  
  compdata<-lapply(varlist,function(x,dclean,draw){
    check<-merge(dclean[c("uuid",x)],draw[c("uuid",x)],by="uuid", all.x=T)
    index<-which(check[,2]!=check[,3])
    if(length(index)!=0){
      check<-check[index,]
      names(check)<-c("uuid","new.value","old.value")
      check$question.name<-x
      check$comments<-"change was made"
      return(check)
    }
    message(x)
  },dclean=dclean, draw=draw) %>% do.call(rbind,.)
  
  
  log<-bind_rows(log,compdata)
  return(log)  
}

