partner_data <- function(data, identifier, couple_identifier, merge = T){
  require(dplyr)
k = 1
  for(i in unique(data[,identifier])){
    
    mydata = data
    ID = i
    COUP = mydata[mydata[,identifier] == i, couple_identifier] 
    
    df <- mydata[mydata[,couple_identifier] == COUP & mydata[,identifier] != ID, ]
    df$Couple_ID <- NULL  
    
    # remove people with no partner data
    if(nrow(df) > 0){
      df[, identifier] <- ID
      
      colnames(df) <- paste(colnames(df), "P", sep = "_")
      colnames(df)[colnames(df) == paste(identifier, "_P", sep ="")] =identifier
      
      if(k == 1){partner_df = df}
      if(k > 1){partner_df = rbind(partner_df, df)}
    }
    k = k+1
  }
  
  
  if(merge == T) {
    colnames(data) <- paste(colnames(data), "A", sep = "_")
    colnames(data)[colnames(data) == paste(identifier, "_A", sep ="")] =identifier
    colnames(data)[colnames(data) == paste(couple_identifier, "_A", sep ="")] =couple_identifier
    
    partner_df <- merge(data, partner_df, by = identifier)
    
    
    partner_df <-  partner_df %>%
      relocate(couple_identifier) 
    
    
    partner_df <-  partner_df %>%
      relocate(identifier) 
  }
  return(partner_df)
  
}
