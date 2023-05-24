partner_data <- function(data, identifier, couple_identifier){
  require(dplyr)
  for(i in 1:nrow(mydata)){
    
    mydata = data
    ID = mydata[i, identifier]  
    COUP = mydata[i, couple_identifier] 
    
    df <- mydata[mydata[,couple_identifier] == COUP & mydata[,identifier] != ID, ]
    df$Couple_ID <- NULL  
    df[, identifier] <- ID
    
    colnames(df) <- paste(colnames(df), "P", sep = "_")
    colnames(df)[colnames(df) == paste(identifier, "_P", sep ="")] =identifier
    
    if(i == 1){partner_df = df}
    if(i > 1){partner_df = rbind(partner_df, df)}
    
  }
  
  colnames(data) <- paste(colnames(data), "A", sep = "_")
  colnames(data)[colnames(data) == paste(identifier, "_A", sep ="")] =identifier
  colnames(data)[colnames(data) == paste(couple_identifier, "_A", sep ="")] =couple_identifier
  
  partner_df <- merge(data, partner_df, by = identifier)
  
  
  partner_df <-  partner_df %>%
    relocate(couple_identifier) 
  
  
  partner_df <-  partner_df %>%
    relocate(identifier) 
  
  return(partner_df)
  
}
