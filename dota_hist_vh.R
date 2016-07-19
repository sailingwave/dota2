library(jsonlite)
library(RCurl)
library(dplyr)

steam_key = "0222C681FD70D34ACE47390BEB57B255"
matches_requested = 100

load(file="vh_list.RData")

cmd <- paste0("https://api.steampowered.com/IDOTA2Match_570/GetMatchHistory/v001/?key=",steam_key,
                     "&matches_requested=",matches_requested,"&skill=",3)

success=0
while(TRUE){
    tmp = try(fromJSON(getURL(cmd)))
    success = ifelse(class(tmp)=='try-error',0,1)
    if(success==1) break
    Sys.sleep(5)
}

#select(tmp$result$matches,-players)

#vh_list = data.frame()

vh_list = rbind(vh_list,select(tmp$result$matches,match_id,match_seq_num))
vh_list = unique(vh_list)

save(vh_list,file="vh_list.RData")
