# 3 inclusion criteria
# - game mode in 'all pick', 'ranked all pick', 'captain mode'
# - no leavers
# - skill bracket is very high

setwd("E:/dota_data/")

library(XML)
library(RCurl)
library(jsonlite)
library(dplyr)
library(RSQLite)
library(data.table)

#a list of very high skill matches
load("D:/Dropbox/MachineLearning/dota/vh_list.RData")

dt_vh = as.data.table(vh_list) %>% arrange(match_seq_num)

load('dota_6.86f.RData')

steam_key=readRDS(file="E:/dota_data/steam_key.rds")

#options
matches_requested=1    #maximum is 100
#curr_seq <- start_at_match_seq_num <- "1914132585"    #from head(sort(vh_list$match_seq_num))
end_seq_num <- "1926009541"    #from max(vh_list$match_seq_num)


#initialize
cmd_hist_0 <- paste0("https://api.steampowered.com/IDOTA2Match_570/GetMatchHistoryBySequenceNum/V001/?key=",steam_key,
                     "&matches_requested=",matches_requested)

cmd_hist_1 <- paste0(cmd_hist_0,"&start_at_match_seq_num=",curr_seq)


#bear_item <- captain <- banpick <- ability_up <- player <- match <- data.frame()
#team_info <- list()


#i=802


while(curr_seq<=end_seq_num){
    #Sys.sleep(1)
    
    cat(paste0("Processing seq ",i,", now is ",Sys.time(),", seqNum is ", curr_seq," \n"))
    
    j <- try_success <- 0
    
    while(TRUE){
        api_res_txt = getURL(cmd_hist_1)
        api_res = try(fromJSON(api_res_txt))
        try_success = ifelse(class(api_res)=="try-error",0,1)
        print(j)
        j = j+1
        
        if(try_success!=0 | j > 5) break
        Sys.sleep(3)
    }
    
    
    
    if(api_res$result$status != 1){
        stop("The API did not return results!")
    }
    
    
    #game mode
    gm_idx = which(api_res$result$matches$game_mode %in% c(1,2,22))    #All Pick, Captains Mode, Ranked All Pick
    
    #leaver, duration > 10min
    if(length(gm_idx)==0){
        print("Wrong game mode")
        i=i+1
        curr_seq = dt_vh$match_seq_num[i]
        cmd_hist_1 = paste0(cmd_hist_0,"&start_at_match_seq_num=",curr_seq)
        next
    }
    
    noleaver_idx = gm_idx[which(sapply(api_res$result$matches$players[gm_idx],function(x) sum(x$leaver_status)==0))]    #no leavers
    duration_idx = gm_idx[which(api_res$result$matches$duration[gm_idx]>600)]
    
    noleaver_idx = intersect(noleaver_idx,duration_idx)
    
    #record
    
    if(length(noleaver_idx)==0){
        print("Leavers or too short")
        i=i+1
        curr_seq = dt_vh$match_seq_num[i]
        cmd_hist_1 = paste0(cmd_hist_0,"&start_at_match_seq_num=",curr_seq)
        next
    }
    
    info_match = select(api_res$result$matches,-players) 
    
    #ability upgrade
    all_ability_upgrade_idx = noleaver_idx[which(sapply(api_res$result$matches$players[noleaver_idx],function(x) !any(sapply(x$ability_upgrades,is.null))))]
    
    if(length(all_ability_upgrade_idx)==0){
        print("Not all had upgraded")
        i=i+1
        curr_seq = dt_vh$match_seq_num[i]
        cmd_hist_1 = paste0(cmd_hist_0,"&start_at_match_seq_num=",curr_seq)
        next
    }
    
    #spirit bear
    #info_addunit = NULL
    player_colname = lapply(api_res$result$matches$players,names)
    addunit_idx = which(lapply(player_colname,length)==25)
    
    if(length(addunit_idx) != 0){
        info_addunit = do.call(rbind,mapply(function(match_id,y) cbind(match_id,y),info_match$match_id[addunit_idx],api_res$result$matches$players[addunit_idx],
                                            SIMPLIFY = F)) %>%
            filter(additional_units!="NULL",hero_id==80)    #druid
        
        if(dim(info_addunit)[1]>0){
            info_addunit = do.call(rbind,mapply(function(match_id,account_id,y) cbind(match_id,account_id,y),info_addunit$match_id,info_addunit$account_id,
                                                info_addunit$additional_units,SIMPLIFY = F))
            bear_item = rbind(bear_item,info_addunit)
        }
    }
    
    #other info
    info_player = do.call(rbind,mapply(function(match_id,y) cbind(match_id,y),info_match$match_id,api_res$result$matches$players,SIMPLIFY = F) %>%
                              lapply(function(x) x[,1:25]))
    info_ability_up = do.call(rbind,mapply(function(match_id,player_slot,y) cbind(match_id,player_slot,y),info_player[,1],info_player[,3],info_player$ability_upgrades,SIMPLIFY = F))
    info_player = select(info_player,-ability_upgrades)
    
    #ban pick info (only for cm mode)
    #info_captain <- info_banpick <- NULL 
    if("picks_bans" %in% names(info_match)){
        
        bp_idx = which(info_match$picks_bans!="NULL")
        
        if(length(bp_idx)!=0){
            info_captain = info_match[bp_idx,c("radiant_captain","dire_captain")] 
            info_banpick = do.call(rbind,mapply(function(match_id,y) cbind(match_id,y),info_match$match_id[bp_idx],info_match$picks_bans[bp_idx],SIMPLIFY = F))
            
            banpick = rbind(banpick,info_banpick)
            captain = rbind(captain,info_captain)
        }
        
        info_match = select(info_match,-radiant_captain,-dire_captain,-picks_bans)
    }
    
    if("dire_team_id" %in% names(info_match) | 'radiant_team_id' %in% names(info_match)){
        
        team_idx1 = which(!is.na(info_match$dire_team_id))
        team_idx2 = which(!is.na(info_match$radiant_team_id))
        
        if(length(team_idx1)!=0){
            team_info = c(team_info,list(info_match[team_idx1,]))
        }
        
        if(length(team_idx2)!=0){
            team_info = c(team_info,list(info_match[team_idx2,]))
        }
        
        info_match = info_match[,1:19]
    }
    
    if('radiant_score' %in% names(info_match)){
        if(info_match$radiant_score != 0){
            stop('score nonzero!')
        }else{
            info_match = info_match[,1:19]
        }
    }
    
    #combine
    match = rbind(match,info_match)
    player = rbind(player,info_player)
    ability_up = rbind(ability_up,info_ability_up)
    
    
    #next iteration
    i=i+1
    curr_seq = dt_vh$match_seq_num[i]
    cmd_hist_1 = paste0(cmd_hist_0,"&start_at_match_seq_num=",curr_seq)
    
    save(match,player,ability_up,bear_item,banpick,captain,team_info,curr_seq,i,file="dota_6.86f.RData")
}


match = unique(match)
ability_up = unique(ability_up)
player = unique(player)
bear_item = unique(bear_item)
banpick = unique(banpick)
captain = unique(captain)


#save to database

db = dbConnect(SQLite(), "dota_6.86f.sqlite3")

dbWriteTable(db,'match',match,row.names = FALSE,overwrite=TRUE)
dbWriteTable(db,'player',player,row.names = FALSE,overwrite=TRUE)
dbWriteTable(db,'ability_up',ability_up,row.names = FALSE,overwrite=TRUE)
dbWriteTable(db,'bear_item',bear_item,row.names = FALSE,overwrite=TRUE)
dbWriteTable(db,'banpick',banpick,row.names = FALSE,overwrite=TRUE)
dbWriteTable(db,'captain',captain,row.names = FALSE,overwrite=TRUE)

# match = dbReadTable(db,'match')
# player = dbReadTable(db,'player')
# ability_up = dbReadTable(db,'ability_up')
# bear_item = dbReadTable(db,'bear_item')

dbDisconnect(db)
