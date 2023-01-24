# find & fix choices that contain forbidden characters in name:
bugged_choice_regex <- "[/'’?]"

bugged_choices <- tool.choices %>% filter(str_detect(name, bugged_choice_regex)) %>%  
  left_join(tool.survey %>% select(q.type, list_name, datasheet)) %>% distinct()

bugged_cols <- tool.survey %>% filter(list_name %in% bugged_choices$list_name) %>%
  select(name, q.type, datasheet, list_name)

if(nrow(bugged_cols) > 0){
  tool.choices <- tool.choices %>% 
    mutate(name = str_replace_all(name, bugged_choice_regex, "_"))
  # fix select_one choices:
  bugged_s_one <- bugged_cols %>% filter(q.type == "select_one")
  if(nrow(bugged_s_one) > 0){
    for(i in 1:nrow(bugged_s_one)){
      dsheet <- bugged_s_one[i,]$datasheet
      col <- bugged_s_one[i,]$name
      data.list[[dsheet]][[col]] <- str_replace_all(data.list[[dsheet]][[col]], bugged_choice_regex, "_")
    }
    cat("\n> Fixed",nrow(bugged_s_one),"bugged select_one choices.")
  }
  # fix select_multiple choice columns:
  bugged_s_multiple <- bugged_choices %>% filter(q.type == "select_multiple")
  if(nrow(bugged_s_multiple) > 0){
    for(i in 1:nrow(bugged_s_multiple)){
      dsheet <- bugged_s_multiple[i,]$datasheet
      choice <- bugged_s_multiple[i,]$name
      choice_rep <- str_replace(choice, bugged_choice_regex, "_")
      colnames(data.list[[dsheet]]) <- str_replace_all(colnames(data.list[[dsheet]]), choice, choice_rep)
    }
    cat("\n> Fixed",nrow(bugged_s_multiple),"bugged select_multiple choices.")
  }
}

# find & fix columns (variables) that contain forbidden characters:
bugged_varname_regex <- "[-'?!`]"

bugged_vars <- tool.survey %>% filter(str_detect(name, bugged_varname_regex)) %>% select(name, q.type, datasheet, list_name)

if(nrow(bugged_vars) > 0){
  for (sheet in names(data.list)) names(data.list[[sheet]]) <- str_replace_all(names(data.list[[sheet]]), bugged_varname_regex, "_")
  tool.survey$name <- str_replace_all(tool.survey$name, bugged_varname_regex, "_")
  daf$variable <- str_replace_all(daf$variable, bugged_varname_regex, "_")
  cat("\n> Fixed",nrow(bugged_vars),"bugged variable names.")
}

# make all choice names shorter (up to 50 chars) - THIS MAY BE DANGEROUS AFTER ALL
# tool.choices$name = str_sub(tool.choices$name, 0, 50)

# for(sheet in names(data.list)){
#   cnames <- colnames(data.list[[sheet]])
#   colnames(data.list[[sheet]]) <- ifelse(str_detect(cnames, "/"), 
#                                          str_sub(cnames, 0, str_locate(cnames, "/") + 50), cnames)
# }
