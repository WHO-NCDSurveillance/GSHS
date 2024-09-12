
####Detailed tables#
detailed_tab_fn = function(std_var ='DB_HUNGRY')
{
  formula = make.formula(std_var)

  cond_output = function(condition =  "age_cat=='12 or younger'")
  {
    if (condition =='')
    {
      tab_output =  data %>%
        dplyr::reframe(label_names = names(table(eval(parse(text = std_var)))),
                       unweighted_freq = table(eval(parse(text = std_var))),
                       weighted_perc = formatC(round(prop.table(svytable(formula,design = svy_data))*100,1),format = 'f', digits = 1)) %>%
        mutate(unweighted_freq = as.character(unweighted_freq)) %>%
        tidyr::pivot_longer(cols=c(weighted_perc,unweighted_freq)) %>% select(-name) %>%
        mutate(label_names = as.character(label_names), value = as.character(value)) %>% as.data.frame()%>%
        bind_rows(data.frame(label_names = eval(parse(text=lang_titles[4])), value ='100%'))%>%
        bind_rows(data.frame(label_names = eval(parse(text=lang_titles[4])), value = eval(parse(text=paste0('sum(table(data$',std_var,'), na.rm = T)'))))%>%
                    mutate(label_names = as.character(label_names), value = as.character(value)))
    }
    else
    {
      tab_output =  eval(parse(text=paste0('data %>% dplyr::filter (', condition,')')))%>%
        dplyr::reframe(label_names = names(table(eval(parse(text = std_var)))),
                       unweighted_freq = table(eval(parse(text = std_var))),
                       weighted_perc = formatC(round(prop.table(svytable(formula,design = subset(svy_data,eval(parse(text=condition)))))*100,1),format = 'f', digits = 1)) %>%
        mutate(unweighted_freq = as.character(unweighted_freq)) %>%
        tidyr::pivot_longer(cols=c(weighted_perc,unweighted_freq)) %>% select(-name)%>%
        mutate(label_names = as.character(label_names), value = as.character(value)) %>% as.data.frame()%>%
        bind_rows(data.frame(label_names = eval(parse(text=lang_titles[4])), value ='100%'))%>%
        bind_rows(data.frame(label_names = eval(parse(text=lang_titles[4])), eval(parse(text=paste0('data %>% dplyr::filter (', condition,')'))) %>%
                               reframe(eval(parse(text = paste0('sum(table(',std_var,'),na.rm=T)'))))) %>%
                    rename(value ='eval.parse.text...paste0..sum.table....std_var.....na.rm.T.....') %>%
                    mutate(label_names = as.character(label_names), value = as.character(value)))
    }
    ###Formatting and restricting outputs where subgroup >=100
    tab_output$value[seq(1,nrow(tab_output),2)][as.numeric(tab_output$value[nrow(tab_output)])<= n_cutoff]=' - '

    return(tab_output)
  }

  ###Calling cond_output function::Total
  total_conditions = c('',"age_cat=='12 or younger'", "age_cat=='13 - 15'","age_cat=='16 or 17'",
                       "age_cat=='13 - 15'|age_cat=='16 or 17'", "age_cat=='18 or older'",
                       paste0("DE_GRADE =='", levels(data$DE_GRADE),"'"))
  eval(parse(text=paste0('total_res = cbind(',paste0('cond_output("',total_conditions,'")[,2]', collapse = ','),')')))
  #
  mod_total_res = rbind(c(eval(parse(text=lang_titles[4])),'','',eval(parse(text=lang_titles[20])),'','','','',eval(parse(text=lang_titles[11])),rep('', length(levels(data$DE_GRADE))-1)),
                          c('','',eval(parse(text=lang_titles[4])),eval(parse(text=lang_titles[6])),'13-15','16-17','13-17',eval(parse(text=lang_titles[9])), gsub('*.        ','',levels(data$DE_GRADE))),
                          cond_output(total_conditions[1])[,1] %>%bind_cols(rep(c('%','N'),nrow(total_res)/2),total_res))
  #mod_total_res = mod_total_res[,-c(15)]
  ###
  
  ###
  if(language =='FRENCH')
  {
    mod_total_res = eval(parse(text=paste0('cbind(',paste0(gsub('\\.',',',mod_total_res), collapse =','),')'))) %>% as.data.frame()
  }else{}
  
  flex_total_res =  mod_total_res %>% flextable() %>% autofit() %>%
    delete_part(part = "header") %>% 
    add_header_lines(paste0(std_var,': ',label(eval(parse(text=paste0('data$',std_var)))),'\n'))%>%
    flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
    bold(i = 1:2)%>%
    bg(bg="white",i=1,part="header")%>%
    hline_top(border = fp_border_default(width = 0),part = "header")%>%
    vline_left(i=1,border = fp_border_default(width = 0),part = "header")%>%
    vline(i=1,border = fp_border_default(width = 0),part = "header")%>%
    fontsize(size = 8 ,part = "all") %>%
    merge_v(j = 1) %>% valign(valign = 'top')%>%
    align(j = 3:ncol(mod_total_res),align = 'right', part = 'all') %>%
    width(j = 1, 3, unit = "in") %>%
    line_spacing(i = 3:nrow(mod_total_res), space = 0.7, part = "body") %>%
    fix_border_issues()%>% width(j = 4, 1, unit = "in")

  ##Males
  male_conditions = c(paste0('DE_SEX == ',"'", levels(data$DE_SEX)[1],"'"),
                      paste0('DE_SEX == ',"'", levels(data$DE_SEX)[1],"' & age_cat=='12 or younger'"),
                      paste0('DE_SEX == ',"'", levels(data$DE_SEX)[1],"' & age_cat=='13 - 15'"),
                      paste0('DE_SEX == ',"'", levels(data$DE_SEX)[1],"' & age_cat=='16 or 17'"),
                      paste0('DE_SEX == ',"'", levels(data$DE_SEX)[1],"' & (age_cat=='13 - 15'|age_cat=='16 or 17')"),
                      paste0('DE_SEX == ',"'", levels(data$DE_SEX)[1],"' & age_cat=='18 or older'"),
                      paste0(paste0('DE_SEX == ',"'", levels(data$DE_SEX)[1],"' & "), "DE_GRADE =='", levels(data$DE_GRADE),"'"))

  eval(parse(text=paste0('male_res = cbind(',paste0('cond_output("',male_conditions,'")[,2]', collapse = ','),')')))
  #
  mod_male_res = rbind(c(eval(parse(text=lang_titles[12])),'','',eval(parse(text=lang_titles[20])),'','','','',
                         eval(parse(text=lang_titles[11])),rep('', length(levels(data$DE_GRADE))-1)),
                       c(eval(parse(text=lang_titles[16])), gsub('*.        ','',levels(data$DE_GRADE))),
                       cond_output(male_conditions[1])[,1] %>%
                         bind_cols(rep(c('%','N'),nrow(male_res)/2),male_res))
 
  ####
  #mod_male_res = mod_male_res[,-c(15)]
  
  ###
  if(language =='FRENCH')
  {
    mod_male_res = eval(parse(text=paste0('cbind(',paste0(gsub('\\.',',',mod_male_res), collapse =','),')'))) %>% as.data.frame()
  }else{}
  
  

  flex_male_res = mod_male_res %>% flextable() %>% autofit() %>%
    delete_part(part = "header") %>% 
    add_header_lines(paste0(std_var,': ',label(eval(parse(text=paste0('data$',std_var)))),'\n'))%>%
    flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
    bold(i = 1:2)%>%
    bg(bg="white",i=1,part="header")%>%
    hline_top(border = fp_border_default(width = 0),part = "header")%>%
    vline_left(i=1,border = fp_border_default(width = 0),part = "header")%>%
    vline(i=1,border = fp_border_default(width = 0),part = "header")%>%
    fontsize(size = 8 ,part = "all") %>%
    merge_v(j = 1) %>% valign(valign = 'top')%>%
    align(j = 3:ncol(mod_male_res),align = 'right', part = 'all') %>%
    width(j = 1, 3, unit = "in") %>%
    line_spacing(i = 3:nrow(mod_male_res), space = 0.7, part = "body") %>%
    fix_border_issues()%>% width(j = 4, 1, unit = "in")

  ###Females
  female_conditions = c(paste0('DE_SEX == ',"'", levels(data$DE_SEX)[2],"'"),
                        paste0('DE_SEX == ',"'", levels(data$DE_SEX)[2],"' & age_cat=='12 or younger'"),
                        paste0('DE_SEX == ',"'", levels(data$DE_SEX)[2],"' & age_cat=='13 - 15'"),
                        paste0('DE_SEX == ',"'", levels(data$DE_SEX)[2],"' & age_cat=='16 or 17'"),
                        paste0('DE_SEX == ',"'", levels(data$DE_SEX)[2],"' & (age_cat=='13 - 15'|age_cat=='16 or 17')"),
                        paste0('DE_SEX == ',"'", levels(data$DE_SEX)[2],"' & age_cat=='18 or older'"),
                        paste0(paste0('DE_SEX == ',"'", levels(data$DE_SEX)[2],"' & "), "DE_GRADE =='", levels(data$DE_GRADE),"'"))

  eval(parse(text=paste0('female_res = cbind(',paste0('cond_output("',female_conditions,'")[,2]', collapse = ','),')')))
  #
  
  mod_female_res = rbind(c(eval(parse(text=lang_titles[13])),'','',eval(parse(text=lang_titles[20])),'','','','',eval(parse(text=lang_titles[11])),rep('', length(levels(data$DE_GRADE))-1)),
                         c(eval(parse(text=lang_titles[17])), gsub('*.        ','',levels(data$DE_GRADE))),
                         cond_output(female_conditions[1])[,1] %>%
                           bind_cols(rep(c('%','N'),nrow(female_res)/2),female_res))
  
  ##
  #mod_female_res = mod_female_res[,-c(15)]
  
  ####
  if(language =='FRENCH')
  {
    mod_female_res = eval(parse(text=paste0('cbind(',paste0(gsub('\\.',',',mod_female_res), collapse =','),')'))) %>% as.data.frame()
  }else{}
  
##

  flex_female_res = mod_female_res %>% flextable() %>% autofit() %>%
    delete_part(part = "header") %>% 
    add_header_lines(paste0(std_var,': ',label(eval(parse(text=paste0('data$',std_var)))),'\n'))%>%
    flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
    bold(i = 1:2)%>%
    bg(bg="white",i=1,part="header")%>%
    hline_top(border = fp_border_default(width = 0),part = "header")%>%
    vline_left(i=1,border = fp_border_default(width = 0),part = "header")%>%
    vline(i=1,border = fp_border_default(width = 0),part = "header")%>%
    fontsize(size = 8 ,part = "all") %>%
    merge_v(j = 1) %>% valign(valign = 'top')%>%
    width(j = 1, 3, unit = "in") %>%
    align(j = 3:ncol(mod_female_res),align = 'right', part = 'all') %>%
    #merge_h_range(j1=9, j2=10)%>%align(j = 9, align = 'left') %>%
    line_spacing(i = 3:nrow(mod_female_res), space = 0.7, part = "body") %>%
    fix_border_issues()%>% width(j = 4, 1, unit = "in")#%>% width(j = 3, 1, unit = "in")

  ###All flextable outputs
  eval(parse(text=paste0(std_var,'_flex_detailed = list(flex_total_res,flex_male_res,flex_female_res)')))
}

###calling detailed_tab_fn
#
vars_for_detailed_tables = setdiff(standard_variables, c("DE_AGE","DE_SEX","DE_GRADE","DB_HEIGHT","DB_WEIGHT"))
#
detailed_tables = parallel::mclapply(vars_for_detailed_tables, detailed_tab_fn, mc.cores = num_cores, mc.preschedule = FALSE)
#detailed_tables = future.apply::future_lapply(vars_for_detailed_tables, detailed_tab_fn,future.seed=TRUE)

final_detailed_tables = unlist(detailed_tables, recursive = FALSE)
##################
#################
##############
# Function to add a Flextable with a break at the end
i = NULL
for (i in 1:length(final_detailed_tables)) {
  my_doc = read_docx(paste0(getwd(),'/templates/detailed_tables_template.docx'))
  if (i < length(final_detailed_tables))
  {
    my_doc = my_doc %>%
      body_add_flextable(width(final_detailed_tables[[i]], width = dim(final_detailed_tables[[i]])$widths*10/(flextable_dim(final_detailed_tables[[i]])$widths)),pos = 'on')%>% 
      body_add_break()  
  }
  else
  {
    my_doc = my_doc %>%
      body_add_flextable(width(final_detailed_tables[[i]], width = dim(final_detailed_tables[[i]])$widths*10/(flextable_dim(final_detailed_tables[[i]])$widths)),pos = 'on') 
  }
  #
  print(my_doc,target=paste0(getwd(),'/temp_tables/tempdetailed',i,'.docx')) 
}

combined_detailed_doc = read_docx(paste0(getwd(),'/templates/detailed_tables_template.docx'))
combined_detailed_doc = headers_replace_text_at_bkm(combined_detailed_doc,"country",site_name)
combined_detailed_doc = headers_replace_text_at_bkm(combined_detailed_doc,"year",paste0(lang_titles[21],' ',survey_year))
combined_detailed_doc = headers_replace_text_at_bkm(combined_detailed_doc,"Detailed",lang_titles[24])
combined_detailed_doc = footers_replace_text_at_bkm(combined_detailed_doc,"ft_bmk1",ft_text1)
combined_detailed_doc = footers_replace_text_at_bkm(combined_detailed_doc,"ft_bmk2",ft_text2)

i=NULL
for(i in rev(1:length(final_detailed_tables))){
  path <- paste0(getwd(),'/temp_tables/tempdetailed',i,'.docx')
  detaileddoc <- body_add_docx(combined_detailed_doc, path, pos = "after") 
}

# print combine doc
print(detaileddoc,target=paste0(getwd(),'/reports/',survey_year,' ' ,site_name,' Detailed Tables.docx')) 






##################################################################################################################
##################################################################################################################
##################################################################################################################
###First function
#detailed_tab_fn = function(std_var ='DB_HUNGRY')
# {
#   formula = make.formula(std_var)
#   
#   cond_output = function(condition =  "age_cat=='12 or younger'")
#   {
#     if (condition =='')
#     {
#       tab_output =  data %>%
#         dplyr::reframe(label_names = names(table(eval(parse(text = std_var)))),
#                        weighted_freq = round(svytable(formula,design = svy_data)),
#                        weighted_perc = formatC(round(prop.table(svytable(formula,design = svy_data))*100,1),format = 'f', digits = 1)) %>%
#         mutate(weighted_freq = as.character(weighted_freq)) %>%
#         tidyr::pivot_longer(cols=c(weighted_perc,weighted_freq)) %>% select(-name) %>% 
#         mutate(label_names = as.character(label_names), value = as.character(value)) %>% as.data.frame()%>%
#         bind_rows(data.frame(label_names = 'Total', value ='100%'))%>%
#         bind_rows(data.frame(label_names = 'Total', value = sum(round(svytable(formula,design = svy_data))))%>%
#                     mutate(label_names = as.character(label_names), value = as.character(value)))
#     }
#     else
#     {
#       tab_output =  eval(parse(text=paste0('data %>% dplyr::filter (', condition,')')))%>%
#         dplyr::reframe(label_names = names(table(eval(parse(text = std_var)))),
#                        weighted_freq = round(svytable(formula,design = subset(svy_data,eval(parse(text=condition))))),
#                        weighted_perc = formatC(round(prop.table(svytable(formula,design = subset(svy_data,eval(parse(text=condition)))))*100,1),format = 'f', digits = 1)) %>%
#         mutate(weighted_freq = as.character(weighted_freq)) %>%
#         tidyr::pivot_longer(cols=c(weighted_perc,weighted_freq)) %>% select(-name)%>%
#         mutate(label_names = as.character(label_names), value = as.character(value)) %>% as.data.frame()%>%
#         bind_rows(data.frame(label_names = 'Total', value ='100%'))%>%
#         bind_rows(data.frame(label_names = 'Total', eval(parse(text=paste0('data %>% dplyr::filter (', condition,')'))) %>%
#                                reframe(value = sum(round(svytable(formula,design = subset(svy_data,eval(parse(text=condition)))))))) %>%
#                     mutate(label_names = as.character(label_names), value = as.character(value)))
#     }
#     ###Formatting and restricting outputs where subgroup >=100
#     tab_output$value[seq(1,nrow(tab_output),2)][as.numeric(tab_output$value[nrow(tab_output)])< 100]=' - '
#     
#     return(tab_output)
#   }
#   
#   ###Calling cond_output function::Total
#   total_conditions = c('',"age_cat=='12 or younger'", "age_cat=='13 - 15'","age_cat=='16 or 17'",
#                        "age_cat=='13 - 15'|age_cat=='16 or 17'", "age_cat=='18 or older'", 
#                        paste0("DE_GRADE =='", levels(data$DE_GRADE),"'"))
#   eval(parse(text=paste0('total_res = cbind(',paste0('cond_output("',total_conditions,'")[,2]', collapse = ','),')')))
#   #
#   mod_total_res = rbind(c('Total','','','Age','','','','','Grade',rep('', length(levels(data$DE_GRADE))-1)),
#                         c('','','Total','12 or\n younger','13-15','16-17','13-17','18 or \nolder', gsub('*.        ','',levels(data$DE_GRADE))),
#                         cond_output(total_conditions[1])[,1] %>%bind_cols(rep(c('%','N'),nrow(total_res)/2),total_res)) 
#   
#   flex_total_res =  mod_total_res %>% flextable() %>% autofit() %>%
#     delete_part(part = "header") %>% bold(i = 1:2)%>%
#     add_header_lines(paste0(std_var,': ',label(eval(parse(text=paste0('data$',std_var)))),'\n'))%>%
#     bg(bg="white",i=1,part="header")%>%  
#     hline_top(border = fp_border_default(width = 0),part = "header")%>%
#     vline_left(i=1,border = fp_border_default(width = 0),part = "header")%>%
#     vline(i=1,border = fp_border_default(width = 0),part = "header")%>%
#     fontsize(size = 8 ,part = "all") %>%
#     merge_v(j = 1) %>% valign(valign = 'top')%>%
#     align(j = 3:ncol(mod_total_res),align = 'right', part = 'all') %>%
#     line_spacing(i = 3:nrow(mod_total_res), space = 0.7, part = "body") %>%
#     fix_border_issues()%>% width(j = 4, 1, unit = "in")
#   
#   ##Males 
#   male_conditions = c(paste0('DE_SEX == ',"'", levels(data$DE_SEX)[1],"'"),
#                       paste0('DE_SEX == ',"'", levels(data$DE_SEX)[1],"' & age_cat=='12 or younger'"), 
#                       paste0('DE_SEX == ',"'", levels(data$DE_SEX)[1],"' & age_cat=='13 - 15'"),
#                       paste0('DE_SEX == ',"'", levels(data$DE_SEX)[1],"' & age_cat=='16 or 17'"),
#                       paste0('DE_SEX == ',"'", levels(data$DE_SEX)[1],"' & (age_cat=='13 - 15'|age_cat=='16 or 17')"), 
#                       paste0('DE_SEX == ',"'", levels(data$DE_SEX)[1],"' & age_cat=='18 or older'"), 
#                       paste0(paste0('DE_SEX == ',"'", levels(data$DE_SEX)[1],"' & "), "DE_GRADE =='", levels(data$DE_GRADE),"'"))
#   
#   eval(parse(text=paste0('male_res = cbind(',paste0('cond_output("',male_conditions,'")[,2]', collapse = ','),')')))
#   #
#   mod_male_res = rbind(c('Male','','','Age','','','','','Grade',rep('', length(levels(data$DE_GRADE))-1)),
#                        c('','','All males','12 or\n younger','13-15','16-17','13-17','18 or \nolder', gsub('*.        ','',levels(data$DE_GRADE))),
#                        cond_output(male_conditions[1])[,1] %>% 
#                          bind_cols(rep(c('%','N'),nrow(male_res)/2),male_res)) 
#   
#   flex_male_res = mod_male_res %>% flextable() %>% autofit() %>%
#     delete_part(part = "header") %>% bold(i = 1:2)%>%
#     add_header_lines(paste0(std_var,': ',label(eval(parse(text=paste0('data$',std_var)))),'\n'))%>%
#     bg(bg="white",i=1,part="header")%>%  
#     hline_top(border = fp_border_default(width = 0),part = "header")%>%
#     vline_left(i=1,border = fp_border_default(width = 0),part = "header")%>%
#     vline(i=1,border = fp_border_default(width = 0),part = "header")%>%
#     fontsize(size = 8 ,part = "all") %>%
#     merge_v(j = 1) %>% valign(valign = 'top')%>%
#     align(j = 3:ncol(mod_male_res),align = 'right', part = 'all') %>%
#     line_spacing(i = 3:nrow(mod_male_res), space = 0.7, part = "body") %>%
#     fix_border_issues()%>% width(j = 4, 1, unit = "in")
#   
#   ###Females
#   female_conditions = c(paste0('DE_SEX == ',"'", levels(data$DE_SEX)[2],"'"),
#                         paste0('DE_SEX == ',"'", levels(data$DE_SEX)[2],"' & age_cat=='12 or younger'"), 
#                         paste0('DE_SEX == ',"'", levels(data$DE_SEX)[2],"' & age_cat=='13 - 15'"),
#                         paste0('DE_SEX == ',"'", levels(data$DE_SEX)[2],"' & age_cat=='16 or 17'"),
#                         paste0('DE_SEX == ',"'", levels(data$DE_SEX)[2],"' & (age_cat=='13 - 15'|age_cat=='16 or 17')"), 
#                         paste0('DE_SEX == ',"'", levels(data$DE_SEX)[2],"' & age_cat=='18 or older'"), 
#                         paste0(paste0('DE_SEX == ',"'", levels(data$DE_SEX)[2],"' & "), "DE_GRADE =='", levels(data$DE_GRADE),"'"))
#   
#   eval(parse(text=paste0('female_res = cbind(',paste0('cond_output("',female_conditions,'")[,2]', collapse = ','),')')))
#   #
#   mod_female_res = rbind(c('Female','','','Age','','','','','Grade',rep('', length(levels(data$DE_GRADE))-1)),
#                          c('','','All females','12 or\n younger','13-15','16-17','13-17','18 or \nolder', gsub('*.        ','',levels(data$DE_GRADE))),
#                          cond_output(female_conditions[1])[,1] %>% 
#                            bind_cols(rep(c('%','N'),nrow(female_res)/2),female_res)) 
#   
#   flex_female_res = mod_female_res %>% flextable() %>% autofit() %>%
#     delete_part(part = "header") %>% bold(i = 1:2)%>%
#     add_header_lines(paste0(std_var,': ',label(eval(parse(text=paste0('data$',std_var)))),'\n'))%>%
#     bg(bg="white",i=1,part="header")%>%  
#     hline_top(border = fp_border_default(width = 0),part = "header")%>%
#     vline_left(i=1,border = fp_border_default(width = 0),part = "header")%>%
#     vline(i=1,border = fp_border_default(width = 0),part = "header")%>%
#     fontsize(size = 8 ,part = "all") %>%
#     merge_v(j = 1) %>% valign(valign = 'top')%>%
#     align(j = 3:ncol(mod_female_res),align = 'right', part = 'all') %>%
#     #merge_h_range(j1=9, j2=10)%>%align(j = 9, align = 'left') %>%
#     line_spacing(i = 3:nrow(mod_female_res), space = 0.7, part = "body") %>%
#     fix_border_issues()%>% width(j = 4, 1, unit = "in")#%>% width(j = 3, 1, unit = "in")
#   
#   ###All flextable outputs
#   eval(parse(text=paste0(std_var,'_flex_detailed = list(flex_total_res,flex_male_res,flex_female_res)')))
# }
######second function
# detailed_tab_fn = function(std_var ='DB_HUNGRY')
# {
#   formula = make.formula(std_var)
#   
#   cond_output = function(condition =  "age_cat=='12 or younger'")
#   {
#     if (condition =='')
#     {
#       tab_output =  data %>%
#         dplyr::reframe(label_names = names(table(eval(parse(text = std_var)))),
#                        unweighted_freq = table(eval(parse(text = std_var))),
#                        weighted_perc = formatC(round(prop.table(svytable(formula,design = svy_data))*100,1),format = 'f', digits = 1)) %>%
#         mutate(unweighted_freq = as.character(unweighted_freq)) %>%
#         tidyr::pivot_longer(cols=c(weighted_perc,unweighted_freq)) %>% select(-name) %>% 
#         mutate(label_names = as.character(label_names), value = as.character(value)) %>% as.data.frame()%>%
#         bind_rows(data.frame(label_names = 'Total', value ='100%'))%>%
#         bind_rows(data.frame(label_names = 'Total', value = eval(parse(text=paste0('sum(table(data$',std_var,'), na.rm = T)'))))%>%
#                     mutate(label_names = as.character(label_names), value = as.character(value)))
#     }
#     else
#     {
#       tab_output =  eval(parse(text=paste0('data %>% dplyr::filter (', condition,')')))%>%
#         dplyr::reframe(label_names = names(table(eval(parse(text = std_var)))),
#                        unweighted_freq = table(eval(parse(text = std_var))),
#                        weighted_perc = formatC(round(prop.table(svytable(formula,design = subset(svy_data,eval(parse(text=condition)))))*100,1),format = 'f', digits = 1)) %>%
#         mutate(unweighted_freq = as.character(unweighted_freq)) %>%
#         tidyr::pivot_longer(cols=c(weighted_perc,unweighted_freq)) %>% select(-name)%>%
#         mutate(label_names = as.character(label_names), value = as.character(value)) %>% as.data.frame()%>%
#         bind_rows(data.frame(label_names = 'Total', value ='100%'))%>%
#         bind_rows(data.frame(label_names = 'Total', eval(parse(text=paste0('data %>% dplyr::filter (', condition,')'))) %>%
#                                reframe(eval(parse(text = paste0('sum(table(',std_var,'),na.rm=T)'))))) %>%
#                     rename(value ='eval.parse.text...paste0..sum.table....std_var.....na.rm.T.....') %>% 
#                     mutate(label_names = as.character(label_names), value = as.character(value)))
#     }
#     ###Formatting and restricting outputs where subgroup >=100
#     tab_output$value[seq(1,nrow(tab_output),2)][as.numeric(tab_output$value[nrow(tab_output)])< 100]=' - '
#     
#     return(tab_output)
#   }
#   
#   ###Calling cond_output function::Total
#   total_conditions = c('',"age_cat=='12 or younger'", "age_cat=='13 - 15'","age_cat=='16 or 17'",
#                        "age_cat=='13 - 15'|age_cat=='16 or 17'", "age_cat=='18 or older'", 
#                        paste0("DE_GRADE =='", levels(data$DE_GRADE),"'"))
#   eval(parse(text=paste0('total_res = cbind(',paste0('cond_output("',total_conditions,'")[,2]', collapse = ','),')')))
#   #
#   mod_total_res = rbind(c('Total','','','Age','','','','','Grade',rep('', length(levels(data$DE_GRADE))-1)),
#                         c('','','Total','12 or\n younger','13-15','16-17','13-17','18 or \nolder', gsub('*.        ','',levels(data$DE_GRADE))),
#                         cond_output(total_conditions[1])[,1] %>%bind_cols(rep(c('%','N'),nrow(total_res)/2),total_res)) 
#   
#   flex_total_res =  mod_total_res %>% flextable() %>% autofit() %>%
#     delete_part(part = "header") %>% bold(i = 1:2)%>%
#     add_header_lines(paste0(std_var,': ',label(eval(parse(text=paste0('data$',std_var)))),'\n'))%>%
#     bg(bg="white",i=1,part="header")%>%  
#     hline_top(border = fp_border_default(width = 0),part = "header")%>%
#     vline_left(i=1,border = fp_border_default(width = 0),part = "header")%>%
#     vline(i=1,border = fp_border_default(width = 0),part = "header")%>%
#     fontsize(size = 8 ,part = "all") %>%
#     merge_v(j = 1) %>% valign(valign = 'top')%>%
#     align(j = 3:ncol(mod_total_res),align = 'right', part = 'all') %>%
#     line_spacing(i = 3:nrow(mod_total_res), space = 0.7, part = "body") %>%
#     fix_border_issues()%>% width(j = 4, 1, unit = "in")
#   
#   ##Males 
#   male_conditions = c(paste0('DE_SEX == ',"'", levels(data$DE_SEX)[1],"'"),
#                       paste0('DE_SEX == ',"'", levels(data$DE_SEX)[1],"' & age_cat=='12 or younger'"), 
#                       paste0('DE_SEX == ',"'", levels(data$DE_SEX)[1],"' & age_cat=='13 - 15'"),
#                       paste0('DE_SEX == ',"'", levels(data$DE_SEX)[1],"' & age_cat=='16 or 17'"),
#                       paste0('DE_SEX == ',"'", levels(data$DE_SEX)[1],"' & (age_cat=='13 - 15'|age_cat=='16 or 17')"), 
#                       paste0('DE_SEX == ',"'", levels(data$DE_SEX)[1],"' & age_cat=='18 or older'"), 
#                       paste0(paste0('DE_SEX == ',"'", levels(data$DE_SEX)[1],"' & "), "DE_GRADE =='", levels(data$DE_GRADE),"'"))
#   
#   eval(parse(text=paste0('male_res = cbind(',paste0('cond_output("',male_conditions,'")[,2]', collapse = ','),')')))
#   #
#   mod_male_res = rbind(c('Male','','','Age','','','','','Grade',rep('', length(levels(data$DE_GRADE))-1)),
#                        c('','','All males','12 or\n younger','13-15','16-17','13-17','18 or \nolder', gsub('*.        ','',levels(data$DE_GRADE))),
#                        cond_output(male_conditions[1])[,1] %>% 
#                          bind_cols(rep(c('%','N'),nrow(male_res)/2),male_res)) 
#   
#   flex_male_res = mod_male_res %>% flextable() %>% autofit() %>%
#     delete_part(part = "header") %>% bold(i = 1:2)%>%
#     add_header_lines(paste0(std_var,': ',label(eval(parse(text=paste0('data$',std_var)))),'\n'))%>%
#     bg(bg="white",i=1,part="header")%>%  
#     hline_top(border = fp_border_default(width = 0),part = "header")%>%
#     vline_left(i=1,border = fp_border_default(width = 0),part = "header")%>%
#     vline(i=1,border = fp_border_default(width = 0),part = "header")%>%
#     fontsize(size = 8 ,part = "all") %>%
#     merge_v(j = 1) %>% valign(valign = 'top')%>%
#     align(j = 3:ncol(mod_male_res),align = 'right', part = 'all') %>%
#     line_spacing(i = 3:nrow(mod_male_res), space = 0.7, part = "body") %>%
#     fix_border_issues()%>% width(j = 4, 1, unit = "in")
#   
#   ###Females
#   female_conditions = c(paste0('DE_SEX == ',"'", levels(data$DE_SEX)[2],"'"),
#                         paste0('DE_SEX == ',"'", levels(data$DE_SEX)[2],"' & age_cat=='12 or younger'"), 
#                         paste0('DE_SEX == ',"'", levels(data$DE_SEX)[2],"' & age_cat=='13 - 15'"),
#                         paste0('DE_SEX == ',"'", levels(data$DE_SEX)[2],"' & age_cat=='16 or 17'"),
#                         paste0('DE_SEX == ',"'", levels(data$DE_SEX)[2],"' & (age_cat=='13 - 15'|age_cat=='16 or 17')"), 
#                         paste0('DE_SEX == ',"'", levels(data$DE_SEX)[2],"' & age_cat=='18 or older'"), 
#                         paste0(paste0('DE_SEX == ',"'", levels(data$DE_SEX)[2],"' & "), "DE_GRADE =='", levels(data$DE_GRADE),"'"))
#   
#   eval(parse(text=paste0('female_res = cbind(',paste0('cond_output("',female_conditions,'")[,2]', collapse = ','),')')))
#   #
#   mod_female_res = rbind(c('Female','','','Age','','','','','Grade',rep('', length(levels(data$DE_GRADE))-1)),
#                          c('','','All females','12 or\n younger','13-15','16-17','13-17','18 or \nolder', gsub('*.        ','',levels(data$DE_GRADE))),
#                          cond_output(female_conditions[1])[,1] %>% 
#                            bind_cols(rep(c('%','N'),nrow(female_res)/2),female_res)) 
#   
#   flex_female_res = mod_female_res %>% flextable() %>% autofit() %>%
#     delete_part(part = "header") %>% bold(i = 1:2)%>%
#     add_header_lines(paste0(std_var,': ',label(eval(parse(text=paste0('data$',std_var)))),'\n'))%>%
#     bg(bg="white",i=1,part="header")%>%  
#     hline_top(border = fp_border_default(width = 0),part = "header")%>%
#     vline_left(i=1,border = fp_border_default(width = 0),part = "header")%>%
#     vline(i=1,border = fp_border_default(width = 0),part = "header")%>%
#     fontsize(size = 8 ,part = "all") %>%
#     merge_v(j = 1) %>% valign(valign = 'top')%>%
#     align(j = 3:ncol(mod_female_res),align = 'right', part = 'all') %>%
#     #merge_h_range(j1=9, j2=10)%>%align(j = 9, align = 'left') %>%
#     line_spacing(i = 3:nrow(mod_female_res), space = 0.7, part = "body") %>%
#     fix_border_issues()%>% width(j = 4, 1, unit = "in")#%>% width(j = 3, 1, unit = "in")
#   
#   ###All flextable outputs
#   eval(parse(text=paste0(std_var,'_flex_detailed = list(flex_total_res,flex_male_res,flex_female_res)')))
# }
# 
# ###calling detailed_tab_fn
# #
# vars_for_detailed_tables = setdiff(standard_variables, c("DE_AGE","DE_SEX","DE_GRADE","DB_HEIGHT","DB_WEIGHT"))
# #
# detailed_tables = mclapply(vars_for_detailed_tables, detailed_tab_fn, mc.cores = num_cores, mc.preschedule = FALSE)
# final_detailed_tables = unlist(detailed_tables, recursive = FALSE)
# ##################
# #################
# ##############
# # Function to add a Flextable with a break at the end
# i = NULL
# for (i in 1:length(vars_for_detailed_tables)) {
#   my_doc = read_docx(paste0(getwd(),'/templates/detailed_tables_template.docx'))
#   if (i < length(vars_for_detailed_tables))
#   {
#     my_doc = my_doc %>%
#       body_add_flextable(width(final_detailed_tables[[i]], width = dim(final_detailed_tables[[i]])$widths*10/(flextable_dim(final_detailed_tables[[i]])$widths)),pos = 'on')%>% 
#       body_add_break()  
#   }
#   else
#   {
#     my_doc = my_doc %>%
#       body_add_flextable(width(final_detailed_tables[[i]], width = dim(final_detailed_tables[[i]])$widths*10/(flextable_dim(final_detailed_tables[[i]])$widths)),pos = 'on') 
#   }
#   #
#   print(my_doc,target=paste0(getwd(),'/temp_tables/tempdetailed',i,'.docx')) 
# }
# 
# combined_detailed_doc = read_docx(paste0(getwd(),'/templates/detailed_tables_template.docx'))
# combined_detailed_doc = headers_replace_text_at_bkm(combined_detailed_doc,"country",site_name)
# combined_detailed_doc = headers_replace_text_at_bkm(combined_detailed_doc,"year",survey_year)
# 
# i=NULL
# for(i in rev(1:length(vars_for_detailed_tables))){
#   path <- paste0(getwd(),'/temp_tables/tempdetailed',i,'.docx')
#   detaileddoc <- body_add_docx(combined_detailed_doc, path, pos = "after") 
# }
# 
# # print combine doc
# print(detaileddoc,target=paste0(getwd(),'/reports/',survey_year,' ' ,site_name,' Detailed Tables.docx')) 

# ###calling detailed_tab_fn
# #
# vars_for_detailed_tables = setdiff(standard_variables, c("DE_AGE","DE_SEX","DE_GRADE","DB_HEIGHT","DB_WEIGHT"))
# #
# detailed_tables <- mclapply(vars_for_detailed_tables, detailed_tab_fn, mc.cores = num_cores, mc.preschedule = FALSE)
# final_detailed_tables = unlist(detailed_tables, recursive = FALSE)
# #
# 
# i = NULL
# my_doc = read_docx(paste0(getwd(),'/templates/detailed_tables_template.docx'))
# #
# my_doc = headers_replace_text_at_bkm(my_doc,"country",site_name)
# my_doc = headers_replace_text_at_bkm(my_doc,"year",survey_year)
# 
# for (i in 1:length(vars_for_detailed_tables))
# {
#   if (i < length(vars_for_detailed_tables))
#   {
#     my_doc = my_doc %>%
#       body_add_flextable(width(final_detailed_tables[[i]], width = dim(final_detailed_tables[[i]])$widths*10/(flextable_dim(final_detailed_tables[[i]])$widths))) %>%
#       body_add_break() 
#   }
#   else
#   {
#     my_doc = my_doc %>%
#       body_add_flextable(width(final_detailed_tables[[i]], width = dim(final_detailed_tables[[i]])$widths*10/(flextable_dim(final_detailed_tables[[i]])$widths))) 
#   }
# }
# 
# print(my_doc,target=paste0(getwd(),'/reports/',survey_year,' ' ,site_name,' Detailed Tables.docx')) 
# 
# if(language =='ENGLISH'){
#   mod_total_res = rbind(c('Total','','','Age','','','','','Grade',rep('', length(levels(data$DE_GRADE))-1)),
#                         c('','','Total','12 or\n younger','13-15','16-17','13-17','18 or \nolder', gsub('*.        ','',levels(data$DE_GRADE))),
#                         cond_output(total_conditions[1])[,1] %>%bind_cols(rep(c('%','N'),nrow(total_res)/2),total_res))
# } else if(language =='FRENCH'){
#   mod_total_res = rbind(c('Total','','','Âge','','','','','Classe',rep('', length(levels(data$DE_GRADE))-1)),
#                         c('','','Total','12 ans \n ou moins','13-15','16-17','13-17','18 ans \nou plus', gsub('*.        ','',levels(data$DE_GRADE))),
#                         cond_output(total_conditions[1])[,1] %>%bind_cols(rep(c('%','N'),nrow(total_res)/2),total_res))
# } else if(language =='SPANISH'){
#   mod_total_res = rbind(c('Total','','','Edad','','','','','Clase',rep('', length(levels(data$DE_GRADE))-1)),
#                         c('','','Total','12 años \no menos','13-15','16-17','13-17','18 años \no más', gsub('*.        ','',levels(data$DE_GRADE))),
#                         cond_output(total_conditions[1])[,1] %>%bind_cols(rep(c('%','N'),nrow(total_res)/2),total_res))
# }else if(language =='RUSSIAN'){
#   mod_total_res = rbind(c('Общий','','','Возраст','','','','','Сорт',rep('', length(levels(data$DE_GRADE))-1)),
#                         c('','','Общий','12 лет \nили младше','13-15','16-17','13-17','18 лет \nи старше', gsub('*.        ','',levels(data$DE_GRADE))),
#                         cond_output(total_conditions[1])[,1] %>%bind_cols(rep(c('%','N'),nrow(total_res)/2),total_res)) %>%as.data.frame()
# } else{}

# if(language == 'ENGLISH')
# {
#   mod_male_res = rbind(c('Male','','','Age','','','','','Grade',rep('', length(levels(data$DE_GRADE))-1)),
#                        c('','','All males','12 or\n younger','13-15','16-17','13-17','18 or \nolder', gsub('*.        ','',levels(data$DE_GRADE))),
#                        cond_output(male_conditions[1])[,1] %>%
#                          bind_cols(rep(c('%','N'),nrow(male_res)/2),male_res))
# } else if (language == 'FRENCH'){
#   mod_male_res = rbind(c('Mâles','','','Âge','','','','','Classe',rep('', length(levels(data$DE_GRADE))-1)),
#                        c('','','Tous les\nmâles','12 ans \n ou moins','13-15','16-17','13-17','18 ans \nou plus', gsub('*.        ','',levels(data$DE_GRADE))),
#                        cond_output(male_conditions[1])[,1] %>%
#                          bind_cols(rep(c('%','N'),nrow(male_res)/2),male_res))
# }else if (language == 'SPANISH'){
#   mod_male_res = rbind(c('Machos','','','Edad','','','','','Clase',rep('', length(levels(data$DE_GRADE))-1)),
#                        c('','','Todos \nlos hombres','12 años \no menos','13-15','16-17','13-17','18 años \no más', gsub('*.        ','',levels(data$DE_GRADE))),
#                        cond_output(male_conditions[1])[,1] %>%
#                          bind_cols(rep(c('%','N'),nrow(male_res)/2),male_res))
# }else if (language == 'RUSSIAN'){
#   mod_male_res = rbind(c('Мужчины','','','Возраст','','','','','Сорт',rep('', length(levels(data$DE_GRADE))-1)),
#                        c('','','Все \nмужчины','12 лет \nили меньше','13-15','16-17','13-17','18 лет \nи старше', gsub('*.        ','',levels(data$DE_GRADE))),
#                        cond_output(male_conditions[1])[,1] %>%
#                          bind_cols(rep(c('%','N'),nrow(male_res)/2),male_res))
# } else{}


# if (language =='ENGLISH')
# {
#   mod_female_res = rbind(c('Female','','','Age','','','','','Grade',rep('', length(levels(data$DE_GRADE))-1)),
#                          c('','','All females','12 or\n younger','13-15','16-17','13-17','18 or \nolder', gsub('*.        ','',levels(data$DE_GRADE))),
#                          cond_output(female_conditions[1])[,1] %>%
#                            bind_cols(rep(c('%','N'),nrow(female_res)/2),female_res))
# } else if(language =='FRENCH'){
#   mod_female_res = rbind(c('Femelle','','','Âge','','','','','Classe',rep('', length(levels(data$DE_GRADE))-1)),
#                          c('','','Toutes les\nfemelle','12 ans \n ou moins','13-15','16-17','13-17','18 ans \nou plus', gsub('*.        ','',levels(data$DE_GRADE))),
#                          cond_output(female_conditions[1])[,1] %>%
#                            bind_cols(rep(c('%','N'),nrow(female_res)/2),female_res))
# }else if(language =='SPANISH'){
#   mod_female_res = rbind(c('Femenino','','','Edad','','','','','Clase',rep('', length(levels(data$DE_GRADE))-1)),
#                          c('','','Todas las mujeres','12 años \no menos','13-15','16-17','13-17','18 años \no más', gsub('*.        ','',levels(data$DE_GRADE))),
#                          cond_output(female_conditions[1])[,1] %>%
#                            bind_cols(rep(c('%','N'),nrow(female_res)/2),female_res))
# }else if(language =='RUSSIAN'){
#   mod_female_res = rbind(c('Женский','','','Возраст','','','','','Сорт',rep('', length(levels(data$DE_GRADE))-1)),
#                          c('','','Все \nженщины','12 лет \nили меньше','13-15','16-17','13-17','18 лет \nи старше', gsub('*.        ','',levels(data$DE_GRADE))),
#                          cond_output(female_conditions[1])[,1] %>%
#                            bind_cols(rep(c('%','N'),nrow(female_res)/2),female_res))
# }
