###When working with rstudio interface, please uncomment lines 95 and 216, and comment lines 96 and 217

degrees_freedom = degf(svy_data3)-1
####
summary_table_fn = function(variable = 'DB_B_HUNGRY')
{ 
  num_var = eval(parse(text=lang_titles[1]))[1]
  
  eval(parse(text=paste0('formula = ~I(',variable, '=="',num_var,'")')))
  #
  cond1 = paste0('!is.na(',variable,')')
  
  ciprop_function = function(cond_subset = 'DE_SEX==""')
  {
    #Number of participants and estimated ci
    if (cond_subset!='')
    {
      n_participants = data %>% filter(eval(parse(text=paste0(cond_subset,'&', cond1)))) %>% reframe(n()) %>% as.numeric()
      
      if (n_participants>n_cutoff)
      {
        est_ciprop = svyciprop(formula, design=subset(svy_data3,eval(parse(text=cond_subset))), method="lo", df = degrees_freedom)
      }
      else
      {
        est_ciprop = '-'
      }
    }
    else
    {
      n_participants = data %>% filter(eval(parse(text=cond1))) %>% reframe(n()) %>% as.numeric()
      if (n_participants>n_cutoff)
      {
        est_ciprop =svyciprop(formula, design=subset(svy_data3,eval(parse(text=cond1))), method="lo", df = degrees_freedom)
      }
      else
      {
        est_ciprop = '-'
      }
    }
    
    #the proportion
    total_est = ifelse(n_participants>n_cutoff,formatC(round(as.vector(est_ciprop)*100,1),format = 'f', digits = 1),'-')
    # the confidence interval
    est_ci = ifelse(n_participants>n_cutoff,paste0('(',formatC(round((as.numeric(attr(est_ciprop, "ci")[1]))*100,1),format = 'f', digits = 1),' - ',formatC(round(100*as.numeric(attr(est_ciprop, "ci")[2]),1),format = 'f', digits = 1),')'),'-')
    ###
    if(weighted_reporting=='Yes'){
    result = bind_cols(total_est, est_ci, n_participants)%>%data.frame()
    colnames(result) = c('Percent','CI','N')
    }else{
      result = bind_cols(total_est, n_participants)%>%data.frame()  
      colnames(result) = c('Percent','N')
    }
    
    return(result)
  }
  
  ###Calling ciprop_function
  age_cat_levels = paste0("age_cat","== '",levels(data$age_cat),"'")
  sex_cat_levels = paste0("DE_SEX","== '",levels(data$DE_SEX),"'")##Sex
  class_cat_levels = paste0("DE_GRADE","== '",levels(data$DE_GRADE),"'")###Class/grade
  ##
  male_age_cat_levels = paste0("DE_SEX","== '",levels(data$DE_SEX)[1],"'",' & ',"age_cat","== '",levels(data$age_cat),"'")
  female_age_cat_levels =  paste0("DE_SEX","== '",levels(data$DE_SEX)[2],"'", ' & ',"age_cat","== '",levels(data$age_cat),"'")
  ##
  male_class_cat_levels = paste0("DE_SEX","== '",levels(data$DE_SEX)[1],"'",' & ',"DE_GRADE","== '",levels(data$DE_GRADE),"'")
  female_class_cat_levels = paste0("DE_SEX","== '",levels(data$DE_SEX)[2],"'", ' & ',"DE_GRADE","== '",levels(data$DE_GRADE),"'")
  #####
  #class level conditions
  class_conditions = NULL
  
  k = NULL
  
  for(k in 1:length(levels(data$DE_GRADE)))
  {
    
    class_level_cond = c(class_cat_levels[k],male_class_cat_levels[k],female_class_cat_levels[k])
    class_conditions = c(class_conditions,class_level_cond)
    
  }
  ###
 all_conditions = c('',sex_cat_levels,
                     age_cat_levels[1],male_age_cat_levels[1],female_age_cat_levels[1],
                     age_cat_levels[2],male_age_cat_levels[2],female_age_cat_levels[2],
                     age_cat_levels[3],male_age_cat_levels[3],female_age_cat_levels[3],
                     '(age_cat=="13 - 15"|age_cat=="16 or 17")',paste0('DE_SEX ==','"', levels(data$DE_SEX)[1],'" & (age_cat=="13 - 15"|age_cat=="16 or 17")'),
                    paste0('DE_SEX ==','"', levels(data$DE_SEX)[2],'" & (age_cat=="13 - 15"|age_cat=="16 or 17")'),
                     age_cat_levels[4],male_age_cat_levels[4],female_age_cat_levels[4],
                     class_conditions)
  
  ###
 #cl = makeCluster(num_cores)
  #list_point_plus_ci_est <- parallel::mclapply(all_conditions, ciprop_function, mc.cores = num_cores,mc.preschedule = FALSE)
  # stopCluster(cl)
  #list_point_plus_ci_est <- future.apply::future_lapply(all_conditions, ciprop_function,future.seed=TRUE)
  list_point_plus_ci_est <- lapply(all_conditions, ciprop_function)
 
  all_point_plus_ci_est = do.call('cbind',list_point_plus_ci_est)
  
  #####All possible ranges
  lower_limits = c()
  for(x in 1:length(all_point_plus_ci_est)){
      if(weighted_reporting=='Yes'){
        if(x%%9==0 & (x >45 & x < length(all_point_plus_ci_est))){
          lower_limits =c(lower_limits,x+1)
        }else{}
      }else{
        if(x%%6==0 & (x >30 & x < length(all_point_plus_ci_est))){
          lower_limits =c(lower_limits,x+1)  
        }else{}}
  }
  
  ###class labels
  if(weighted_reporting=='Yes'){
    eval(parse(text=paste0('class_tab =rbind(',paste0('c("',levels(data$DE_GRADE),
                                                    '",all_point_plus_ci_est[',
                                                    lower_limits,':',lower_limits+8,'])', collapse = ','),')')))
  }else{
    eval(parse(text=paste0('class_tab =rbind(',paste0('c("',levels(data$DE_GRADE),
                                                      '",all_point_plus_ci_est[',
                                                      lower_limits,':',lower_limits+5,'])', collapse = ','),')')))
  }
  ####text outputs
  text_output =eval(parse(text = lang_titles[18]))
  
  ####
  if(weighted_reporting=='Yes'){
  
  output_table = rbind(c(text_output[1], all_point_plus_ci_est[1:9]),
                       c(text_output[2], rep('',9)),
                       c(text_output[3], all_point_plus_ci_est[10:18]),
                       c(text_output[4], all_point_plus_ci_est[19:27]),
                       c(text_output[5], all_point_plus_ci_est[28:36]),
                       c(text_output[6], all_point_plus_ci_est[37:45]),
                       c(text_output[7], all_point_plus_ci_est[46:54]),
                       c(text_output[8], rep('',9)),
                       class_tab
                         )
  }else{
    output_table = rbind(c(text_output[1], all_point_plus_ci_est[1:6]),
                         c(text_output[2], rep('',6)),
                         c(text_output[3], all_point_plus_ci_est[7:12]),
                         c(text_output[4], all_point_plus_ci_est[13:18]),
                         c(text_output[5], all_point_plus_ci_est[19:24]),
                         c(text_output[6], all_point_plus_ci_est[25:30]),
                         c(text_output[7], all_point_plus_ci_est[31:36]),
                         c(text_output[8], rep('',6)),
                         class_tab
    )  
  }
  
  
  #####Creating flextable
  tab_names = colnames(output_table)
  table_tile = paste0(variable,': ',label(eval(parse(text=paste0('data$',variable)))))
  ######
  if(weighted_reporting=='Yes'){
    res_table_obj = rbind(c('','',text_output[9],''
                          ,'',text_output[10],''
                          ,'',text_output[11],''),
                        c('',text_output[12],text_output[13],'N'
                          ,text_output[12],text_output[13],'N'
                          ,text_output[12],text_output[13],'N'),
                        output_table) %>% as.data.frame()
  }else{
    res_table_obj = rbind(c('','',text_output[9]
                            ,'',text_output[10]
                            ,'',text_output[11]),
                          c('',text_output[12],'N'
                            ,text_output[12],'N'
                            ,text_output[12],'N'),
                          output_table) %>% as.data.frame()
  }
  
  res_table_obj = res_table_obj %>%mutate(across(everything(), as.character))
  #
  if(language =='FRENCH')
  {
    res_table_obj = eval(parse(text=paste0('cbind(',paste0(gsub('\\.',',',res_table_obj), collapse =','),')'))) %>% as.data.frame()
  }else{}
  #
  if(weighted_reporting=='Yes'){
  table_output = res_table_obj %>% flextable() %>% autofit() %>%
                  delete_part(part = "header") %>% 
                  add_header_lines(table_tile)%>%
                  flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
                  bold(i = c(1:2,4,10) )%>%
                  bg(bg="white",i=1,part="header")%>%  
                  hline_top(border = fp_border_default(width = 0),part = "header")%>%
                  vline_left(i=1,border = fp_border_default(width = 0),part = "header")%>%
                  vline(i=1,border = fp_border_default(width = 0),part = "header")%>%
                  align(align = "center", j = 2:10, part = "all") %>%
                  width(j = 1:10, 3, unit = "in")%>%
                  fontsize(size = 9 ,part = "all")
  }else{
    table_output = res_table_obj %>% flextable() %>% autofit() %>%
      delete_part(part = "header") %>% 
      add_header_lines(table_tile)%>%
      flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
      bold(i = c(1:2,4,10) )%>%
      bg(bg="white",i=1,part="header")%>%  
      hline_top(border = fp_border_default(width = 0),part = "header")%>%
      vline_left(i=1,border = fp_border_default(width = 0),part = "header")%>%
      vline(i=1,border = fp_border_default(width = 0),part = "header")%>%
      align(align = "center", j = 2:7, part = "all") %>%
      width(j = 1:7, 3, unit = "in")%>%
      fontsize(size = 9 ,part = "all")
  }
  return(table_output)
}

#####Calling summary_table_fn function
# cl = makeCluster(num_cores)
# all_summary_tables <<- parallel::mclapply(new_variables, summary_table_fn, mc.cores = num_cores, mc.preschedule = FALSE)
# stopCluster(cl)
#all_summary_tables = future.apply::future_lapply(new_variables, summary_table_fn, future.seed=TRUE)
all_summary_tables = lapply(new_variables, summary_table_fn)

# Function to add a Flextable with a break at the end
i = NULL
for (i in 1:length(new_variables)) {
  
  if(weighted_reporting=='Yes'){
    my_doc = read_docx(paste0(getwd(),'/templates/',language,'/Table_summary_template.docx'))
  }else{
    my_doc = read_docx(paste0(getwd(),'/templates/',language,'/UNWEIGHTED/Table_summary_template.docx'))  
  }
  
  summary_tables = all_summary_tables[[i]]

  if (i < length(new_variables))
  {
    if(weighted_reporting=='Yes'){
    my_doc = my_doc %>%
             body_add_flextable(width(summary_tables, width = dim(summary_tables)$widths*10/(flextable_dim(summary_tables)$widths)),pos = 'on') %>%
      body_add_break()
    }else{
      my_doc = my_doc %>%
        body_add_flextable(width(summary_tables, width = dim(summary_tables)$widths*7/(flextable_dim(summary_tables)$widths)),pos = 'on') %>%
        body_add_break() 
    }
  }
  else
  {
    if(weighted_reporting=='Yes'){
    my_doc = my_doc %>%
             body_add_flextable(width(summary_tables, width = dim(summary_tables)$widths*10/(flextable_dim(summary_tables)$widths)),pos = 'on')
    }else{
      my_doc = my_doc %>%
        body_add_flextable(width(summary_tables, width = dim(summary_tables)$widths*7/(flextable_dim(summary_tables)$widths)),pos = 'on') 
    }
  }
  #
  print(my_doc,target=paste0(getwd(),'/temp_tables/tempsum',i,'.docx'))
}

#reactive({
if(weighted_reporting=='Yes'){
  combined_sum_doc <<- read_docx(paste0(getwd(),'/templates/',language,'/Table_summary_template.docx'))
}else{
  combined_sum_doc <<- read_docx(paste0(getwd(),'/templates/',language,'/UNWEIGHTED/Table_summary_template.docx'))
}
combined_sum_doc <<- officer::headers_replace_text_at_bkm(combined_sum_doc,"country",site_name)
combined_sum_doc <<- officer::headers_replace_text_at_bkm(combined_sum_doc,"year",survey_year)
#})
i=NULL
for(i in 1:length(new_variables)){
  path <- paste0(getwd(),'/temp_tables/tempsum',i,'.docx')
  combined_sum_doc <<- body_add_docx(combined_sum_doc, path, pos = "after") 
}

# print combine doc

print(combined_sum_doc,target=paste0(getwd(),'/Reports/',survey_year,' ' ,site_name,' Summary Tables.docx')) 