###long_school_sample
colnames(sample_schools) = tolower(colnames(sample_schools))
###Adding total enrolment for each school to sample_schools
sample_schools = sample_schools %>% #mutate_all(~replace_na(., 0)) %>%
  mutate(quantil_grp = case_when(enrolment < quantile(enrolment, 0.3333, na.rm = T)~1,
                                 enrolment >= quantile(enrolment, 0.3333, na.rm = T)&
                                   enrolment < quantile(enrolment, 0.6666, na.rm = T)~2,
                                 enrolment >= quantile(enrolment, 0.6666, na.rm = T) ~3),
         school_id = as.character(school_id)) 

########
##Computing school non-response adjustment factor by quantiles
sample_schools = sample_schools %>%
  left_join(sample_schools %>%
              reframe(sch_prt = sum(school_part==1, na.rm = T), sch_sel = sum((school_part==1|school_part==0), na.rm = T),
                      schwgt_adj_factor = round(sch_sel/sch_prt,5),.by=quantil_grp) %>%dplyr::select(quantil_grp, schwgt_adj_factor))

##Applying school non-response factor to obtain adjusted school weight
adj_school_wt = sample_schools %>% mutate(adjusted_scwgt = scwgt*schwgt_adj_factor) %>% mutate(school_id = as.character(school_id))

##Transforming the school sample into long format
long_school_sample = adj_school_wt %>%
  gather(key, value, starts_with("class"), starts_with("cenrol"), starts_with("stpart")) %>%
  separate(key, into = c("var", "index"), sep = "(?<=\\D)(?=\\d)") %>%
  spread(key = var, value = value) %>%
  dplyr::rename(class_id = class) %>%
  dplyr::filter(class_id!=0)%>%mutate(school_id = as.character(school_id))


##########Fact sheet 
if(language =='FRENCH')
{
  factsheet_sections = updated_matrix %>% dplyr::filter(!is.na(factsheet_section))%>%
    dplyr::select(bin_standard, factsheet_section,factsheet_subtitle) %>% distinct() %>%
    rename(sub_title = factsheet_subtitle) 
} else{
  factsheet_sections = updated_matrix %>% dplyr::filter(!is.na(factsheet_section))%>%
    dplyr::select(bin_standard, factsheet_section,factsheet_subtitle) %>% distinct() %>%
    rename(sub_title = factsheet_subtitle) %>%
    mutate(sub_title = gsub('And','and',str_to_title(gsub('\\s+',' ',sub_title))))
}
#
log_conditions = c("(age_cat=='13 - 15'| age_cat=='16 or 17')",
                   paste0("(age_cat=='13 - 15'| age_cat=='16 or 17') & DE_SEX =='",levels(data$DE_SEX)[1],"'"),
                   paste0("(age_cat=='13 - 15'| age_cat=='16 or 17') & DE_SEX =='",levels(data$DE_SEX)[2],"'"))

# log_conditions = c("(age_cat=='13 - 15')",
#                    paste0("(age_cat=='13 - 15') & DE_SEX =='",levels(data$DE_SEX)[1],"'"),
#                    paste0("(age_cat=='13 - 15') & DE_SEX =='",levels(data$DE_SEX)[2],"'"))

degrees_freedom = degf(svy_data3)-1



fact_sheet_fn = function(section = factsheet_sections$factsheet_section[1], sub_title =factsheet_sections$sub_title[1])
{
  i = NULL
  j = NULL
  all_variable_results = NULL
  var_group = (factsheet_sections %>% dplyr::filter(factsheet_section == section))$bin_standard
  
  for(i in var_group)
  {
    outputs = c()
    for (j in log_conditions)
    {
      num_var = eval(parse(text=lang_titles[1]))[1]
      
      formula = make.formula(paste0(i, '=="',num_var,'"'))
      est_ciprop = svyciprop(formula, design=subset(svy_data3,eval(parse(text=j))), method="lo", df = degrees_freedom)
      #the proportion and  confidence interval
      est_ci = paste0(formatC(round(as.vector(est_ciprop)*100,1),format = 'f', digits = 1), '\n(',
                      formatC(round((as.numeric(attr(est_ciprop, "ci")[1]))*100,1),format = 'f', digits = 1),' - ',
                      formatC(round(100*as.numeric(attr(est_ciprop, "ci")[2]),1),format = 'f', digits = 1),')')
      ##
      n_participants = data %>% filter(eval(parse(text=paste0('!is.na(',i,') & (',j,')')))) %>% reframe(n()) %>% as.numeric()
      est_ci = ifelse(n_participants>=100,est_ci,'-')
      ##
      outputs =c(outputs,est_ci)
    }
    var_output = c(label(data[,i]), outputs)
    all_variable_results = rbind(all_variable_results, var_output)
    rownames(all_variable_results)=NULL
  }
  ##Adding subtitle
  final_var_results = rbind(c(sub_title, rep('',3)), all_variable_results)
  return(final_var_results)                        
}


###Calling fact_sheet_fn function
i = NULL
full_fact_sheet = NULL

for(i in sort(unique(factsheet_sections$factsheet_section)))
{
  corresp_subtitle = unique(factsheet_sections$sub_title[factsheet_sections$factsheet_section==i])
  fact_subtable = fact_sheet_fn(section = i, sub_title =corresp_subtitle)
  #
  full_fact_sheet = rbind(full_fact_sheet, fact_subtable)
}

rev_full_fact_sheet = full_fact_sheet%>% as.data.frame()

#
subtitle_nrows = as.numeric((rev_full_fact_sheet %>% bind_cols(row_names = rownames(rev_full_fact_sheet))%>%
                               dplyr::filter(V2=='') %>% dplyr::select(row_names))$row_names)

###
if(language =='FRENCH')
{
  rev_full_fact_sheet = eval(parse(text=paste0('cbind(',paste0(gsub('\\.',',',rev_full_fact_sheet), collapse =','),')'))) %>% as.data.frame()
  #
  #colnames(rev_full_fact_sheet) =c('Résultats pour les élèves âgés de 13 à 17 ans','Total','Garçons','Filles')
  
}else{}

colnames(rev_full_fact_sheet) = eval(parse(text = lang_titles[19]))

#
flex_fact_sheet = rev_full_fact_sheet %>% flextable() %>% autofit() %>%
                  flextable::style(pr_t=fp_text(font.size=10,font.family='Source Sans Pro'), part = 'all')%>%
                  bold(i = subtitle_nrows)%>%
                  bg(bg="white",i=1,part="header")%>%  
                  theme_box()%>% 
                  align(align = "center", j = 2:4, part = "all") %>%
                  #fontsize(size = 10 ,part = "all")%>%
                  merge_h_range(i=subtitle_nrows, j1=1,j2=4)%>%
                  width(j = 2:4, 4.3, unit = "in")%>% 
                  bg(bg="#C9DDF3",i=1,part="header")%>%
                  bg(bg="#009ADE",i=subtitle_nrows,part="body")%>%
                  padding(padding = 0, part = "all") %>%
                  paginate()
# rev_full_fact_sheet = rbind(c('Results for Students Aged 13-17 Years','Total','Boys','Girls'),full_fact_sheet) %>% as.data.frame()
# #
# subtitle_nrows = as.numeric((rev_full_fact_sheet %>% bind_cols(row_names = rownames(rev_full_fact_sheet))%>%
#                                    dplyr::filter(V2=='') %>% dplyr::select(row_names))$row_names)
# #
# flex_fact_sheet = rev_full_fact_sheet %>% flextable() %>% autofit() %>%
#                   delete_part(part = "header") %>% 
#                   bold(i = c(1, subtitle_nrows))%>%
#                   bg(bg="white",i=1,part="header")%>%  
#                   theme_box()%>% 
#                   align(align = "center", j = 2:4, part = "all") %>%
#                   #fontsize(size = 10 ,part = "all")%>%
#                   merge_h_range(i=subtitle_nrows, j1=1,j2=4)%>%
#                   width(j = 2:4, 4.3, unit = "in")%>% 
#                   bg(bg="#C9DDF3",i=1,part="body")%>%
#                   bg(bg="#009ADE",i=subtitle_nrows,part="body")%>%
#                   style(pr_t=fp_text(font.size=10,font.family='Source Sans Pro'))%>%
#                   padding(padding = 0, part = "all") %>%
#                   paginate()
###
##Generating Factsheet::::
if(is_this_census==FALSE)
{
  doc = read_docx(paste0(getwd(),'/templates/fact_sheet_template_',language,'.docx'))
}else{
  doc = read_docx(paste0(getwd(),'/templates/census_fact_sheet_template_',language,'.docx'))
  }

#
doc = headers_replace_text_at_bkm(doc,"country",site_name)
doc = headers_replace_text_at_bkm(doc,"year",paste0(lang_titles[29],' ',survey_year))
doc = headers_replace_text_at_bkm(doc,"Fact",lang_titles[26])

#Adding other text
#Determining grades
#class_positions = which(LETTERS %in% toupper(unique(post_strat_adj$grade)))
#
if(is_this_census == FALSE)
{
additional_text = c(paste0(survey_year,' ',site_name),
                    paste0(levels(data$DE_GRADE)[1],' - ',levels(data$DE_GRADE)[length(levels(data$DE_GRADE))]),
                    paste0(levels(data$DE_GRADE)[1],' - ',levels(data$DE_GRADE)[length(levels(data$DE_GRADE))]),
                    site_name,
                    sum(sample_schools$school_part==1|sample_schools$school_part==0, na.rm = T),
                    #nrow(data),
                    site_name,
                    paste0(paste0(sort(unique(factsheet_sections$sub_title))[-length(unique(factsheet_sections$sub_title))], collapse = '; '),
                           '; & ',sort(unique(factsheet_sections$sub_title))[length(unique(factsheet_sections$sub_title))]),
                    paste0(formatC(round(100*(sum(sample_schools$school_part==1, na.rm = T)/sum(sample_schools$school_part==1|sample_schools$school_part==0, na.rm = T)),1),format = 'f', digits = 1),'%'),
                    paste0(formatC(round(100*(sum(long_school_sample$stpart,na.rm = T)/sum(long_school_sample$cenrol, na.rm = T)),1),format = 'f', digits = 1),'%'),
                    paste0(formatC(round(100*(sum(sample_schools$school_part==1, na.rm = T)/sum(sample_schools$school_part==1|sample_schools$school_part==0, na.rm = T))*(sum(long_school_sample$stpart,na.rm = T)/sum(long_school_sample$cenrol, na.rm = T)),1),format = 'f', digits = 1),'%'),
                    #sum(long_school_sample$stpart,na.rm = T),
                    nrow(data),
                    site_name)
bmks = paste0('bmk', 1:length(additional_text))
#
eval(parse(text=paste0('doc = body_replace_text_at_bkm(doc,"', bmks,'","', additional_text,'")', sep='\n')))
} else{
  additional_text = c(paste0(survey_year,' ',site_name),
                      paste0(levels(data$DE_GRADE)[1],' - ',levels(data$DE_GRADE)[length(levels(data$DE_GRADE))]),
                      paste0(levels(data$DE_GRADE)[1],' - ',levels(data$DE_GRADE)[length(levels(data$DE_GRADE))]),
                      site_name,
                      #sum(sample_schools$school_part==1|sample_schools$school_part==0, na.rm = T),
                      site_name,
                      paste0(paste0(sort(unique(factsheet_sections$sub_title))[-length(unique(factsheet_sections$sub_title))], collapse = '; '),
                             '; & ',sort(unique(factsheet_sections$sub_title))[length(unique(factsheet_sections$sub_title))]),
                      paste0(formatC(round(100*(sum(sample_schools$school_part==1, na.rm = T)/sum(sample_schools$school_part==1|sample_schools$school_part==0, na.rm = T)),1),format = 'f', digits = 1),'%'),
                      paste0(formatC(round(100*(sum(long_school_sample$stpart,na.rm = T)/sum(long_school_sample$cenrol, na.rm = T)),1),format = 'f', digits = 1),'%'),
                      paste0(formatC(round(100*(sum(sample_schools$school_part==1, na.rm = T)/sum(sample_schools$school_part==1|sample_schools$school_part==0, na.rm = T))*(sum(long_school_sample$stpart,na.rm = T)/sum(long_school_sample$cenrol, na.rm = T)),1),format = 'f', digits = 1),'%'),
                      #sum(long_school_sample$stpart,na.rm = T),
                      nrow(data),
                      site_name)
  #bmks = setdiff(paste0('bmk', 1:length(additional_text)),'bmk5')
  bmks = paste0('bmk', 1:length(additional_text))
  
  #
  eval(parse(text=paste0('doc = body_replace_text_at_bkm(doc,"', bmks,'","', additional_text,'")', sep='\n')))
}



#
doc=doc %>% cursor_bookmark(id  = "table1") %>%
  body_add_flextable(width(flex_fact_sheet, width = dim(flex_fact_sheet)$widths*7.25/(flextable_dim(flex_fact_sheet)$widths)), pos = "on", align = 'left')

print(doc,target=paste0(getwd(),'/reports/',survey_year,' ' ,site_name,' Factsheet.docx')) 


# ##subtitle
# mutate(sub_title = case_when(factsheet_section =='alcohol_use' ~ 'Alcohol Use',
#                              factsheet_section =='dietary' ~ 'Dietary Behaviors',
#                              factsheet_section =='drug' ~ 'Drug Use',
#                              factsheet_section =='hygiene' ~ 'Hygiene',
#                              factsheet_section =='mental' ~ 'Mental Health',
#                              factsheet_section =='physical' ~ 'Physical Activity',
#                              factsheet_section =='protective' ~ 'Protective Factors',
#                              factsheet_section =='sexual' ~ 'Sexual Behaviors',
#                              factsheet_section =='tobacco' ~ 'Tobacco Use',
#                              factsheet_section =='violence' ~ 'Violence and Unintentional Injury'))

# if (language == 'ENGLISH'){
#   colnames(rev_full_fact_sheet) = c('Results for Students Aged 13-17 Years','Total','Boys','Girls')
# } else if (language =='FRENCH'){  
#   colnames(rev_full_fact_sheet) =c('Résultats pour les élèves âgés de 13 à 17 ans','Total','Garçons','Filles')
# }else if (language =='SPANISH'){  
#   colnames(rev_full_fact_sheet) =c('Resultados de alumnos de 13 a 17 años', 'Total', 'Chicos', 'Niñas')
# }else if (language =='RUSSIAN'){  
#   colnames(rev_full_fact_sheet) =c('Результаты учащихся 13-17 лет','Всего','Мальчики','Девочки')
# } else{}
