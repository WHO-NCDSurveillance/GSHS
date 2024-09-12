demographic_table_fn = function()
{ 
  
  ###Distribution by age category
  unwgt_age_freq = cbind(with(data, table(age_cat, useNA = 'ifany')),
                     with(data, table(age_cat, DE_SEX, useNA = 'ifany')))
  unwgt_age_freq[is.na(unwgt_age_freq)]=0
  #
  unwgt_age_perc = rbind(formatC(round(cbind(with(data, prop.table(table(age_cat))),
                     with(data, prop.table(table(age_cat, DE_SEX),2)))*100,1),digits = 1, 
                     format = 'f'),c('-','-','-'))
  #
  wgt_age_perc = rbind(cbind(formatC(round(prop.table(svytable(~age_cat,design = svy_data))*100,1),
                                     format = 'f', digits = 1),
                   formatC(round(prop.table(svytable(~age_cat+DE_SEX,design = svy_data),2)*100,1),
                           format = 'f', digits = 1)),c('-','-','-'))
 ###Distribution by grade
  unwgt_grade_freq = cbind(with(data, table(DE_GRADE, useNA = 'ifany')),
                         with(data, table(DE_GRADE, DE_SEX, useNA = 'ifany')))
  #
  unwgt_grade_freq[is.na(unwgt_grade_freq)]=0
  #
  unwgt_grade_perc = rbind(formatC(round(cbind(with(data, prop.table(table(DE_GRADE))),
                                             with(data, prop.table(table(DE_GRADE, DE_SEX),2)))*100,1), 
                                   digits = 1, format = 'f'),c('-','-','-'))
  #
  wgt_grade_perc = rbind(cbind(formatC(round(prop.table(svytable(~DE_GRADE,design = svy_data))*100,1),format = 'f', digits = 1),
                             formatC(round(prop.table(svytable(~DE_GRADE+DE_SEX,design = svy_data),2)*100,1),
                                     format = 'f', digits = 1)),c('-','-','-'))
  #####
  if (ncol(unwgt_age_freq)==3)
  {
    unwgt_age_freq = unwgt_age_freq %>% as.data.frame() %>% mutate(Missing = 0)
  }else{}
  #
  if (ncol(unwgt_grade_freq)==3)
  {
    unwgt_grade_freq = unwgt_grade_freq %>% as.data.frame() %>% mutate(Missing = 0)
  }else{}
  

  #####
  total_n = c(nrow(data), with(data,table(DE_SEX, useNA = 'ifany')))
  total_n[4][is.na(total_n[4])]=0
  #
  combined_table = cbind(c('','',eval(parse(text=lang_titles[4])), eval(parse(text=lang_titles[5])),
                           eval(parse(text=lang_titles[6])),eval(parse(text=lang_titles[7])),eval(parse(text=lang_titles[8])),eval(parse(text=lang_titles[9])),
                           eval(parse(text=lang_titles[2])),eval(parse(text=lang_titles[11])), levels(data$DE_GRADE),eval(parse(text=lang_titles[2]))),
                         rbind(c(eval(parse(text=lang_titles[4])),'','',eval(parse(text=lang_titles[12])),'','',eval(parse(text=lang_titles[13])),'','',eval(parse(text=lang_titles[2]))),
                               c('N',eval(parse(text=lang_titles[14])),eval(parse(text=lang_titles[15])),
                                 'N',eval(parse(text=lang_titles[14])),eval(parse(text=lang_titles[15])),
                                 'N',eval(parse(text=lang_titles[14])),eval(parse(text=lang_titles[15])),''),
                               c(total_n[1],'','',total_n[2],'','',total_n[3],'','',total_n[4]),
                               c('','','','','','','','','',''),
                               cbind(unwgt_age_freq[,1],unwgt_age_perc[,1],wgt_age_perc[,1],
                                     unwgt_age_freq[,2],unwgt_age_perc[,2],wgt_age_perc[,2],
                                     unwgt_age_freq[,3],unwgt_age_perc[,3],wgt_age_perc[,3],unwgt_age_freq[,4]),
                               rep('',10),
                               cbind(unwgt_grade_freq[,1],unwgt_grade_perc[,1],wgt_grade_perc[,1],
                                     unwgt_grade_freq[,2],unwgt_grade_perc[,2],wgt_grade_perc[,2],
                                     unwgt_grade_freq[,3],unwgt_grade_perc[,3],wgt_grade_perc[,3],unwgt_grade_freq[,4])))
  
  colnames(combined_table) =c('',eval(parse(text=lang_titles[4])),'','',eval(parse(text=lang_titles[12])),'','',eval(parse(text=lang_titles[13])),'','',eval(parse(text=lang_titles[2])))
  
  
  ###########
  rownames(combined_table)=NULL
  if (language =='FRENCH')
  {
    combined_table = gsub('\\.',',',combined_table)
  } else{}
 #
  flex_table_output = combined_table %>% as.data.frame() %>% flextable() %>% autofit() %>%
    flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>% 
    delete_part(part = "header") %>% bold(i = c(1:2,4,10))%>%
    bg(bg="white",i=1,part="header")%>%  
    hline_top(border = fp_border_default(width = 0),part = "header")%>%
    vline_left(i=1,border = fp_border_default(width = 0),part = "header")%>%
    vline(i=1,border = fp_border_default(width = 0),part = "header")%>%
    align(align = "center", j = 2:11, part = "all") %>%
    width(j = 1:11, 2.8, unit = "in")%>%
    fontsize(size = 8,part = "all")%>%padding(padding = 0, part = "all")
  #
  return(flex_table_output)
}

demographic_table = demographic_table_fn()
## Printing of Demographic Table::::
doc = read_docx(paste0(getwd(),'/templates/demographic_table_template.docx'))
#
doc = headers_replace_text_at_bkm(doc,"country",site_name)
doc = headers_replace_text_at_bkm(doc,"year",paste0(lang_titles[21],' ',survey_year))
doc = headers_replace_text_at_bkm(doc,"Demographic",lang_titles[23])

#
doc=doc %>% cursor_bookmark(id  = "table1") %>%
  body_add_flextable(width(demographic_table, width = dim(demographic_table)$widths*10/(flextable_dim(demographic_table)$widths)), pos = "on", align = 'left')

print(doc,target=paste0(getwd(),'/reports/',survey_year,' ' ,site_name,' Demographic Table.docx')) 



# 
# 
# if (language =='ENGLISH')
# {
#   combined_table = cbind(c('','','Total', 'Age (Years)',levels(data$age_cat),'Missing','Grade', levels(data$DE_GRADE),'Missing'),
#                          rbind(c('Total','','','Male','','','Female','','','Missing'),
#                                c('N','Unweighted\nPercentage','Weighted\nPercentage',
#                                  'N','Unweighted\nPercentage','Weighted\nPercentage',
#                                  'N','Unweighted\nPercentage','Weighted\nPercentage',''),
#                                c(total_n[1],'','',total_n[2],'','',total_n[3],'','',total_n[4]),
#                                c('','','','','','','','','',''),
#                                cbind(unwgt_age_freq[,1],unwgt_age_perc[,1],wgt_age_perc[,1],
#                                      unwgt_age_freq[,2],unwgt_age_perc[,2],wgt_age_perc[,2],
#                                      unwgt_age_freq[,3],unwgt_age_perc[,3],wgt_age_perc[,3],unwgt_age_freq[,4]),
#                                rep('',10),
#                                cbind(unwgt_grade_freq[,1],unwgt_grade_perc[,1],wgt_grade_perc[,1],
#                                      unwgt_grade_freq[,2],unwgt_grade_perc[,2],wgt_grade_perc[,2],
#                                      unwgt_grade_freq[,3],unwgt_grade_perc[,3],wgt_grade_perc[,3],unwgt_grade_freq[,4])))
#   
#   colnames(combined_table) =c('','Total','','','Male','','','Female','','','Missing')
# } else if(language == 'FRENCH'){
#   combined_table = cbind(c('','','Total', 'Âge (ans)','12 ans ou moins','13 - 15','16 - 17','18 ans ou plus','Manquant','Classe', levels(data$DE_GRADE),'Manquant'),
#                          rbind(c('Total','','','Mâle','','','Femelle','','','Manquant'),
#                                c('N','Fréquence\n non pondérée','Pourcentage\n pondéré',
#                                  'N','Fréquence\n non pondérée','Pourcentage\n pondéré',
#                                  'N','Fréquence\n non pondérée','Pourcentage\n pondéré',''),
#                                c(total_n[1],'','',total_n[2],'','',total_n[3],'','',total_n[4]),
#                                c('','','','','','','','','',''),
#                                cbind(unwgt_age_freq[,1],unwgt_age_perc[,1],wgt_age_perc[,1],
#                                      unwgt_age_freq[,2],unwgt_age_perc[,2],wgt_age_perc[,2],
#                                      unwgt_age_freq[,3],unwgt_age_perc[,3],wgt_age_perc[,3],unwgt_age_freq[,4]),
#                                rep('',10),
#                                cbind(unwgt_grade_freq[,1],unwgt_grade_perc[,1],wgt_grade_perc[,1],
#                                      unwgt_grade_freq[,2],unwgt_grade_perc[,2],wgt_grade_perc[,2],
#                                      unwgt_grade_freq[,3],unwgt_grade_perc[,3],wgt_grade_perc[,3],unwgt_grade_freq[,4])))
#   
#   colnames(combined_table) =c('','Total','','','Mâle','','','Femelle','','','Manquant')
# } else if (language == 'SPANISH'){
#   combined_table = cbind(c('','','Total', 'Años de edad)','12 años o menos','13 - 15','16 - 17','18 años o más','Perdidos','Clase', levels(data$DE_GRADE),'Perdidos'),
#                          rbind(c('Total','','','Masculino','','','Femenino','','','Perdidos'),
#                                c('N','Frecuencia \nno ponderada','Porcentaje \nponderado',
#                                  'N','Frecuencia \nno ponderada','Porcentaje \nponderado',
#                                  'N','Frecuencia \nno ponderada','Porcentaje \nponderado',''),
#                                c(total_n[1],'','',total_n[2],'','',total_n[3],'','',total_n[4]),
#                                c('','','','','','','','','',''),
#                                cbind(unwgt_age_freq[,1],unwgt_age_perc[,1],wgt_age_perc[,1],
#                                      unwgt_age_freq[,2],unwgt_age_perc[,2],wgt_age_perc[,2],
#                                      unwgt_age_freq[,3],unwgt_age_perc[,3],wgt_age_perc[,3],unwgt_age_freq[,4]),
#                                rep('',10),
#                                cbind(unwgt_grade_freq[,1],unwgt_grade_perc[,1],wgt_grade_perc[,1],
#                                      unwgt_grade_freq[,2],unwgt_grade_perc[,2],wgt_grade_perc[,2],
#                                      unwgt_grade_freq[,3],unwgt_grade_perc[,3],wgt_grade_perc[,3],unwgt_grade_freq[,4])))
#   
#   colnames(combined_table) =c('','Total','','','Masculino','','','Femenino','','','Perdidos')
#   
# } else if (language =='RUSSIAN'){
#   combined_table = cbind(c('','','Общий', 'Возраст (лет)','12 лет или меньше','13 - 15','16 - 17','18 лет и старше','Потерянный','Сорт', levels(data$DE_GRADE),'Потерянный'),
#                          rbind(c('Общий','','','Мужской','','','Женский','','','Потерянный'),
#                                c('N','Невзвешенная\n частота','Взвешенный \nпроцент',
#                                  'N','Невзвешенная\n частота','Взвешенный \nпроцент',
#                                  'N','Невзвешенная\n частота','Взвешенный \nпроцент',''),
#                                c(total_n[1],'','',total_n[2],'','',total_n[3],'','',total_n[4]),
#                                c('','','','','','','','','',''),
#                                cbind(unwgt_age_freq[,1],unwgt_age_perc[,1],wgt_age_perc[,1],
#                                      unwgt_age_freq[,2],unwgt_age_perc[,2],wgt_age_perc[,2],
#                                      unwgt_age_freq[,3],unwgt_age_perc[,3],wgt_age_perc[,3],unwgt_age_freq[,4]),
#                                rep('',10),
#                                cbind(unwgt_grade_freq[,1],unwgt_grade_perc[,1],wgt_grade_perc[,1],
#                                      unwgt_grade_freq[,2],unwgt_grade_perc[,2],wgt_grade_perc[,2],
#                                      unwgt_grade_freq[,3],unwgt_grade_perc[,3],wgt_grade_perc[,3],unwgt_grade_freq[,4])))
#   
#   colnames(combined_table) =c('','Общий','','','Мужской','','','Женский','','','Потерянный')
#   
# } else{}
# 
# 
# 
