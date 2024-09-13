convertMenuItem <- function(mi,tabName) {
    mi$children[[1]]$attribs['data-toggle']="tab"
    mi$children[[1]]$attribs['data-value'] = tabName
    mi
}

fqiHeader <- dashboardHeader(
    title = 'Survey Tools',    tags$li(a(href = 'https://www.who.int/teams/noncommunicable-diseases/surveillance/systems-tools/global-school-based-student-health-survey','Global school-based student health survey',
                                      title = "GSHS", width ='1000px'),
                                    class = "dropdown"),
    
    
    tags$li(a(href = 'https://www.who.int/teams/noncommunicable-diseases/surveillance/systems-tools/global-school-based-student-health-survey',
              img(src = 'logo_new.png', width = "150px",
                  title = "GSHS", height = "40px"),
              style = "padding-top:5px; padding-bottom:5px; margin-left: 5px"),
            class = "dropdown"))

fqiSidebar <- dashboardSidebar(uiOutput("sidebar"),

  tags$head(tags$style(HTML('
.box {margin-top:1px;margin-bottom:1px;margin-left:1px;margin-right:1px;}
div {padding: 1px !important;}'
))),
tags$head(tags$style(HTML(".custom-column {height: 35px;margin-bottom: 15px;margin-top: 0px;}"))
),
tags$head(tags$style(HTML('* {font-family: "Source Sans Pro"};'))),
tags$style(type="text/css", "#q1_pager {background-color:#00205C;color: #00205C;font-family: Source Sans Pro}")
)

##################################################
css <- "
#reverseSlider .irs-bar {
    border-top: 1px solid #ddd;
    border-bottom: 1px solid #ddd;
    background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
}
#reverseSlider .irs-bar-edge {
    border: 1px solid #ddd;
    background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
    border-right: 0;
}
#reverseSlider .irs-line {
    background: #428bca;
    border: 1px solid #428bca;
}
"
fqiBody <- dashboardBody(id='main_body',shinyjs::useShinyjs(),
                        shinyauthr::loginUI("login"),uiOutput("body_output"),
                        tags$head(tags$style(HTML('
                          .box {margin-top:1px;margin-bottom:1px;margin-left:1px;margin-right:1px;}
                          div {padding: 1px !important;}'
                        ))),
                        tags$head(tags$style(HTML(".custom-column {height: 35px;margin-bottom: 15px;margin-top: 0px;}"))),
                        tags$head(tags$style(HTML('* {font-family: "Source Sans Pro"};'))),
                        tags$style(type="text/css", "#q1_pager {background-color:blue;color: black;font-family: Source Sans Pro}"),
                        tags$style(type="text/css",
                          ".shiny-output-error { visibility: hidden; }",
                          ".shiny-output-error:before { visibility: hidden; }"
                         )) 

shinyUI(
    dashboardPage(
        fqiHeader,
        fqiSidebar,
        fqiBody, skin = 'black'
    )
)


