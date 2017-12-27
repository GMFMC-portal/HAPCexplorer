## ui.R
## November 12, 2015
## September 11, 2015
## version 0.33
## version 0.33
##version 0.31

ui <- dashboardPage(skin="black", 
  dashboardHeader(title = "", disable=TRUE),
                
  dashboardSidebar(disable = FALSE,
                   sidebarMenu(####includeHTML("modalbutton.html"),########
                     ## List of icons: http://fontawesome.io/icons/  "fa fa-" prefix no longer necessary 
                     menuItem("Map", tabName = "distribution", icon = icon("globe"),selected=TRUE),#, badgeLabel="Updated"),
                     menuItem("HAPCs with fishing regulations", tabName = "table", icon = icon("table")),
                     menuItem("HAPCs without fishing regulations", tabName = "table2", icon = icon("table")),
                     menuItem("Recommended HAPCs", tabName = "table3", icon = icon("table")),
                     menuItem("Coral locations", tabName = "table4", icon = icon("table"))
                     
                     #menuItem("How to: coming soon", tabName = "howto", icon = icon("fa fa-info"))
                   ),
                   div(tags$a(style="color: #21b0a5;", href="http://portal.gulfcouncil.org/Regulations/Recommended_HAPCs_WGS84.zip", 
                              "Download Recommended HAPCs"), align="center"),
                   #(img(src="logo.jpg", height=150, width=150)),
                   br(),
                   div(img(src="expl7292.png", height=517, width=200), style="text-align: center;"),
                   #div(img(src="coral3.jpg", height=239, width=200), style="text-align: center;"),
                   div(tags$p()), ## space between pictures
                   #div(img(src="logo.jpg", height=150, width=150), style="text-align: center;"),
                   div(tags$p()), ## space between pictures
                   div(tags$a(href="mailto: portal@gulfcouncil.org", "Contact us"), align="center"),
                   
                   
                   br(),
                   br(),
                   div(tags$a(href = "https://coralreef.noaa.gov/", target="_blank",img(src="landscape_white.png", style="max-width: 200px;"), style="text-align: center;")),
                   div(tags$p(style="color: #b8c7ce;margin:0;","Version 1.1")),
                   div(tags$p(style="color: #b8c7ce; margin:0;","Updated 6-Dec-2017"))
  ),
  
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "distribution",includeHTML('modalHTML3.html'),
              includeScript('modalJS.js'),
              div(class="outer",
                  tags$head(
                    includeCSS("styles.css")
                  )), ##This was added version 0.52. 
              fluidRow(
                column(width = 12,
                       box(width=NULL, solidHeader = TRUE,
                           tags$img(src="HAPCViewerBanner.png",  width="100%"),
#                            tags$style(type="text/css", "h2 { color: #708090;font-style: normal; }"),
#                            h2("Gulf of Mexico Habitat Areas of Particular Concern"), 
#                            #style = "background-color: #F0F0F0;",
#                            style= "font-family: 'Lobster', cursive",
#                            style = "color: #708090;",
                           leafletOutput("map", height=725)#,
                           #h4("Click on map to display coordinates")#,
                           #verbatimTextOutput("out"),
                           #verbatimTextOutput("out2"),
                           ##add absolute panel
                           # absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                           #               draggable = TRUE, top = 170, left = "auto", right = 20, bottom = "auto",
                           #               # width = 330, height = "auto",
                           #               width = 350, height = "auto",
                           #               plotOutput("HAPCarea", height = 250),
                           #               div(h5("*Dashed line (---) indicates total area of current and recommended HAPCs in the Gulf of Mexico"), align="center")
                           #              
                           # )#,
                           #exp
#                            absolutePanel(top = 150, right = 10, draggable=TRUE,
#                                         
#                                          checkboxInput("legend", "Show legend", TRUE)
#                            )
                          
                           
                           ##exo
                           
                           
                           
                           ##add absolute panel
                           )#box,
                       ## See AbsolutePanel
                        ) #column
                      ) ##fluidRow
                ),#tabItem
      tabItem(tabName = "table",
              fluidRow(
                   column(width = 12,
                          box(width=NULL, solidHeader = TRUE,
                              h2("HAPCs with bottom-contact gear restrictions"),
                              DT::dataTableOutput('tbl')
                              ) #box
                         ) #column
                      ) #fluid row
             ), ##tabName "table"
      #################
      tabItem(tabName = "table2",
              fluidRow(
                   column(width = 12,
                          box(width=NULL, solidHeader = TRUE,
                              h2("HAPCs with no specific fishing regulations"),
                              DT::dataTableOutput('tbl2')
                          ) #box
                   ) #column
              ) #fluid row
      ),
      
      ###########
      
      tabItem(tabName = "table3",
              fluidRow(
                   column(width = 12,
                          box(width=NULL, solidHeader = TRUE,
                              h2("Recommended HAPCs with proposed bottom-contact gear restrictions"),
                              DT::dataTableOutput('tbl3')
                          ) #box
                   ) #column
              ) #fluid row
      ),
      
      tabItem(tabName = "table4",
              fluidRow(
                column(width = 12,
                       box(width=NULL, solidHeader = TRUE,
                           h2("Records of known coral locations"),
                           DT::dataTableOutput('tbl4')
                       ) #box
                ) #column
              ) #fluid row
      ),
tabItem(tabName = "welcome",
                fluidRow(
                  column(width = 12,
                         box(width=NULL, solidHeader = TRUE,
                            #h2("About this page"),
                             includeMarkdown("about/about.Rmd")
                            #includeHTML("about.html")
                         ) #box
                  ) #column
                ) #fluid row
      )#,
   


# tabItem(tabName = "howto",
#         fluidRow(
#           column(width = 12,
#                  box(width=NULL, solidHeader = TRUE,
#                       #h2("Coming soon"),
#                      includeMarkdown("howto/howto.Rmd")
#                  ) #box
#           ) #column
#         ) #fluid row
# )   
     
          )#tabItems
    
  )#dashboardBody
)#dashboardPage