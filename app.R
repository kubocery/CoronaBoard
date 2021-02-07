library(shinydashboard)
library(plotly)

source('coronaplot4.R')
source('SEIR2.R')

a1 <- list('None'='None')
inp <- list('China'='China','Hubei'='Hubei','Japan' ='Japan','South Korea'='Korea','USA'='US',
            'France'='France','Canada'='Canada','Australia' = 'Australia','Germany'='Germany','Italy'='Italy',
            'United Kingdom'='United Kingdom','Russia'='Russia','Sweden'='Sweden','Spain'='Spain','Belgium'='Belgium','Iran' ='Iran',
            'Croatia'='Croatia','Switzerland'="Switzerland",'Austria'='Austria','Israel'='Israel','Greece'='Greece',
            'Norway'='Norway','Finland'='Finland','Portugal'= 'Portugal','Netherlands'='Netherlands','Poland'='Poland',
            'Slovakia'='Slovakia','Czech rep.'='Czechia','Hungary'='Hungary','Slovenia'='Slovenia','Serbia'='Serbia',
            'Romania'='Romania','Ukraine'='Ukraine','Estonia'='Estonia','Latvia'='Latvia','Lithuania'='Lithuania',
            'Denmark'='Denmark')
inp3 <- inp[order(names(inp))][c(2,1,3:length(names(inp)))]
inp2 <- c(a1,inp[order(names(inp))])

header <- dashboardHeader(title = "Coronaboard v1.0")

sidebar <- dashboardSidebar(
  sidebarMenu(id='menu',
              menuItem('Historical Data', tabName = 'Corona'),
              menuItem('Map - live data', tabName = 'Map'),
              menuItem('Model | Beta', tabName = 'Model'))
)

bodys <- dashboardBody(
  # Boxes need to be put in a row (or column)
tabItems(
  tabItem(tabName = 'Corona',
  shiny::fluidRow(column(width = 3, 
                         shinydashboard::box(width =NULL,height=636,shiny::selectInput('c1','Select Country:',
                                                                            inp3),
                                             shiny::selectInput('c2',label = NULL,
                                                                inp2),
                                             shiny::selectInput('c3',label = NULL,
                                                                inp2),
                                             shiny::selectInput('c4',label = NULL,
                                                                inp2),
                                             shiny::selectInput('c5',label = NULL,
                                                                inp2),
                                             shiny::selectInput('c6',label = NULL,
                                                                inp2),
                                             shiny::selectInput('c7',label = NULL,
                                                                inp2),
                                             shiny::selectInput('c8',label = NULL,
                                                                inp2),
                                             shiny::selectInput('c9',label = NULL,
                                                                inp2),
                                             shiny::selectInput('c10',label = NULL,
                                                                inp2),
                                            shiny::div(style="display: flex;align-items: center;justify-content: center;" ,
                                                       shinyWidgets::switchInput(inputId = "logsc", label = "Log scale", size = 'mini', value = FALSE))
                         )),
                  shiny::column(width = 9,
                                shinydashboard::box(width = NULL,
                                                    title = "Disclaimer",
                                                    '- Active cases numbers are vastly depenent on the ability and willingness to test patients',br(),
                                                    '- The data ', a(href='https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series','source'),
                                                    'from John Hopkins University has been ',a(href='https://github.com/CSSEGISandData/COVID-19/issues','inconsistent'),br(),
                                                    '- Please wait a couple of seconds for the graphs to load ', br(),
                                                    '- If the selected country is not in the graph, it did not cross the 1 case per million threshold'
                                                    
                                ), 
                                shiny::tabsetPanel(type = 'tabs',
                                                  shiny::tabPanel(title = "Active cases", plotlyOutput('corplot1')),
                                                  shiny::tabPanel(title = "Active cases smoothed", plotlyOutput('corplot5')),
                                                  shiny::tabPanel(title = "New cases",plotlyOutput('corplot4')),
                                                  shiny::tabPanel(title = "New cases smoothed",plotlyOutput('corplot3')),
                                                  shiny::tabPanel(title = "Data download", "Data used for the graphs: ",br(),downloadButton("downloadData", "Download"))
                                )
                  )
                  
                  
                  
  )
),#closes 1st tab
tabItem(tabName = 'Map',
shiny::fluidRow(width = 12,
              shinydashboard::box(width = NULL,
                                  title = "Disclaimer",
                                  '- Active cases numbers are vastly depenent on the ability and willingness to test patients',br(),
                                  '- The data is scraped from', a(href='https://www.worldometers.info/coronavirus/',' worldometers.info'), br(),
                                  '- If  country is not on the map, the data is not available'
                                  
              ),
              shinydashboard::box(width=NULL,height = 600,plotlyOutput('corplot2', height = 550))
              )),


tabItem(tabName = 'Model',
        shiny::fluidPage(includeScript("slider.js"),
        shiny::fluidRow(height=900, column(width = 3, height=800,
                               shinydashboard::box(width =NULL,height=800,
                                                   shiny::sliderInput(inputId = 'R0', label = div(HTML('Reproduction number R<sub>0</sub>')),
                                                                      min=1.5, max=4, value = 2.5, step = 0.5),
                                                   shiny::sliderInput(inputId = 'pop', label = div(HTML('Population')),
                                                                      min=100000, max=10000000, value = 5000000, step = 100000),
                                                   #shiny::sliderInput(inputId = 'startpop', label = div(HTML('Starting population with infection')),
                                                  #                   min=0.5, max=7, value = 3.5, step = 0.5),
                                                   shiny::sliderInput(inputId = 'days', label = div(HTML('Time span (days)')),
                                                                      min=50, max=750, value = 200, step = 50),
                                                   #shiny::sliderInput(inputId = 'rho', label = div(HTML('Countermeasures taken')),
                                                  #                    min=0.2, max=1, value = 1, step = 0.01),
                                                  div(class="my_slider", sliderInput("rho",
                                                                  "Social distancing:", ticks = F,
                                                                  min = 0, max = 0.8, value = 0, step=0.01)),
                                                   shiny::sliderInput(inputId = 'gamma', label = div(HTML('Average latent time')),
                                                                      min=1, max=10, value = 3, step = 0.1),
                                                   shiny::sliderInput(inputId = 'delta', label = div(HTML('Average quarantine time')),
                                                                      min=1, max=7, value = 3.5, step = 0.5),
                                                   shiny::sliderInput(inputId = 'death', label = div(HTML('Deathrate')),
                                                                      min=0.1, max=10, value = 3, step = 0.1)
                                                  
                               )),
                        shiny::column(width = 9,
                                      shinydashboard::box(width = NULL,
                                                          title = "Disclaimer",
                                                          '- This is an (unprofessional) implementation of', 
                                                          a(href='https://arxiv.org/pdf/2002.06563.pdf', 'SEIR'),
                                                          ' model', br(),
                                                          '- Exposed patiens are infected but not yet be infectious, in a latent period'
                                                          
                                      ), 
                                      shinydashboard::box(width = NULL,
                                                          #plotlyOutput('modplot1',height = 350),
                                                          plotlyOutput('modplot2',height = 650))
                        )
                        
                        
                        
        )))

)
#closes tab items
#closes tab items


)#closes body

ui <- dashboardPage(header, sidebar, bodys)


server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$corplot1 <- plotly::renderPlotly({
    places <- c(input$c1, input$c2,input$c3,input$c4,input$c5,input$c6,input$c7,input$c8,input$c9,input$c10)
    coronaplot(places, logscale=input$logsc, smooth=FALSE)
  })
  
  output$corplot5 <- plotly::renderPlotly({
    places <- c(input$c1, input$c2,input$c3,input$c4,input$c5,input$c6,input$c7,input$c8,input$c9,input$c10)
    coronaplot(places, logscale=input$logsc, smooth=TRUE)
  })
  
  output$corplot2 <- plotly::renderPlotly({
    #places <- c(input$c1, input$c2,input$c3,input$c4,input$c5,input$c6,input$c7,input$c8,input$c9,input$c10)
    coronamap()
  })
  output$corplot3 <- plotly::renderPlotly({
    places <- c(input$c1, input$c2,input$c3,input$c4,input$c5,input$c6,input$c7,input$c8,input$c9,input$c10)
    new_cases_smooth(places, logscale=input$logsc)
  })
  output$corplot4 <- plotly::renderPlotly({
    places <- c(input$c1, input$c2,input$c3,input$c4,input$c5,input$c6,input$c7,input$c8,input$c9,input$c10)
    new_cases_raw(places, logscale=input$logsc)
  })
  
  #output$modplot1 <- plotly::renderPlotly({
  #  model_fn(input$R0, input$pop, input$days,1-input$rho, input$gamma, input$delta, input$death/100, cum=F)
  #})
  output$modplot2 <- plotly::renderPlotly({
    model_fn(input$R0, input$pop, input$days,1-input$rho, input$gamma, input$delta, input$death/100, cum=T)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data',lubridate::today(), ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(dfs, file)
    }
  )
}

shinyApp(ui, server)
