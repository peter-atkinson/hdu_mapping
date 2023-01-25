fluidPage(titlePanel("Heartworm stuff"),
          tags$head(
            tags$link(rel ="stylesheet", type = "text/css", href = "main.css")
          ),
          fluidRow(
            column(9, style = "margin-bottom: 50px;",
                   wellPanel(h3(textOutput("selecteddatemap"),
                             br(),
                             dateInput("dates", label=NULL, value = (Sys.Date()-2), min = min(dseq), max = max(dseq)),
                             plotOutput("binaryoutput", height=400, width="100%")))),
                   column(3,
                   wellPanel(h4(strong('What is happening in the capital cities?')),
                             tableOutput("capital.cities")))),
            fluidRow(
              column(12,
                     wellPanel((h3("Location")),
                               # br(),
                               textInput("postcode", "Enter your postcode:"),
                               textOutput("postcode"),),
                     div(class = "output-container",
                         plotOutput("locationplot")
                     )
              )
            ),
            fluidRow(
              column(6,
                     wellPanel(p(strong(paste(as.Date((Sys.Date()-2), format = "%d-%m-%Y"), "'s", " status:", sep="")),
                                 textOutput(("dailystatus")))),
                     wellPanel(dataTableOutput("cutofftable"))
              ),
              column(6,
                     wellPanel(dataTableOutput("percentagetable")))),
            fluidRow(
              column(3,
                   wellPanel(radioButtons(inputId = "summaryselection", 
                                          label=h4(strong("10 year summary of transmission zones")),
                                          #choices=c("1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2019"),
                                          choiceNames = c("1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2019", "summary GIF", "51-year summary"),
                                          choiceValues = c("1.7079sum.png", "2.8089sum.png", "3.9099sum.png", "4.0009sum.png", "5.1019sum.png", "summary.gif", "51yearsumm.png"),
                                          selected=NULL))),
              column(9,
                     wellPanel(imageOutput("summaryImage", height="auto", width="60%")))
                  )
         
)

