fluidPage(titlePanel("Heartworm stuff"),
          fluidRow(
            column(6,
                   wellPanel(h3(textOutput("selecteddatemap"),
                             br(),
                             dateInput("dates", label=NULL, value = (Sys.Date()-2), min = min(dseq), max = max(dseq)),
                             plotOutput("binaryoutput", height=700, width=850))),
                   br(),
                   br(),
                   br(),
                   br()),
            column(6,
                   wellPanel(h4(strong('What is happening in the capital cities?')),
                             tableOutput("capital.cities"))),
            column(6,
                   wellPanel(img(src = "10 year plot.png", 
                                 height = 700, width = 750)))
          ),
          fluidRow(
            column(12,
                   wellPanel((h3("Location")),
                             # br(),
                             textInput("postcode", "Enter your postcode:"),
                             textOutput("postcode"),),
                   plotOutput("locationplot")
                   )
            ),
          fluidRow(
           column(6,
                  wellPanel(p(strong(paste(as.Date((Sys.Date()-2), format = "%d-%m-%Y"), "'s", " status:", sep="")),
                              textOutput(("dailystatus")))),
                  wellPanel(dataTableOutput("cutofftable"))
            ),
           column(6,
                  wellPanel(dataTableOutput("percentagetable")))
          )
)

