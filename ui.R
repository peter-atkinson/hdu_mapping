fluidPage(titlePanel("Heartworm stuff"),
          fluidRow(
            column(6,
                   wellPanel(h3("As of", (Sys.Date()-2), ", where can heartworm be transmitted?"),
                             br(),
                             plotOutput("binaryoutput"))),
            column(6,
                   wellPanel(h4(strong('What is happening in the capital cities?')),
                             tableOutput("capital.cities")))
          ),
          fluidRow(
            column(6,
                   wellPanel(img(src = "chdu2011sine.gif", 
                                 height = 600, width = 600)))
            ),
          fluidRow(
            column(12,
                   wellPanel((h3("Location")),
                             # br(),
                             textInput("postcode", "Enter your postcode:"),
                             # textOutput("location"),
                             textOutput("postcode"),),
                   plotOutput("locationplot")
                   )
            ),
          fluidRow(
           column(6,
                   wellPanel(dataTableOutput("cutofftable"))
            )
          )
)
