#sets the user interface
#performed after global.R

navbarPage(strong("Heartworm in Australia"),
           tabPanel(("Main Page"),
                    fluidPage(titlePanel("Heartworm stuff"),
                      fluidRow(
                        column(6, 
                               wellPanel(img(src = "bchdu.png", 
                                             height = 500, width = 960))),
                        column(6,
                               wellPanel(h4(strong('What is happening in the capital cities?')),
                                 tableOutput("capital_cities")))
                      ),
                      fluidRow(
                        column(6,
                               wellPanel(img(src = "chdu2011sine.gif", 
                                             height = 600, width = 600))),
                        column(6,
                               wellPanel((h3("Location")),
                                         br(),
                                         textInput("location", "Enter your location:"),
                                         textInput("postcode", "Enter your postcode:"),
                                         textOutput("location"),
                                         textOutput("postcode")))
                      )
                    )
           ),
           tabPanel(strong(em("About")),
                    sidebarLayout(
                      sidebarPanel(h4(strong("Canine heartworm")),
                                   p('heartworm_info')),
                      mainPanel(
                        img(src = "Diro_Pulmonary_LifeCycle_lg.jpg", height = 400, width = 400)
                      )
                    )
           ),
           tabPanel(strong(em("Australia-wide")),
                    sidebarLayout(
                      sidebarPanel(h4(strong("About this map")),
                                   p('map_info')),
                      mainPanel(
                        img(src = "chdu2011sine.gif", height = 600, width = 600)
                      )
                    )
           ),
           tabPanel(strong(em("My Location")),
                    sidebarLayout(
                      sidebarPanel(
                        (h3("Location")),
                        br(),
                        textInput("location", "Enter your location:"),
                        textInput("postcode", "Enter your postcode:"),
                        textOutput("location"),
                        textOutput("postcode")
                      ),
                      mainPanel(
                        dateRangeInput("dates", h3("Date range"), start = min(dseq), end = max(dseq)
                        ),
                        tableOutput("location.table"),
                        #tableOutput("cutoff.table")
                      )
                    )
                    ),
           tabPanel(em("Contact Us"),
                    textInput("text", h3("Leave us a comment"),
                              value="We love your feedback..."))
)
