library(shiny)

shinyUI( pageWithSidebar(
    headerPanel("The aftermath of recent 3 air crashes : Are flight journeys safe?"),
    sidebarPanel(
        h4("Please select the visualization parameter to generate graph"),
        selectInput("variable", "Parameter:", c("","Year", "Month", "Category")),
        submitButton("Submit")
    ),
    mainPanel(
        p("This is a application to visualize the trend in the aviation safety over the past 10 years (Data Source : Aviation Safety Network). With an interactive interface, the user can choose to visualize the information from different perspectives."),
        plotOutput('plotdiag'),
        verbatimTextOutput("comments"),
        verbatimTextOutput("documentation")
    )
)

)