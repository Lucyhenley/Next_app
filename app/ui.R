
ui <- fluidPage(
  
  titlePanel(h1("Maximum capacity of Next office plans: Trial",align="centre")),
  

  
  wellPanel(uiOutput("tab",style = "font-size:15px;")),
  
  fluidRow(
    
    
    
     column(3,      wellPanel(sliderInput("SocialDistance", h4("Social distancing rule (m)", align = "center"),
                                                          min = 1, max = 2, value = 2,width='100%'))

                     
                     
    )
    

    
    
  ),
  

  
#  fluidRow(
#    column(8, h1("ediuhe")),
#    column(4,textOutput("emissionstext")       )
#  ),
  
 
  
  fluidRow(
    column(7,
           headerPanel(""),
           headerPanel(""),
           plotOutput("subplots", width = "100%", height = "1600px")
           ), 

    
    
    column(5,
           h3("First floor office"),
           img(src="firstfloorplan2.png",width="650", height="450",align="centre")
           , align="center"
    )

    
  )
  
  #  fluidRow(
  #    column(8, h1("ediuhe")),
  #    column(4,textOutput("emissionstext")       )
  #  ),
  
  # fluidRow(
  #  column(4),
  # column(4,      h1("Train plan", align = "center"),
  #       img(src="train_floorplan.png",width="600", height="150")
  #)
  #  )
  
)

