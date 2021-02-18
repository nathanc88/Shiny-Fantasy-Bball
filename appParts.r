overviewSection <-  function() {
  fluidPage(
    
    h2("Overview"), 
fluidRow(

  column(4,
         selectInput("posElig",
                     h5("Position:"),
                     c("All",
                       'PG', 'SG', 'SF', 'PF', 'C')
         )
  ),
  
  dataTableOutput("overviewTable")
)
)}





 depthSection <-  function() {
  fluidPage(
    
    h2("Depth Charts"),
   
    fluidRow(
      column(4,
             selectInput("team",
                         h5("Team:"),
                         c("All",
                           sort(unique(as.character(s2$tm)))))
      ),
      column(4,
             selectInput("depth",
                         h5("Depth:"),
                         selected = '1',
                         c("All",
                           as.character(sort(as.numeric(unique(s2$depth))))))
      ),
      column(4,
             selectInput("pos",
                         h5("Position:"),
                         c("All",
                           'PG', 'SG', 'SF', 'PF', 'C')
      )
    ),
    # Create a new row for the table.
    dataTableOutput("depthTable")
  )
)}

bios <-  function() {
   fluidPage(
     
     h2("Player Bios"),
     
     fluidRow(
       column(4,
              selectInput("team",
                          h5("Team:"),
                          c("All",
                            sort(unique(as.character(s2$tm)))))
       ),
       column(4,
              selectInput("depth",
                          h5("Depth:"),
                          c("All",
                            sort(unique(as.character(s2$depth)))))
       ),
       column(4,
              selectInput("pos",
                          h5("Position:"),
                          c("All",
                            'PG', 'SG', 'SF', 'PF', 'C')
              )
       ),

       dataTableOutput("bioTable")
     )
   )}

drafts <-  function() {
  fluidPage(
    
    h2("Draft Offers"),
    
    fluidRow(
      column(4,
             selectInput("team",
                         h5("Team:"),
                         c("All",
                           sort(unique(as.character(s2$tm)))))
      ),
      column(4,
             selectInput("depth",
                         h5("Depth:"),
                         c("All",
                           sort(unique(as.character(s2$depth)))))
      ),
      column(4,
             selectInput("pos",
                         h5("Position:"),
                         c("All",
                           'PG', 'SG', 'SF', 'PF', 'C')
             )
      ),
      
      dataTableOutput("draftTable")
    )
  )}

pct30Section <-  function() {
  fluidPage(
    
    h2("Per 30 Percentile"),
    
    fluidRow(

      
      dataTableOutput("pct30Table")
    )
  )}

zSection <-  function() {
  fluidPage(
    
    h2("Z Score Rankings"),
    
    fluidRow(
      
      
      dataTableOutput("zTable")
    )
  )}

pctHalf2Section <-  function() {
  fluidPage(
    
    h2("Second Half Percentile"),
    
    fluidRow(
      dataTableOutput("pctHalf2Table")
    )
)}

pctHalf1Section <-  function() {
  fluidPage(
    
    h2("First Half Percentile"),
    
    fluidRow(
      dataTableOutput("pctHalf1Table")
    )
  )}

pctSection <-  function() {
  fluidPage(
    
    h2("Full Season Percentile"),
    
    fluidRow(
      dataTableOutput("pctTable")
    )
  )
}


progressSection <- function(){
  sidebarLayout(
    sidebarPanel(width = 3,
                 checkboxGroupInput("columns",
                                    h3("Select Columns"),
                                    choices = unique(progress$variable),
                                    selected = unique(progress$variable),
                                    inline = T),  
                 br(),
                 selectInput("select", 
                             h3("Select Player"), 
                             choices = sort(unique(progress$name)), 
                             selected = 1) 
                 
    ),
    mainPanel(width = 9, 
              h3(style = "text-align: center","Season Tracker"), 
              dygraphOutput("dygraph")))  
  
}

exploreSection <- function(){
  fluidPage(
            fluidRow(
               column(
                      fluidRow(
                        column(width = 4,
                               selectInput("scatterD3_x", "x variable :",
                                           choices = c("MPG" = "mpg",
                                                       "Displacement" = "disp",
                                                       "Horsepower" = "hp",
                                                       "Rear axle ratio" = "drat",
                                                       "Weight" = "wt",
                                                       "1/4 mile time" = "qsec",
                                                       "Number Cyl" = "cyl_cat"),
                                           selected = "wt")
                        ), 
                        column(width =  4,
                               selectInput("scatterD3_y", "y variable :",
                                           choices = c("MPG" = "mpg",
                                                       "Displacement" = "disp",
                                                       "Horsepower" = "hp",
                                                       "Rear axle ratio" = "drat",
                                                       "Weight" = "wt",
                                                       "1/4 mile time" = "qsec",
                                                       "Number Cyl" = "cyl_cat"),
                                            selected = "mpg")      
                        ),
                        column(width = 4,
                               sliderInput("scatterD3_nb", 
                                           "Number of observations",
                                           min = 3, 
                                           max = nrow(mtcars), 
                                           step = 1, 
                                           value = nrow(mtcars))
                        )
                      ),
                      width = 6,
                      scatterD3Output("scatter1") 
               ),
               column(width = 6,
                      DT::dataTableOutput("scatter2"))
             )
      )
}

crossTalksection <- function(){

fluidPage(
  fluidRow(
    column(
      fluidRow(
        column(width = 4,
               selectInput("x2", "x variable :",
                           choices = colnames(pct30[, -1]),
                           selected = "ast")
        ), 
        column(width =  4,
               selectInput("y2", 
                           "y variable :",
                           choices = colnames(pct30[, -1]),                         
                           selected = "pts")      
        ) 
        # ,column(width =  4,
        #        selectInput("color2", "Color variable :",
        #                    choices = colnames(iris),                           
        #                    selected = "Species")      
        # )
      ),
      width = 6,
      d3scatterOutput("scatterA")
    ),
    column(width = 6,
           DT::dataTableOutput("scatterB")) 
  )
)
}  
  
  
mockDraftSection <- function(){
  sidebarLayout(
    sidebarPanel(
      width = 3,
      fluid = TRUE,
      selectInput(
           "pg1",
           h3(style = "text-align: center","PG 1"), 
           choices = sort(unique(select(filter(f,grepl("PG",Pos)),name))$name), 
           selected = sort(unique(select(filter(f,grepl("PG",Pos)),name))$name)[1]), 
      selectInput(
           "pg2",
           h3("PG 2"), 
           choices = sort(unique(select(filter(f,grepl("PG",Pos)),name))$name), 
           selected = sort(unique(select(filter(f,grepl("PG",Pos)),name))$name)[2]), 
      selectInput(
          "sg1",
          h3("SG 1"),
          choices = sort(unique(select(filter(f,grepl("SG",Pos)),name))$name), 
          selected = sort(unique(select(filter(f,grepl("SG",Pos)),name))$name)[2]),
      selectInput(
          "sg2",
          h3("SG 2"),
          choices = sort(unique(select(filter(f,grepl("SG",Pos)),name))$name), 
          selected = sort(unique(select(filter(f,grepl("SG",Pos)),name))$name)[1]) , 
      selectInput(
          "sf1",
          h3("SF 1"),
          choices = sort(unique(select(filter(f,grepl("SF",Pos)),name))$name),
          selected = sort(unique(select(filter(f,grepl("SF",Pos)),name))$name)[1]),
      selectInput(
          "sf2",
          h3("SF 2"),
          choices = sort(unique(select(filter(f,grepl("SF",Pos)),name))$name),
          selected = sort(unique(select(filter(f,grepl("SF",Pos)),name))$name)[2]) ,
      selectInput(
          "pf1",
          h3("PF 1"),
          choices = sort(unique(select(filter(f,grepl("PF",Pos)),name))$name),
          selected = sort(unique(select(filter(f,grepl("PF",Pos)),name))$name)[1]),
      selectInput(
          "pf2",
          h3("PF 2"),
          choices = sort(unique(select(filter(f,grepl("PF",Pos)),name))$name),
          selected =  sort(unique(select(filter(f,grepl("PF",Pos)),name))$name)[2]) ,
      selectInput(
          "c1",
          h3("C 1"),
          choices =  sort(unique(select(filter(f,grepl("C",Pos)),name))$name) ,
          selected = sort(unique(select(filter(f,grepl("C",Pos)),name))$name)[1]),
      selectInput(
          "c2",
          h3("C 2"),
          choices =  sort(unique(select(filter(f,grepl("C",Pos)),name))$name) ,
          selected =  sort(unique(select(filter(f,grepl("C",Pos)),name))$name)[2]),
      selectInput(
          "util",
          h3("Util"),              
          choices = sort(unique(select(f ,name))$name),
          selected = sort(unique(select(f ,name))$name)[11]),       
      selectInput(
        "sub1",
        h3("Sub 1"),
        choices = sort(unique(select(f,name))$name),
        selected = sort(unique(select(f,name))$name)[12]),
      selectInput(
        "sub2",
        h3("Sub 2"),
        choices = sort(unique(select(f,name))$name),
        selected = sort(unique(select(f,name))$name)[13]),
      selectInput(
        "sub3",
        h3("Sub 3"),
        choices = sort(unique(select(f,name))$name),
        selected = sort(unique(select(f,name))$name)[14]),
      selectInput(
        "sub4",
        h3("Sub 4"),
        choices = sort(unique(select(f,name))$name),
        selected = sort(unique(select(f,name))$name)[15]),
      
      
      ),
    mainPanel(width = 9, 
              h3(style = "text-align: center","Mock Draft"), 
              DT::dataTableOutput("mockDraftTable"))
    )  
  
}

