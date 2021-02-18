# server function
server <- function(input, output) {

# Tables ----        

  #   Overview  ----
  output$overviewTable <- DT::renderDataTable(DT::datatable({

    sFull <-
      s %>% 
      group_by(name) %>% 
      summarise(depth = min(depth) 
                 ) %>% 
      ungroup()
    
    data <- 
      sFull %>% 
      left_join(.,s2, by = c("name", "depth" )) %>% 
      mutate_at(vars( c("depth")),list(~as.integer(.))) %>%  
      select(name,depth, tm, pos, posElig, NewOffer, contains("rank"),zTot, Age, NewTeam, LastTm, NewStarter,  LastOffer,OfferChg, Age, everything() ) %>% 
      select(-pos,-contains("posRank")) %>% 
      arrange(rank)
     
    if (input$posElig != "All") {
      data <- filter(data, grepl(input$posElig, posElig)) %>% arrange(rank)}
    data %>% arrange(rank)
  }, 
  rownames = F, 
  filter = 'top', 
  extensions = c('Buttons',  'ColReorder', 'FixedColumns','Scroller' ), 
  options = list(
    dom = 'Bfrtip',
    buttons =
      list('copy', 'print', list(
        extend = 'collection',
        buttons = list(
          list(extend = 'csv', filename = "Overview"),
          list(extend = 'excel', filename = "Overview"),
          list(extend = 'pdf', filename = "Overview")),
        text = 'Download'
      )),
    pageLength = 25,
    colReorder = TRUE ,
    scrollX = TRUE,
    deferRender = TRUE,
    scrollY = 500,
    scroller = TRUE,
    fixedColumns = list(leftColumns = 1),
    columnDefs = list(list(className = 'dt-left',targets = "_all"))
  )
  )) 
  
  
   
        #   Depth Charts  ----
    output$depthTable <- DT::renderDataTable(DT::datatable({
        data <- s %>% 
                select(name, tm, pos , depth)
        
        if (input$team != "All") {
            data <- data[data$tm == input$team,]
        }
        if (input$pos != "All") {
            data <- data[data$pos == input$pos,]
        }
        if (input$depth != "All") {
            data <- data[data$depth == input$depth,]
        }
        data
    }, rownames = F, 
    filter = 'top', 
    extensions = c('Buttons',  'ColReorder', 'FixedColumns','Scroller' ), 
    options = list(
        dom = 'Bfrtip',
        buttons =
            list('copy', 'print', list(
                extend = 'collection',
                buttons = list(
                    list(extend = 'csv', filename = "Depth Charts"),
                    list(extend = 'excel', filename = "Depth Charts"),
                    list(extend = 'pdf', filename = "Depth Charts")),
                text = 'Download'
            )),
        pageLength = 25,
        colReorder = TRUE ,
        scrollX = TRUE,
        deferRender = TRUE,
        scrollY = 500,
        scroller = TRUE,
        fixedColumns = list(leftColumns = 1),
        columnDefs = list(list(className = 'dt-left',targets = "_all"))
    )
    )) 
    
    #   Bio  ----
    output$bioTable <- DT::renderDataTable(DT::datatable({
        data <- b %>% 
            select(Name = name, Pos , Exp, Age, In = IN, Wt = WT, BMI, 
                   `Body Type` = body, `Draft Year` = draftYr, Round = round,
                   `Pick Overall` = pickOvr, PreNBA, everything()) 
        data
    }, rownames = F, 
    filter = 'top', 
    extensions = c('Buttons',  'ColReorder', 'FixedColumns','Scroller' ), 
    options = list(
        dom = 'Bfrtip',
        buttons =
            list('copy', 'print', list(
                extend = 'collection',
                buttons = list(
                    list(extend = 'csv', filename = "Player Bios"),
                    list(extend = 'excel', filename = "Player Bios"),
                    list(extend = 'pdf', filename = "Player Bios")),
                text = 'Download'
            )),
        pageLength = 25,
        colReorder = TRUE ,
        scrollX = TRUE,
        deferRender = TRUE,
        scrollY = 500,
        scroller = TRUE,
        fixedColumns = list(leftColumns = 1),
        columnDefs = list(list(className = 'dt-left',targets = "_all"))
    )
    )) 
    
    
    # Draft Section ----
    output$draftTable <- DT::renderDataTable(DT::datatable({
        data <- s2 %>% 
            select(Name=name,Team = tm,Pos= pos,Depth= depth,`New Offer`=NewOffer,
                   `Last Offer`=LastOffer,`Offer Change`=  OfferChg)
        
        if (input$team != "All") {
            data <- data[data$Team == input$team,]
        }
        if (input$pos != "All") {
            data <- data[data$Pos == input$pos,]
        }
        if (input$depth != "All") {
            data <- data[data$Depth == input$depth,]
        }
        data
    }, rownames = F, 
    filter = 'top', 
    extensions = c('Buttons',  'ColReorder', 'FixedColumns','Scroller' ), 
    options = list(
        dom = 'Bfrtip',
        buttons =
            list('copy', 'print', list(
                extend = 'collection',
                buttons = list(
                    list(extend = 'csv', filename = "Draft Draft"),
                    list(extend = 'excel', filename = "Draft Offer"),
                    list(extend = 'pdf', filename = "Draft Offer")),
                text = 'Download'
            )),
        pageLength = 25,
        colReorder = TRUE ,
        scrollX = TRUE,
        deferRender = TRUE,
        scrollY = 500,
        scroller = TRUE,
        fixedColumns = list(leftColumns = 1),
        columnDefs = list(list(className = 'dt-left',targets = "_all"))
    )
    )) 
    # Per30 Pct ----
    output$pct30Table <- DT::renderDataTable(DT::datatable({
        data <- pct30 %>%
            select(-c(OFFER, Draft, ratio, pos)) %>% 
            select(name,everything())
        
        data
    }, rownames = F, 
    filter = 'top', 
    extensions = c('Buttons',  'ColReorder', 'FixedColumns','Scroller' ), 
    options = list(
        dom = 'Bfrtip',
        buttons =
            list('copy', 'print', list(
                extend = 'collection',
                buttons = list(
                    list(extend = 'csv', filename = "Per 30 Pct"),
                    list(extend = 'excel', filename = "Per 30 Pct"),
                    list(extend = 'pdf', filename = "Per 30 Pct")),
                text = 'Download'
            )),
        pageLength = 25,
        colReorder = TRUE ,
        scrollX = TRUE,
        deferRender = TRUE,
        scrollY = 500,
        scroller = TRUE,
        fixedColumns = list(leftColumns = 1),
        columnDefs = list(list(className = 'dt-left',targets = "_all"))
    )
    )) 
  
    # Z Scores  ----
    output$zTable <- DT::renderDataTable(DT::datatable({
        data <- z %>%
            select(-c(OFFER, Draft, ratio, pos)) %>% 
            select(name,everything())
        
        data
    }, rownames = F, 
    filter = 'top', 
    extensions = c('Buttons',  'ColReorder', 'FixedColumns','Scroller' ), 
    options = list(
        dom = 'Bfrtip',
        buttons =
            list('copy', 'print', list(
                extend = 'collection',
                buttons = list(
                    list(extend = 'csv', filename = "Z Scores"),
                    list(extend = 'excel', filename = "Z Scores"),
                    list(extend = 'pdf', filename = "Z Scores")),
                text = 'Download'
            )),
        pageLength = 25,
        colReorder = TRUE ,
        scrollX = TRUE,
        deferRender = TRUE,
        scrollY = 500,
        scroller = TRUE,
        fixedColumns = list(leftColumns = 1),
        columnDefs = list(list(className = 'dt-left',targets = "_all"))
    )
    ))

    # 1st Half Pct  ----
    output$pctHalf1Table <- DT::renderDataTable(DT::datatable({
        data <- pctHalf1 %>%
            select(name,everything())
        
        data
    }, rownames = F, 
    filter = 'top', 
    extensions = c('Buttons',  'ColReorder', 'FixedColumns','Scroller' ), 
    options = list(
        dom = 'Bfrtip',
        buttons =
            list('copy', 'print', list(
                extend = 'collection',
                buttons = list(
                    list(extend = 'csv', filename = "First Half Pct"),
                    list(extend = 'excel', filename = "First Half Pct"),
                    list(extend = 'pdf', filename = "First Half Pct")),
                text = 'Download'
            )),
        pageLength = 25,
        colReorder = TRUE ,
        scrollX = TRUE,
        deferRender = TRUE,
        scrollY = 500,
        scroller = TRUE,
        fixedColumns = list(leftColumns = 1),
        columnDefs = list(list(className = 'dt-left',targets = "_all"))
    )
    ))
    
    # 2nd Half Pct  ----
    output$pctHalf2Table <- DT::renderDataTable(DT::datatable({
        data <- pctHalf2 %>%
            select(  name,everything())
        
        data
    }, rownames = F, 
    filter = 'top', 
    extensions = c('Buttons',  'ColReorder', 'FixedColumns','Scroller' ), 
    options = list(
        dom = 'Bfrtip',
        buttons =
            list('copy', 'print', list(
                extend = 'collection',
                buttons = list(
                    list(extend = 'csv', filename = "Second Half Pct"),
                    list(extend = 'excel', filename = "Second Half Pct"),
                    list(extend = 'pdf', filename = "Second Half Pct")),
                text = 'Download'
            )),
        pageLength = 25,
        colReorder = TRUE ,
        scrollX = TRUE,
        deferRender = TRUE,
        scrollY = 500,
        scroller = TRUE,
        fixedColumns = list(leftColumns = 1),
        columnDefs = list(list(className = 'dt-left',targets = "_all"))
    )
    ))
    
    # Full Season Pct  ----
    output$pctTable <- DT::renderDataTable(DT::datatable({
        data <- pct  %>%
            select(-c(OFFER, Draft, ratio, pos)) %>% 
            select(name,everything())
        
        data
    }, rownames = F, 
    filter = 'top', 
    extensions = c('Buttons',  'ColReorder', 'FixedColumns','Scroller' ), 
    options = list(
                   dom = 'Bfrtip',
                   buttons =
                       list('copy', 'print', list(
                           extend = 'collection',
                           buttons = list(
                               list(extend = 'csv', filename = "Full Season Pct"),
                               list(extend = 'excel', filename = "Full Season Pct"),
                               list(extend = 'pdf', filename = "Full Season Pct")),
                           text = 'Download'
                       )),
                   pageLength = 25,
                   colReorder = TRUE ,
                   scrollX = TRUE,
                   deferRender = TRUE,
                   scrollY = 500,
                   scroller = TRUE,
                   fixedColumns = list(leftColumns = 1),
                   columnDefs = list(list(className = 'dt-left',targets = "_all"))
    )
    ))
    
    # Progress TS ----
    progTS <- reactive({
        prog2 <- progress %>% 
            filter(variable %in% input$columns, 
                   name == input$select ) %>% 
            group_by(variable) %>% 
            arrange(dt) %>% 
            complete(dt = seq.Date(min(dt), as.Date('2020-11-01') , by="day")) %>% 
            fill(c(name,value, rollAvg, rollRank), .direction = "down" ) %>% 
            ungroup() %>% 
            select(dt, rollRank, variable ) %>% 
            spread(variable,rollRank )
        
          xts(prog2, order.by = as.Date(prog2$dt , "%m/%d/%Y"))
    })
    
    
    output$dygraph <- renderDygraph({
            dygraph(progTS())  %>%
            dyLegend(show = "always", width = 600, hideOnMouseOut = FALSE) %>%
            dyAxis("y", label = "Percentile" ) %>%
            dyAxis("x", label = "Date", valueRange = c(0.0,1.0)) %>%            
            dyEvent("2020-03-13", "Season Paused", labelLoc = "bottom") %>%
            dyEvent("2020-07-30", "Season Resumed", labelLoc = "bottom")%>% 
            dyRangeSelector(dateWindow = c("2019-11-08", "2020-03-13")) %>% 
            dyLimit(.25, color = "red") %>%
            dyLimit(.5, color = "red") %>%
            dyLimit(.75, color = "red") %>%
            dyLimit(.95, color = "red") %>%
            dyHighlight(highlightCircleSize = 5, 
                        highlightSeriesBackgroundAlpha = 0.2,
                        hideOnMouseOut = FALSE)    
            
            
    })
    
    ## Explore ----
    data <- reactive({
      d[1:input$scatterD3_nb,]
    })
    

    
    output$scatter1 <- renderScatterD3({
 
      scatterD3(x = data()[,input$scatterD3_x] , #mpg$year,# 
                y = data()[,input$scatterD3_y] ) #mpg$cyl)#
    })

    output$scatter2 <- DT::renderDataTable({
      datatable( data() )#(shared_iris)
    }) #, server = FALSE)
    
    
    # CrossTalk----
    jittered_iris <- reactive({
      cbind(pct30 %>% 
            select(col1 = input$x2,
                   col2 = input$y2
                   #,col3 = input$color2
            ),  
           pct30 %>% 
            select(everything())
      )
    })
    
    shared_iris <- SharedData$new(jittered_iris)
    
    
    
    output$scatterA <- renderD3scatter({
      d3scatter(shared_iris, 
                ~col1, 
                ~col2, 
                #~col3, 
                width = "100%",
                # x_lim = c(1:100), 
                # y_lim = c(1:100),
                x_label = input$x2,
                y_label = input$y2)
    })
    
    output$scatterB <- DT::renderDataTable({
      datatable(shared_iris,
                options=list(columnDefs = list(list(visible=FALSE, targets=c(1:2)))))
    } , 
      server = FALSE)
    
    #   Mock Draft  ----
    output$mockDraftTable <- DT::renderDataTable(DT::datatable({
      data <- f %>% 
              filter(name %in% c(input$pg1, 
                                 input$pg2,
                                 input$sg1,
                                 input$sg2,
                                 input$sf1,
                                 input$sf2,
                                 input$pf1,
                                 input$pf2,
                                 input$c1,
                                 input$c2,
                                 input$util,
                                 input$sub1,
                                 input$sub2,
                                 input$sub3,
                                 input$sub4
                                 )) %>%
        
        mutate_at(vars(-c("name","PLAYER","Team", "Pos")),list(~as.numeric(.))) %>% 
        select(name, Pos, SALARY, Team)
      
      data %>%  
        adorn_totals(where = "row")
      
    }, rownames = F, 
    
    filter = 'top', 
    extensions = c('Buttons',  'ColReorder', 'FixedColumns','Scroller' ), 
    options = list(
      dom = 'Bfrtip',
      buttons =
        list('copy', 'print', list(
          extend = 'collection',
          buttons = list(
            list(extend = 'csv', filename = "Mock Draft"),
            list(extend = 'excel', filename = "Mock Draft"),
            list(extend = 'pdf', filename = "Mock Draft")),
          text = 'Download'
        )),
      pageLength = 500,
      colReorder = TRUE ,
#       scrollX = TRUE,
#       deferRender = TRUE,
#       scrollY = 500,
#       scroller = TRUE,
      fixedColumns = list(leftColumns = 1),
      columnDefs = list(list(className = 'dt-left',targets = "_all"))
    )     )) 
    
    
    
    
        
    
} # closes server

