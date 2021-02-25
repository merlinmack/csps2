#Shiny App test
library(shiny)
library(dplyr)
library(MASS)
library(plotrix)
ui <- fluidPage(titlePanel("Supply and Demand"),
                sidebarLayout(
                  sidebarPanel(
                    #code to calculate window size (this is independent of output display)
                    tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')),
                    
                    h4("Click the button to start or get new equations"),
                    actionButton("run", "Get Equations"),
                    checkboxInput("inverse", label="See Inverse Equations", value=F),
                    checkboxInput("equil", label="See Equilibrium", value=F),
                    checkboxInput("csps", label="See CS and PS", value=F),
                    checkboxInput("graph", label="See Graph", value=F),
                    checkboxInput("tax", label="See Tax", value=F),
                    radioButtons("curve", "Tax on Suppliers or Consumers", c("Suppliers"="sup", "Consumers"="con")),
                    checkboxInput("incentive", label="See Incentives with Tax", value=F),
                    checkboxInput("equil.t", label="See Equilibrium with Tax", value=F),
                    checkboxInput("csps.t", label="See Surplus with Tax", value=F),
                    checkboxInput("graph.t", label="See Graph with Tax", value=F),
                    h4("Choose the Limit"),
                    radioButtons("limit", "Limit", c("10"="ten", "100"="hundred", "1000"="thousand"))

                  ),#end sidebarpanel
                  
                  mainPanel(
                    #verbatimTextOutput("dimension_display"),
                    h4("Normal Demand and Supply Equations"),
                    verbatimTextOutput("n.eq"),
                    h4("Inverse Demand and Supply Equations"),
                    verbatimTextOutput("i.eq"),
                    h4("Equilibrium"),
                    verbatimTextOutput("equil"),
                    h4("CS and PS"),
                    verbatimTextOutput("csps"),
                    h4("Tax"),
                    verbatimTextOutput("tax"),
                    h4("Incentive with Tax"),
                    verbatimTextOutput("inc"),
                    h4("Equilibrium with Tax"),
                    verbatimTextOutput("equil.t"),
                    h4("CS and PS with Tax"),
                    verbatimTextOutput("csps.t"),
                    h4("Graph"),
                    plotOutput("sd.graph"),
                    
                    
                  )#end mainpanel
                )#end sidebarlayout
)#end fluidPage 

server <- function(input, output, session){
  #this will display the window dimensions...I have this turned off in the display for now
  output$dimension_display <- renderText({
    paste(input$dimension[1], input$dimension[2], input$dimension[2]/input$dimension[1])
  })
  
  #set graph size
  g.s <- reactive(
    ifelse(input$dimension[1] < 500, "auto", 500)
  )
  
  #code that generates reactive (when the button is clicked) equation values
  vals <- reactiveValues()
  vals$g.n <- 0

    observeEvent(input$run,{
      #reset the checkboxes
      updatedValue = FALSE
      updateCheckboxInput(session =  session, inputId = "inverse", value = updatedValue)    
      updateCheckboxInput(session =  session, inputId = "equil", value = updatedValue)    
      updateCheckboxInput(session =  session, inputId = "csps", value = updatedValue)    
      updateCheckboxInput(session =  session, inputId = "graph", value = updatedValue)    
      updateCheckboxInput(session =  session, inputId = "tax", value = updatedValue)    
      updateCheckboxInput(session =  session, inputId = "incentive", value = updatedValue)    
      updateCheckboxInput(session =  session, inputId = "equil.t", value = updatedValue)    
      updateCheckboxInput(session =  session, inputId = "csps.t", value = updatedValue)    
      updateCheckboxInput(session =  session, inputId = "graph.t", value = updatedValue)    
      

      #get limit value from radio button
      if(input$limit == "ten"){
          vals$limit <- 10
      }else if(input$limit == "hundred"){
          vals$limit <- 100
      }else if (input$limit == "thousand"){
          vals$limit <- 1000
      }
      #build slope matrix
      q <- matrix(0,10,10)
      i <- seq(1,1000,1)
      #turn on the normal equations
      vals$g.n <- 1
      for(a in 1:10){
        #run across columns
        for(b in 1:10){
          q[a,b] <- i[a]/i[b]
        }
      }
      #loop to find whole-number equilibrium
      test=1
      count=1
      while(test==1){
        #set range of graph
        vals$range.1 <- seq(vals$limit/2, vals$limit, 1)
        #get parameter values for inverse demand and supply
        vals$a <- sample(vals$range.1,1)
        vals$b <- sample(q,1)
        vals$c <- sample(0:(vals$limit/2 - 1),1)
        vals$d <- sample(q,1)
        #choos tax
        vals$t <- sample(vals$c:vals$a, 1)
        #tax price and quantity
        vals$qt <- (vals$a-(vals$c + vals$t))/(vals$d+vals$b)
        vals$pc <- round((vals$a-vals$b*vals$qt), digits=2)
        vals$pp <- round((vals$pc - vals$t), digits=2)
        #calculate equilibrium
        vals$q1 <- (vals$a-vals$c)/(vals$d+vals$b)
        vals$p1 <- vals$a-vals$b*vals$q1
        #create rounded equilibrium values to test actual values to
        q.r <- round(vals$q1)
        p.r <- round(vals$p1)
        q.t <- round(vals$qt)
        #calculate cs and ps
        vals$cs <- round((.5 *  ((vals$q1) * (vals$a - vals$p1))), digits=2)
        vals$ps <- round((.5 * (vals$q1 * (vals$p1 - vals$c))), digits=2)
        #calculate cs and ps with tax
        vals$cs.t <- round((.5 *  ((vals$qt) * (vals$a - vals$pc))), digits=2)
        vals$ps.t <- round((.5 * (vals$qt * (vals$pp - vals$c))), digits=2)
        #DWL with tax
        vals$dwl <- round((.5 * vals$t * (vals$q1 - vals$qt)), digits=2)
        #test to see if rounded values equal actual values
        test <- ifelse(vals$q1 == q.r & vals$p1 == p.r & vals$qt == q.t & vals$qt > 0 & vals$t > 0, 2, 1)
        count=count+1
    }#end loop
  })#end observeevent
  
  #turn off the complementary graphing checkbox when one is is checked
  observe({
    if(input$graph == T){
      updateCheckboxInput(session =  session, inputId = "graph.t", value = FALSE)
    }
  })
  observe({
    if(input$graph.t == T){
      updateCheckboxInput(session =  session, inputId = "graph", value = FALSE)
    }
  })
  
  #send output back to UI
  #normal demand and supply
  output$n.eq <- renderText(if(vals$g.n == 1){paste(" Demand: Q = ", fractions(vals$a/vals$b), " - ", fractions(1/vals$b), "P \n",  
                                  "Supply: Q = ", fractions(1/vals$d), "P", " - ", fractions(vals$c/vals$d))})
  #inverse demand and supply
  output$i.eq <- renderText(if(input$inverse == T){paste(" Demand: P = ", fractions(vals$a), " - ", fractions(vals$b), "Q \n",
                                  "Supply: P = ", fractions(vals$c), " + ", fractions(vals$d), "Q")})
  #equilibrium values
  output$equil <- renderText(if(input$equil == T){paste("Q* = ", vals$q1, ", P* = $", vals$p1)})
  
  #CS and PS output
  output$csps <- renderText(if(input$csps == T){paste("CS = $", vals$cs, ", PS = $", vals$ps)})
  
  #Tax output
  output$tax <- renderText(if(input$tax == T){paste("Tax = $", vals$t)})
  
  #Inventives output
  output$inc <- renderText(if(input$incentive == T & input$curve == "sup"){
    paste("New Inverse Supply: P = ", fractions(vals$c+vals$t), " + ", fractions(vals$d), "Q")
  }else if(input$incentive == T & input$curve == "con"){
    paste("New Inverse Demand: P = ", fractions(vals$a-vals$t), " - ", fractions(vals$b), "Q")
  }
)
  #equilibrium values with tax
  output$equil.t <- renderText(if(input$equil.t == T){paste("Q*t = ", vals$qt, ", P*t = $", vals$pc)})
  
  #CS and PS output
  output$csps.t <- renderText(if(input$csps.t == T){paste("CS = $", vals$cs.t, ", PS = $", vals$ps.t, ", Tax Revenue = $", vals$t * vals$qt, ", DWL = $", vals$dwl)})
  
  #create graph figures without tax
  output$sd.graph <- renderPlot(expr={
    if(input$graph == T){
      #set range of x
      range.x <- ifelse(vals$limit > vals$q1, vals$limit,  round((vals$q1 + .25 * vals$q1)/10)*10)
      #set range of y
      range.y <- ifelse(vals$limit > vals$p1, vals$limit,  round((vals$p1 + .25 * vals$p1)/10)*10)
      #start plot
      plot(0, xlim =c(0,range.x), ylim = c(0,range.y), type="l", ylab="", xlab="", axes=F, las=1)#, asp=.5) 
      #cs
      polygon(c(0,0:vals$q1,vals$q1),c(vals$p1,vals$a-vals$b*(0:vals$q1),vals$p1), col="blue")
      #ps
      polygon(c(0,0:vals$q1,vals$q1),c(vals$p1,vals$c+vals$d*(0:vals$q1),vals$p1), col="purple")
      #add demand
      ablineclip(vals$a, -vals$b, x1=0, y1=0)
      #add supply
      ablineclip(vals$c, vals$d, x1=0)
      #add axes
      axis(1, pos=0, at=seq(0,range.x,(range.x/10)), cex.axis=.9)
      axis(2, pos=0, at=seq(0,range.y,(range.y/10)), cex.axis=.9)
      #add equilibrium lines
      ablineclip(h=vals$p1, x1=0, x2=vals$q1, lty="dashed", col="grey")
      ablineclip(v=vals$q1, y1=0, y2=vals$p1, lty="dashed", col="grey")
      #set positioning for S and D labels in graph
      x.end <- min(range.x-.1, (range.x-vals$c)/vals$d-.11)
      text(x=x.end, y=(vals$c + vals$d * x.end), labels="S", cex=1, pos=3)
      x.end <- min(range.x-.1, vals$a/vals$b-.1)
      text(x=x.end, y=(vals$a - vals$b * x.end), labels="D", cex=1, pos=3)
      #set axis labels and equilibrium labels on axes
      mtext(text=paste("Q*=", vals$q1), side=1, at=vals$q1, line=1.5)
      mtext(text=paste("P*=", vals$p1), side=2, at=vals$p1, line=1, las=1)
      mtext(text="P", side=2, at=range.y, line=3, las=1, cex=1.4)
      mtext(text="Q", side=1, at=range.x, line=3, las=1, cex=1.4)
      legend("top", inset=.05, c("CS", "PS"), fill=c("blue", "purple"))# cex=2)
      }else if(input$graph.t == T){#end of code that builds the plot
        #set range of x
        range.x <- ifelse(vals$limit > vals$q1, vals$limit,  round((vals$q1 + .25 * vals$q1)/10)*10)
        #set range of y
        range.y <- ifelse(vals$limit > vals$p1, vals$limit,  round((vals$p1 + .25 * vals$p1)/10)*10)
        #start plot
        plot(0, xlim =c(0,range.x), ylim = c(0,range.y), type="l", ylab="", xlab="", axes=F, las=1)#, asp=.5) 
        #tax revenue
        rect(xleft=0, ybottom=vals$pp, xright=vals$qt,ytop=vals$pc,col="green")
        #cs
        polygon(c(0,0:vals$qt,vals$qt),c(vals$pc,vals$a-vals$b*(0:vals$qt),vals$pc), col="blue")
        #ps
        polygon(c(0,0:vals$qt,vals$qt),c(vals$pp,vals$c+vals$d*(0:vals$qt),vals$pp), col="purple")
        #dwl
        polygon(c(vals$qt,vals$qt:vals$q1,vals$qt),c(vals$pp,vals$c+vals$d*(vals$qt:vals$q1),vals$pc), col="red")
        #add demand
        ablineclip(vals$a, -vals$b, x1=0, y1=0)
        #add supply
        ablineclip(vals$c, vals$d, x1=0)
        #add new incentive line
        if(input$curve == "sup"){
          ablineclip((vals$c + vals$t), vals$d, x1=0, col="red")
        }else if(input$curve == "con"){
          ablineclip((vals$a - vals$t), -vals$b, x1=0, y1=0, col="red")
        }
        #add axes
        axis(1, pos=0, at=seq(0,range.x,(range.x/10)), cex.axis=.9)
        axis(2, pos=0, at=seq(0,range.y,(range.y/10)), cex.axis=.9)
        #add equilibrium lines
        ablineclip(h=vals$p1, x1=0, x2=vals$q1, lty="dashed", col="grey")
        ablineclip(v=vals$q1, y1=0, y2=vals$p1, lty="dashed", col="grey")
        #tax line
        ablineclip(v=vals$qt, y1=vals$pp, y2=vals$pc, lty="dotted")
        #tax equilibrium
        ablineclip(h=vals$pc, x1=0, x2=vals$qt, lty="dashed", col="grey")
        ablineclip(h=vals$pp, x1=0, x2=vals$qt, lty="dashed", col="grey")
        ablineclip(v=vals$qt, y1=0, y2=vals$pp, lty="dashed", col="grey")
        #set positioning for S and D labels in graph
        x.end <- min(range.x-.1, (range.x-vals$c)/vals$d-.11)
        text(x=x.end, y=(vals$c + vals$d * x.end), labels="S", cex=1, pos=3)
        x.end <- min(range.x-.1, vals$a/vals$b-.1)
        text(x=x.end, y=(vals$a - vals$b * x.end), labels="D", cex=1, pos=3)
        #set axis labels and equilibrium labels on axes
        mtext(text=paste("Q*=", vals$q1), side=1, at=vals$q1, line=1.5)
        mtext(text=paste("P*=", vals$p1), side=2, at=vals$p1, line=1, las=1)
        mtext(text=paste("Q*t=", vals$qt), side=1, at=vals$qt, line=1.5)
        mtext(text=paste("Pc=", vals$pc), side=2, at=vals$pc, line=.75, las=1, cex=.8)
        mtext(text=paste("Pp=", vals$pp), side=2, at=vals$pp, line=.75, las=1, cex=.8)
        mtext(text="P", side=2, at=range.y, line=3, las=1, cex=1.4)
        mtext(text="Q", side=1, at=range.x, line=3, las=1, cex=1.4)
        legend("top", inset=.05, c("CS", "PS", "Tax Rev.", "DWL"), fill=c("blue", "purple", "green","red"))# cex=2)
        
      }#end of code that builds the plot
    
      
    }, width = g.s, height = g.s
    )#end renderPlot
  


    
}#end server function
shinyApp(ui = ui, server = server)



