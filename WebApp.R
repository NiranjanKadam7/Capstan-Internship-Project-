library(shiny)
library(ggplot2)



ui <- fluidPage(
  titlePanel("Trend"),
  fluidRow(
    column(width = 3,
           wellPanel(
             h4("Brand"),
             selectInput(inputId = "group",label = "Choose type",choices = c("Sales"="Sales",
                                                                             "Volume"="Volume",
                                                                             "Price"="Price",
                                                                             "No of Dealers"="No of Dealers",
                                                                             "Weighted Value of dist."="Weighted Value of dist.",
                                                                             "TVGRPs"="TVGRPs",
                                                                             "Absolute Media Spends(non-TV)"="Absolute Media Spends(non-TV)"
                                                                              ),
                         selected = "Sales",multiple = F),
             selectInput(inputId = "brand",label = "Brand ",choices = c("B1"="B1",
                                                                        "C1"="C1",
                                                                        "C2"="C2",
                                                                        "C3"="C3",
                                                                        "C4"="C4",
                                                                        "C5"="C5",
                                                                        "C6"="C6",
                                                                        "C7"="C7"
             ),
             selected = "B1",multiple = F),selectInput(inputId = "Time",label = "Time period",choices = c("Oct.2001- Sept.2002"="Oct.2001- Sept.2002",
                                                                                                          "Oct.2002- Sept.2003"="Oct.2002- Sept.2003",
                                                                                                          "Oct.2003- Sept.2004"="Oct.2003- Sept.2004"
             ),
             selected = "Oct.2001- Sept.2001",multiple = F)
           ),
    
           wellPanel(
             h4("Compare Brands"),
             selectInput(inputId = "group2",label = "Choose type",choices = c("Sales"="Sales",
                                                                             "Volume"="Volume",
                                                                             "Price"="Price",
                                                                             "No of Dealers"="No of Dealers",
                                                                             "Weighted Value of dist."="Weighted Value of dist."
                                                                             ),
                         selected = "Sales",multiple = F),
             selectInput(inputId = "brand21",label = "Select Brand ",choices = c("B1"="B1"
                                                                                 
                                                                        ),
                         selected = "B1",multiple = F),
             
             selectInput(inputId = "brand22",label = "Select Brand ",choices = c("C1"="C1",
                                                                                 "C2"="C2",
                                                                                 "C3"="C3",
                                                                                 "C4"="C4",
                                                                                 "C5"="C5",
                                                                                 "C6"="C6",
                                                                                 "C7"="C7"
                                                                         ),
                         
                          selected = "C1",multiple = F)
           )
          ),
    column(8,offset = 1,
           wellPanel(
             plotOutput(outputId = "Sales1",height = 275)
           ),
           wellPanel(
             plotOutput(outputId = "compare1",height = 275)
           ))
     )
)
server<-function(input,output){
  output$compare1 <- renderPlot({
    
    if(input$group2=="Sales" & input$brand21 =="B1" & input$brand22=="C1"){
    ggplot(sales,aes(x= Date))+
      geom_line(aes(y=B1),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C1),size=1,group=1,color="orange")+
        labs(x="Time",y="B1(Blue) &  C1 (Orange)",title = "Brand Comparison Graph")
  }else if(input$group2=="Sales" & input$brand21 =="B1" & input$brand22=="C2"){
    ggplot(sales,aes(x= Date))+
    geom_line(aes(y=B1),size=1,group=1,color="skyblue")+
    geom_line(aes(y=C2),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C2 (Orange)",title = "Brand Comparison Graph")
  }else if(input$group2=="Sales" & input$brand21 =="B1" & input$brand22=="C3"){
    ggplot(sales,aes(x= Date))+
      geom_line(aes(y=B1),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C3),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C3 (Orange)",title = "Brand Comparison Graph")
  }else if(input$group2=="Sales" & input$brand21 =="B1" & input$brand22=="C4"){
    ggplot(sales,aes(x= Date))+
      geom_line(aes(y=B1),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C4),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C4 (Orange)",title = "Brand Comparison Graph")
  }else if(input$group2=="Sales" & input$brand21 =="B1" & input$brand22=="C5"){
    ggplot(sales,aes(x= Date))+
      geom_line(aes(y=B1),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C5),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C5 (Orange)",title = "Brand Comparison Graph")
  }else if(input$group2=="Sales" & input$brand21 =="B1" & input$brand22=="C6"){
    ggplot(sales,aes(x= Date))+
      geom_line(aes(y=B1),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C6),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C6 (Orange)",title = "Brand Comparison Graph")
  }else if(input$group2=="Sales" & input$brand21 =="B1" & input$brand22=="C7"){
    ggplot(sales,aes(x= Date))+
      geom_line(aes(y=B1),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C7),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C7 (Orange)",title = "Brand Comparison Graph")
    
    
    
    
    
  }else if(input$group2=="Volume" & input$brand21 =="B1" & input$brand22=="C1"){
    ggplot(volume,aes(x= Date))+
      geom_line(aes(y=`B1 Sales Volume (000 Kgs)`),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C1),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C1 (Orange)",title = "Brand Comparison Graph")
  }else if(input$group2=="Volume" & input$brand21 =="B1" & input$brand22=="C2"){
    ggplot(volume,aes(x= Date))+
      geom_line(aes(y=`B1 Sales Volume (000 Kgs)`) ,size=1,group=1,color="skyblue")+
      geom_line(aes(y=C2),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C2 (Orange)",title = "Brand Comparison Graph")
  }else if(input$group2=="Volume" & input$brand21 =="B1" & input$brand22=="C3"){
    ggplot(volume,aes(x= Date))+
      geom_line(aes(y=`B1 Sales Volume (000 Kgs)`),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C3),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C3 (Orange)",title = "Brand Comparison Graph")
  }else if(input$group2=="Volume" & input$brand21 =="B1" & input$brand22=="C4"){
    ggplot(volume,aes(x= Date))+
      geom_line(aes(y=`B1 Sales Volume (000 Kgs)`),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C4),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C4 (Orange)",title = "Brand Comparison Graph")
  }else if(input$group2=="Volume" & input$brand21 =="B1" & input$brand22=="C5"){
    ggplot(volume,aes(x= Date))+
      geom_line(aes(y=`B1 Sales Volume (000 Kgs)`),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C5),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C5 (Orange)",title = "Brand Comparison Graph")
  }else if(input$group2=="Volume" & input$brand21 =="B1" & input$brand22=="C6"){
    ggplot(volume,aes(x= Date))+
      geom_line(aes(y=`B1 Sales Volume (000 Kgs)`),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C6),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C6 (Orange)",title = "Brand Comparison Graph")
  }else if(input$group2=="Volume" & input$brand21 =="B1" & input$brand22=="C7"){
    ggplot(volume,aes(x= Date))+
      geom_line(aes(y=`B1 Sales Volume (000 Kgs)`),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C7),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C7 (Orange)",title = "Brand Comparison Graph")
  
    
    
    
    
    
  }else if(input$group2=="Price" & input$brand21 =="B1" & input$brand22=="C1"){
    ggplot(price,aes(x= Date))+
      geom_line(aes(y=`B1  \r\nAvg Price/Litre`),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C1),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C1 (Orange)",title = "Brand Comparison Graph")
  }else if(input$group2=="Price" & input$brand21 =="B1" & input$brand22=="C2"){
    ggplot(price,aes(x= Date))+
      geom_line(aes(y=`B1  \r\nAvg Price/Litre`),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C2),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C2 (Orange)",title = "Brand Comparison Graph")
  }else if(input$group2=="Price" & input$brand21 =="B1" & input$brand22=="C3"){
    ggplot(price,aes(x= Date))+
      geom_line(aes(y=`B1  \r\nAvg Price/Litre`),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C3),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C3 (Orange)",title = "Brand Comparison Graph")
  }else if(input$group2=="Price" & input$brand21 =="B1" & input$brand22=="C4"){
    ggplot(price,aes(x= Date))+
      geom_line(aes(y=`B1  \r\nAvg Price/Litre`),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C4),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C4 (Orange)",title = "Brand Comparison Graph")
  }else if(input$group2=="Price" & input$brand21 =="B1" & input$brand22=="C5"){
    ggplot(price,aes(x= Date))+
      geom_line(aes(y=`B1  \r\nAvg Price/Litre`),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C5),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C5 (Orange)",title = "Brand Comparison Graph")
  }else if(input$group2=="Price" & input$brand21 =="B1" & input$brand22=="C6"){
    ggplot(price,aes(x= Date))+
      geom_line(aes(y=`B1  \r\nAvg Price/Litre`),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C6),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C6 (Orange)",title = "Brand Comparison Graph")
  }else if(input$group2=="Price" & input$brand21 =="B1" & input$brand22=="C7"){
    ggplot(price,aes(x= Date))+
      geom_line(aes(y=`B1  \r\nAvg Price/Litre`),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C7),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C7 (Orange)",title = "Brand Comparison Graph")
    
    
    
    
  }else if(input$group2=="No of Dealers" & input$brand21 =="B1" & input$brand22=="C1"){
    ggplot(NumD,aes(x= Date))+
      geom_line(aes(y=`B1 Number of Dealers ('000)`),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C1),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C1 (Orange)",title = "Brand Comparison Graph")
  }else if(input$group2=="No of Dealers" & input$brand21 =="B1" & input$brand22=="C2"){
    ggplot(NumD,aes(x= Date))+
      geom_line(aes(y=`B1 Number of Dealers ('000)`),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C2),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C2 (Orange)",title = "Brand Comparison Graph")
  }else if(input$group2=="No of Dealers" & input$brand21 =="B1" & input$brand22=="C3"){
    ggplot(NumD,aes(x= Date))+
      geom_line(aes(y=`B1 Number of Dealers ('000)`),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C3),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C3 (Orange)",title = "Brand Comparison Graph")
  }else if(input$group2=="No of Dealers" & input$brand21 =="B1" & input$brand22=="C4"){
    ggplot(NumD,aes(x= Date))+
      geom_line(aes(y=`B1 Number of Dealers ('000)`),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C4),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C4 (Orange)",title = "Brand Comparison Graph")
  }else if(input$group2=="No of Dealers" & input$brand21 =="B1" & input$brand22=="C5"){
    ggplot(NumD,aes(x= Date))+
      geom_line(aes(y=`B1 Number of Dealers ('000)`),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C5),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C5 (Orange)",title = "Brand Comparison Graph")
  }else if(input$group2=="No of Dealers" & input$brand21 =="B1" & input$brand22=="C6"){
    ggplot(NumD,aes(x= Date))+
      geom_line(aes(y=`B1 Number of Dealers ('000)`),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C6),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C6 (Orange)",title = "Brand Comparison Graph")
  }else if(input$group2=="No of Dealers" & input$brand21 =="B1" & input$brand22=="C7"){
    ggplot(NumD,aes(x= Date))+
      geom_line(aes(y=`B1 Number of Dealers ('000)`),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C7),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C7 (Orange)",title = "Brand Comparison Graph")
    
    
    
    
  }else if(input$group2=="Weighted Value of dist." & input$brand21 =="B1" & input$brand22=="C1"){
    ggplot(WtdVal,aes(x= Date))+
      geom_line(aes(y=`B1  \r\nWtd Val Distn (%)`),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C1),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C1 (Orange)",title = "Brand Comparison Graph")
  }else if(input$group2=="Weighted Value of dist." & input$brand21 =="B1" & input$brand22=="C2"){
    ggplot(WtdVal,aes(x= Date))+
      geom_line(aes(y=`B1  \r\nWtd Val Distn (%)`),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C2),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C2 (Orange)",title = "Brand Comparison Graph")
  }else if(input$group2=="Weighted Value of dist." & input$brand21 =="B1" & input$brand22=="C3"){
    ggplot(WtdVal,aes(x= Date))+
      geom_line(aes(y=`B1  \r\nWtd Val Distn (%)`),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C3),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C3 (Orange)",title = "Brand Comparison Graph")
  }else if(input$group2=="Weighted Value of dist." & input$brand21 =="B1" & input$brand22=="C4"){
    ggplot(WtdVal,aes(x= Date))+
      geom_line(aes(y=`B1  \r\nWtd Val Distn (%)`),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C4),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C4 (Orange)",title = "Brand Comparison Graph")
  }else if(input$group2=="Weighted Value of dist." & input$brand21 =="B1" & input$brand22=="C5"){
    ggplot(WtdVal,aes(x= Date))+
      geom_line(aes(y=`B1  \r\nWtd Val Distn (%)`),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C5),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C5 (Orange)",title = "Brand Comparison Graph")
  }else if(input$group2=="Weighted Value of dist." & input$brand21 =="B1" & input$brand22=="C6"){
    ggplot(WtdVal,aes(x= Date))+
      geom_line(aes(y=`B1  \r\nWtd Val Distn (%)`),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C6),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C6 (Orange)",title = "Brand Comparison Graph")
  }else if(input$group2=="Weighted Value of dist." & input$brand21 =="B1" & input$brand22=="C7"){
    ggplot(WtdVal,aes(x= Date))+
      geom_line(aes(y=`B1  \r\nWtd Val Distn (%)`),size=1,group=1,color="skyblue")+
      geom_line(aes(y=C7),size=1,group=1,color="orange")+
      labs(x="Time",y="B1(Blue) &  C7 (Orange)",title = "Brand Comparison Graph")
    
    
    
    
    
    
  }
  })    
 
  
  
  
  
  
  
  
  output$Sales1 <- renderPlot({
    
    if((input$group=="Sales") & input$brand =="B1" & input$Time =="Oct.2001- Sept.2002" ){
      ggplot(y1,aes(x=Date,y=B1))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Sales -B1")
    }else if(input$group=="Sales" & input$brand =="C1" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(y1,aes(x=Date,y=C1))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Sales C1")
    }else if(input$group=="Sales" & input$brand =="C2" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(y1,aes(x=Date,y=C2))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Sales C2")
    }else if(input$group=="Sales" & input$brand =="C3" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(y1,aes(x=Date,y=C3))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Sales C3")
    }else if(input$group=="Sales" & input$brand =="C4" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(y1,aes(x=Date,y=C4))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Sales C4")
    }else if(input$group=="Sales" & input$brand =="C5" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(y1,aes(x=Date,y=C5))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Sales C5")
      
    }else if(input$group=="Sales" & input$brand =="C6" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(y1,aes(x=Date,y=C6))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Sales C6")
      
    }else if(input$group=="Sales" & input$brand =="C7" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(y1,aes(x=Date,y=C7))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Sales C7")
      
    }else if(input$group=="Sales" & input$brand =="B1" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(y2,aes(x=Date,y=B1))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Sales -B1")
    }else if(input$group=="Sales" & input$brand =="C1" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(y2,aes(x=Date,y=C1))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Sales C1")
    }else if(input$group=="Sales" & input$brand =="C2" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(y2,aes(x=Date,y=C2))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Sales C2")
    }else if(input$group=="Sales" & input$brand =="C3" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(y2,aes(x=Date,y=C3))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Sales C3")
    }else if(input$group=="Sales" & input$brand =="C4" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(y2,aes(x=Date,y=C4))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Sales C4")
    }else if(input$group=="Sales" & input$brand =="C5" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(y2,aes(x=Date,y=C5))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Sales C5")
    }else if(input$group=="Sales" & input$brand =="C6" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(y2,aes(x=Date,y=C6))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Sales C6")
    }else if(input$group=="Sales" & input$brand =="C7" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(y2,aes(x=Date,y=C7))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Sales C7")
    }else if(input$group=="Sales" & input$brand =="B1" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(y3,aes(x=Date,y=B1))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Sales B1")
    }else if(input$group=="Sales" & input$brand =="C1" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(y3,aes(x=Date,y=C1))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Sales C1")
    }else if(input$group=="Sales" & input$brand =="C2" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(y3,aes(x=Date,y=C2))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Sales C2")
    }else if(input$group=="Sales" & input$brand =="C3" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(y3,aes(x=Date,y=C3))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Sales C3")
    }else if(input$group=="Sales" & input$brand =="C4" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(y3,aes(x=Date,y=C4))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Sales C4")
    }else if(input$group=="Sales" & input$brand =="C5" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(y3,aes(x=Date,y=C5))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Sales C5")
    }else if(input$group=="Sales" & input$brand =="C6" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(y3,aes(x=Date,y=C6))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Sales C6")
    }else if(input$group=="Sales" & input$brand =="C7" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(y3,aes(x=Date,y=C7))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Sales C7")
      
      
      
    }else if(input$group=="Volume" & input$brand =="B1" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(VY1,aes(x=Date,y=`B1 Sales Volume (000 Kgs)`))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Volume B1")
    }else if(input$group=="Volume" & input$brand =="C1" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(VY1,aes(x=Date,y=C1))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Volume B1")
    }else if(input$group=="Volume" & input$brand =="C2" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(VY1,aes(x=Date,y=C2))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Volume C2")
    }else if(input$group=="Volume" & input$brand =="C3" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(VY1,aes(x=Date,y=C3))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Volume C3")
    }else if(input$group=="Volume" & input$brand =="C4" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(VY1,aes(x=Date,y=C4))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Volume C4")
    }else if(input$group=="Volume" & input$brand =="C5" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(VY1,aes(x=Date,y=C5))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Volume C5")
      
    }else if(input$group=="Volume" & input$brand =="C6" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(VY1,aes(x=Date,y=C6))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Volume C6")
      
    }else if(input$group=="Volume" & input$brand =="C7" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(VY1,aes(x=Date,y=C7))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Volume C7")
      
    }else if(input$group=="Volume" & input$brand =="B1" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(VY2,aes(x=Date,y=`B1 Sales Volume (000 Kgs)`))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Volume -B1")
    }else if(input$group=="Volume" & input$brand =="C1" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(VY2,aes(x=Date,y=C1))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Volume C1")
    }else if(input$group=="Volume" & input$brand =="C2" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(VY2,aes(x=Date,y=C2))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Volume C2")
    }else if(input$group=="Volume" & input$brand =="C3" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(VY2,aes(x=Date,y=C3))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Volume C3")
    }else if(input$group=="Volume" & input$brand =="C4" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(VY2,aes(x=Date,y=C4))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Volume C4")
    }else if(input$group=="Volume" & input$brand =="C5" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(VY2,aes(x=Date,y=C5))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Volume C5")
    }else if(input$group=="Volume" & input$brand =="C6" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(VY2,aes(x=Date,y=C6))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Volume C6")
    }else if(input$group=="Volume" & input$brand =="C7" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(VY2,aes(x=Date,y=C7))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Volume C7")
    }else if(input$group=="Volume" & input$brand =="B1" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(VY3,aes(x=Date,y=`B1 Sales Volume (000 Kgs)`))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Volume B1")
    }else if(input$group=="Volume" & input$brand =="C1" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(VY3,aes(x=Date,y=C1))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Volume C1")
    }else if(input$group=="Volume" & input$brand =="C2" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(VY3,aes(x=Date,y=C2))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Volume C2")
    }else if(input$group=="Volume" & input$brand =="C3" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(VY3,aes(x=Date,y=C3))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Volume C3")
    }else if(input$group=="Volume" & input$brand =="C4" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(VY3,aes(x=Date,y=C4))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Volume C4")
    }else if(input$group=="Volume" & input$brand =="C5" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(VY3,aes(x=Date,y=C5))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Volume C5")
    }else if(input$group=="Volume" & input$brand =="C6" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(VY3,aes(x=Date,y=C6))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Volume C6")
    }else if(input$group=="Volume" & input$brand =="C7" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(VY3,aes(x=Date,y=C7))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Volume C7")
      
      
      
    }else if(input$group=="Price" & input$brand =="B1" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(py1,aes(x=Date,y=`B1  \r\nAvg Price/Litre`))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Price B1")
    }else if(input$group=="Price" & input$brand =="C1" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(py1,aes(x=Date,y=C1))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Price B1")
    }else if(input$group=="Price" & input$brand =="C2" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(py1,aes(x=Date,y=C2))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Price C2")
    }else if(input$group=="Price" & input$brand =="C3" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(py1,aes(x=Date,y=C3))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Price C3")
    }else if(input$group=="Price" & input$brand =="C4" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(py1,aes(x=Date,y=C4))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Price C4")
    }else if(input$group=="Price" & input$brand =="C5" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(py1,aes(x=Date,y=C5))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Price C5")
      
    }else if(input$group=="Price" & input$brand =="C6" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(py1,aes(x=Date,y=C6))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Price C6")
      
    }else if(input$group=="Price" & input$brand =="C7" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(py1,aes(x=Date,y=C7))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Price C7")
      
    }else if(input$group=="Price" & input$brand =="B1" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(py2,aes(x=Date,y=`B1  \r\nAvg Price/Litre`))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Price B1")
    }else if(input$group=="Price" & input$brand =="C1" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(py2,aes(x=Date,y=C1))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Price C1")
    }else if(input$group=="Price" & input$brand =="C2" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(py2,aes(x=Date,y=C2))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Price C2")
    }else if(input$group=="Price" & input$brand =="C3" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(py2,aes(x=Date,y=C3))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Price C3")
    }else if(input$group=="Price" & input$brand =="C4" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(py2,aes(x=Date,y=C4))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Price C4")
    }else if(input$group=="Price" & input$brand =="C5" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(py2,aes(x=Date,y=C5))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Price C5")
    }else if(input$group=="Price" & input$brand =="C6" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(py2,aes(x=Date,y=C6))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Price C6")
    }else if(input$group=="Price" & input$brand =="C7" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(py2,aes(x=Date,y=C7))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Price C7")
    }else if(input$group=="Price" & input$brand =="B1" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(py3,aes(x=Date,y=
                       `B1  \r\nAvg Price/Litre`))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Price B1")
    }else if(input$group=="Price" & input$brand =="C1" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(py3,aes(x=Date,y= C1))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Price C1")
    }else if(input$group=="Price" & input$brand =="C2" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(py3,aes(x=Date,y=C2))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Price C2")
    }else if(input$group=="Price" & input$brand =="C3" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(py3,aes(x=Date,y=C3))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Price C3")
    }else if(input$group=="Price" & input$brand =="C4" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(py3,aes(x=Date,y=C4))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Price C4")
    }else if(input$group=="Price" & input$brand =="C5" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(py3,aes(x=Date,y=C5))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Price C5")
    }else if(input$group=="Price" & input$brand =="C6" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(py3,aes(x=Date,y=C6))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Price C6")
    }else if(input$group=="Price" & input$brand =="C7" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(py3,aes(x=Date,y=C7))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Price C7")
      
      
      
      
    }else if(input$group=="No of Dealers" & input$brand =="B1" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(NY1,aes(x=Date,y=`B1 Number of Dealers ('000)`))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="No of Dealers B1")
    }else if(input$group=="No of Dealers" & input$brand =="C1" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(NY1,aes(x=Date,y=C1))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="No of Dealers B1")
    }else if(input$group=="No of Dealers" & input$brand =="C2" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(NY1,aes(x=Date,y=C2))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="No of Dealers C2")
    }else if(input$group=="No of Dealers" & input$brand =="C3" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(NY1,aes(x=Date,y=C3))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="No of Dealers C3")
    }else if(input$group=="No of Dealers" & input$brand =="C4" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(NY1,aes(x=Date,y=C4))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="No of Dealers C4")
    }else if(input$group=="No of Dealers" & input$brand =="C5" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(NY1,aes(x=Date,y=C5))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="No of Dealers C5")
      
    }else if(input$group=="No of Dealers" & input$brand =="C6" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(NY1,aes(x=Date,y=C6))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="No of Dealers C6")
      
    }else if(input$group=="No of Dealers" & input$brand =="C7" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(NY1,aes(x=Date,y=C7))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="No of Dealers C7")
      
    }else if(input$group=="No of Dealers" & input$brand =="B1" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(NY2,aes(x=Date,y=`B1 Number of Dealers ('000)`))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="No of Dealers B1")
    }else if(input$group=="No of Dealers" & input$brand =="C1" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(NY2,aes(x=Date,y=C1))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="No of Dealers C1")
    }else if(input$group=="No of Dealers" & input$brand =="C2" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(NY2,aes(x=Date,y=C2))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="No of Dealers C2")
    }else if(input$group=="No of Dealers" & input$brand =="C3" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(NY2,aes(x=Date,y=C3))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="No of Dealers C3")
    }else if(input$group=="No of Dealers" & input$brand =="C4" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(NY2,aes(x=Date,y=C4))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="No of Dealers C4")
    }else if(input$group=="No of Dealers" & input$brand =="C5" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(NY2,aes(x=Date,y=C5))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="No of Dealers C5")
    }else if(input$group=="No of Dealers" & input$brand =="C6" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(NY2,aes(x=Date,y=C6))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="No of DealersC6")
    }else if(input$group=="No of Dealers" & input$brand =="C7" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(NY2,aes(x=Date,y=C7))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="No of Dealers C7")
    }else if(input$group=="No of Dealers" & input$brand =="B1" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(NY3,aes(x=Date,y=`B1 Number of Dealers ('000)`))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="No of Dealers B1")
    }else if(input$group=="No of Dealers" & input$brand =="C1" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(NY3,aes(x=Date,y=C1))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="No of Dealers C1")
    }else if(input$group=="No of Dealers" & input$brand =="C2" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(NY3,aes(x=Date,y=C2))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="No of Dealers C2")
    }else if(input$group=="No of Dealers" & input$brand =="C3" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(NY3,aes(x=Date,y=C3))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="No of Dealers C3")
    }else if(input$group=="No of Dealers" & input$brand =="C4" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(NY3,aes(x=Date,y=C4))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="No of Dealers C4")
    }else if(input$group=="No of Dealers" & input$brand =="C5" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(NY3,aes(x=Date,y=C5))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="No of Dealers C5")
    }else if(input$group=="No of Dealers" & input$brand =="C6" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(NY3,aes(x=Date,y=C6))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="No of DealersC6")
    }else if(input$group=="No of Dealers" & input$brand =="C7" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(NY3,aes(x=Date,y=C7))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="No of Dealers C7")
      
      
      
      
      
      
    }else if(input$group=="Weighted Value of dist." & input$brand =="B1" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(WY1,aes(x=Date,y=`B1  \r\nWtd Val Distn (%)`))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Weighted Value of dist. B1")
    }else if(input$group=="Weighted Value of dist." & input$brand =="C1" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(WY1,aes(x=Date,y=C1))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Weighted Value of dist. B1")
    }else if(input$group=="Weighted Value of dist." & input$brand =="C2" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(WY1,aes(x=Date,y=C2))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Weighted Value of dist. C2")
    }else if(input$group=="Weighted Value of dist." & input$brand =="C3" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(WY1,aes(x=Date,y=C3))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Weighted Value of dist. C3")
    }else if(input$group=="Weighted Value of dist." & input$brand =="C4" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(WY1,aes(x=Date,y=C4))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Weighted Value of dist. C4")
    }else if(input$group=="Weighted Value of dist." & input$brand =="C5" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(WY1,aes(x=Date,y=C5))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Weighted Value of dist. C5")
      
    }else if(input$group=="Weighted Value of dist." & input$brand =="C6" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(WY1,aes(x=Date,y=C6))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Weighted Value of dist. C6")
      
    }else if(input$group=="Weighted Value of dist." & input$brand =="C7" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(WY1,aes(x=Date,y=C7))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Weighted Value of dist. C7")
      
    }else if(input$group=="Weighted Value of dist." & input$brand =="B1" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(WY2,aes(x=Date,y=`B1  \r\nWtd Val Distn (%)`))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Weighted Value of dist. B1")
    }else if(input$group=="Weighted Value of dist." & input$brand =="C1" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(WY2,aes(x=Date,y=C1))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Weighted Value of dist. C1")
    }else if(input$group=="Weighted Value of dist." & input$brand =="C2" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(WY2,aes(x=Date,y=C2))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Weighted Value of dist. C2")
    }else if(input$group=="Weighted Value of dist." & input$brand =="C3" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(WY2,aes(x=Date,y=C3))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Weighted Value of dist. C3")
    }else if(input$group=="Weighted Value of dist." & input$brand =="C4" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(WY2,aes(x=Date,y=C4))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Weighted Value of dist. C4")
    }else if(input$group=="Weighted Value of dist." & input$brand =="C5" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(WY2,aes(x=Date,y=C5))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Weighted Value of dist. C5")
    }else if(input$group=="Weighted Value of dist." & input$brand =="C6" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(WY2,aes(x=Date,y=C6))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Weighted Value of dist. C6")
    }else if(input$group=="Weighted Value of dist." & input$brand =="C7" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(WY2,aes(x=Date,y=C7))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Weighted Value of dist. C7")
    }else if(input$group=="Weighted Value of dist." & input$brand =="B1" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(WY3,aes(x=Date,y=`B1  \r\nWtd Val Distn (%)`))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Weighted Value of dist. B1")
    }else if(input$group=="Weighted Value of dist." & input$brand =="C1" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(WY3,aes(x=Date,y= C1))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Weighted Value of dist. C1")
    }else if(input$group=="Weighted Value of dist." & input$brand =="C2" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(WY3,aes(x=Date,y=C2))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Weighted Value of dist. C2")
    }else if(input$group=="Weighted Value of dist." & input$brand =="C3" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(WY3,aes(x=Date,y=C3))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Weighted Value of dist. C3")
    }else if(input$group=="Weighted Value of dist." & input$brand =="C4" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(WY3,aes(x=Date,y=C4))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Weighted Value of dist. C4")
    }else if(input$group=="Weighted Value of dist." & input$brand =="C5" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(WY3,aes(x=Date,y=C5))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Weighted Value of dist. C5")
    }else if(input$group=="Weighted Value of dist." & input$brand =="C6" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(WY3,aes(x=Date,y=C6))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Weighted Value of dist. C6")
    }else if(input$group=="Weighted Value of dist." & input$brand =="C7" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(WY3,aes(x=Date,y=C7))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Weighted Value of dist. C7")
      
      
      
      
      
      
      
    }else if(input$group=="TVGRPs" & input$brand =="B1" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(TY1,aes(x=Date,y=`B1 Normalised GRP's (30 seconds)`))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="TVGRPs B1")
    }else if(input$group=="TVGRPs" & input$brand =="C1" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(TY1,aes(x=Date,y=C1))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="TVGRPs B1")
    }else if(input$group=="TVGRPs" & input$brand =="C2" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(TY1,aes(x=Date,y=C2))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="TVGRPsC2")
    }else if(input$group=="TVGRPs" & input$brand =="C3" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(TY1,aes(x=Date,y=C3))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="TVGRPs C3")
    }else if(input$group=="TVGRPs" & input$brand =="C4" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(TY1,aes(x=Date,y=C4))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="TVGRPs C4")
    }else if(input$group=="TVGRPs" & input$brand =="C5" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(TY1,aes(x=Date,y=C5))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="TVGRPs C5")
      
    }else if(input$group=="TVGRPs" & input$brand =="C6" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(TY1,aes(x=Date,y=C6))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="TVGRPs C6")
      
    }else if(input$group=="TVGRPs" & input$brand =="C7" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(TY1,aes(x=Date,y=C7))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="TVGRPs C7")
      
    }else if(input$group=="TVGRPs" & input$brand =="B1" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(TY2,aes(x=Date,y=`B1 Normalised GRP's (30 seconds)`))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="TVGRPs -B1")
    }else if(input$group=="TVGRPs" & input$brand =="C1" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(TY2,aes(x=Date,y=C1))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="TVGRPs C1")
    }else if(input$group=="TVGRPs" & input$brand =="C2" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(TY2,aes(x=Date,y=C2))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="TVGRPs C2")
    }else if(input$group=="TVGRPs" & input$brand =="C3" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(TY2,aes(x=Date,y=C3))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="TVGRPs C3")
    }else if(input$group=="TVGRPs" & input$brand =="C4" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(TY2,aes(x=Date,y=C4))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="TVGRPs C4")
    }else if(input$group=="TVGRPs" & input$brand =="C5" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(TY2,aes(x=Date,y=C5))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="TVGRPs C5")
    }else if(input$group=="TVGRPs" & input$brand =="C6" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(TY2,aes(x=Date,y=C6))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="TVGRPs C6")
    }else if(input$group=="TVGRPs" & input$brand =="C7" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(TY2,aes(x=Date,y=C7))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="TVGRPs C7")
    }else if(input$group=="TVGRPs" & input$brand =="B1" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(TY3,aes(x=Date,y=`B1 Normalised GRP's (30 seconds)`))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="TVGRPs B1")
    }else if(input$group=="TVGRPs" & input$brand =="C1" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(TY3,aes(x=Date,y=C1))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="TVGRPs C1")
    }else if(input$group=="TVGRPs" & input$brand =="C2" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(TY3,aes(x=Date,y=C2))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="TVGRPs C2")
    }else if(input$group=="TVGRPs" & input$brand =="C3" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(TY3,aes(x=Date,y=C3))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="TVGRPs C3")
    }else if(input$group=="TVGRPs" & input$brand =="C4" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(TY3,aes(x=Date,y=C4))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="TVGRPs C4")
    }else if(input$group=="TVGRPs" & input$brand =="C5" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(TY3,aes(x=Date,y=C5))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="TVGRPs C5")
    }else if(input$group=="TVGRPs" & input$brand =="C6" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(TY3,aes(x=Date,y=C6))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="TVGRPs C6")
    }else if(input$group=="TVGRPs" & input$brand =="C7" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(TY3,aes(x=Date,y=C7))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="TVGRPs C7")
      
      
      
    }else if(input$group=="Absolute Media Spends(non-TV)" & input$brand =="B1" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(AY1,aes(x=Date,y=`B1  \r\nAbsolute Media Spends Non TV (Print)`))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Absolute Media Spends(non-TV) B1")
    }else if(input$group=="Absolute Media Spends(non-TV)." & input$brand =="C1" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(AY1,aes(x=Date,y=C1))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Absolute Media Spends(non-TV) B1")
    }else if(input$group=="Absolute Media Spends(non-TV)" & input$brand =="C2" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(AY1,aes(x=Date,y=C2))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Absolute Media Spends(non-TV) C2")
    }else if(input$group=="Absolute Media Spends(non-TV)" & input$brand =="C3" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(AY1,aes(x=Date,y=C3))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Absolute Media Spends(non-TV) C3")
    }else if(input$group=="Absolute Media Spends(non-TV)" & input$brand =="C4" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(AY1,aes(x=Date,y=C4))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Absolute Media Spends(non-TV) C4")
    }else if(input$group=="Absolute Media Spends(non-TV)" & input$brand =="C5" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(AY1,aes(x=Date,y=C5))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Absolute Media Spends(non-TV) C5")
      
    }else if(input$group=="Absolute Media Spends(non-TV)" & input$brand =="C6" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(AY1,aes(x=Date,y=C6))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Absolute Media Spends(non-TV) C6")
      
    }else if(input$group=="Absolute Media Spends(non-TV)" & input$brand =="C7" & input$Time =="Oct.2001- Sept.2002"){
      ggplot(AY1,aes(x=Date,y=C7))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Absolute Media Spends(non-TV) C7")
      
    }else if(input$group=="Absolute Media Spends(non-TV)" & input$brand =="B1" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(AY2,aes(x=Date,y=`B1  \r\nAbsolute Media Spends Non TV (Print)`))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Absolute Media Spends(non-TV) B1")
    }else if(input$group=="Absolute Media Spends(non-TV)" & input$brand =="C1" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(AY2,aes(x=Date,y=C1))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Absolute Media Spends(non-TV) C1")
    }else if(input$group=="Absolute Media Spends(non-TV)" & input$brand =="C2" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(AY2,aes(x=Date,y=C2))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Absolute Media Spends(non-TV) C2")
    }else if(input$group=="Absolute Media Spends(non-TV)" & input$brand =="C3" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(AY2,aes(x=Date,y=C3))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Absolute Media Spends(non-TV) C3")
    }else if(input$group=="Absolute Media Spends(non-TV)" & input$brand =="C4" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(AY2,aes(x=Date,y=C4))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Absolute Media Spends(non-TV) C4")
    }else if(input$group=="Absolute Media Spends(non-TV)" & input$brand =="C5" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(AY2,aes(x=Date,y=C5))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Absolute Media Spends(non-TV) C5")
    }else if(input$group=="Absolute Media Spends(non-TV)" & input$brand =="C6" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(AY2,aes(x=Date,y=C6))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Absolute Media Spends(non-TV) C6")
    }else if(input$group=="Absolute Media Spends(non-TV)" & input$brand =="C7" & input$Time =="Oct.2002- Sept.2003"){
      ggplot(AY2,aes(x=Date,y=C7))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Absolute Media Spends(non-TV) C7")
    }else if(input$group=="Absolute Media Spends(non-TV)" & input$brand =="B1" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(AY3,aes(x=Date,y=`B1  \r\nAbsolute Media Spends Non TV (Print)`))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Absolute Media Spends(non-TV) B1")
    }else if(input$group=="Absolute Media Spends(non-TV)" & input$brand =="C1" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(AY3,aes(x=Date,y= C1))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Absolute Media Spends(non-TV) C1")
    }else if(input$group=="Absolute Media Spends(non-TV)" & input$brand =="C2" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(AY3,aes(x=Date,y=C2))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Absolute Media Spends(non-TV) C2")
    }else if(input$group=="Absolute Media Spends(non-TV)" & input$brand =="C3" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(AY3,aes(x=Date,y=C3))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Absolute Media Spends(non-TV) C3")
    }else if(input$group=="Absolute Media Spends(non-TV)" & input$brand =="C4" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(AY3,aes(x=Date,y=C4))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Absolute Media Spends(non-TV) C4")
    }else if(input$group=="Absolute Media Spends(non-TV)" & input$brand =="C5" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(AY3,aes(x=Date,y=C5))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Absolute Media Spends(non-TV) C5")
    }else if(input$group=="Absolute Media Spends(non-TV)" & input$brand =="C6" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(AY3,aes(x=Date,y=C6))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Absolute Media Spends(non-TV)")
    }else if(input$group=="Absolute Media Spends(non-TV)" & input$brand =="C7" & input$Time =="Oct.2003- Sept.2004"){
      ggplot(AY3,aes(x=Date,y=C7))+
        geom_point(size=2)+
        geom_line(size=1)+ 
        labs(title="Absolute Media Spends(non-TV)C7")
      
      
      
      
      
      
      
    }
  })

}


shinyApp(ui, server)
    
    
    