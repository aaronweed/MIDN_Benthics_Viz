#The server.R script contains the instructions that your computer needs to build your app

library(shiny)
library(leaflet)
library(lattice)
library(rgdal)
library(shinyjs)
library(jsonlite,pos=100)
library(httr)
library(dplyr)
library(DT)
library(ggplot2)
library(RColorBrewer)

df<-read.csv("./Data/long_metrics.csv")

hab<-read.csv("./Data/rapid_bio.csv")
##### Begin Server Function ####

shinyServer(function(input,output){
  
  
  # data.r <- reactive(function(){
  #   
  #   a<- as.data.frame(subset(df, Stream_Name == input$y & Microhabitat =="RIF" & variable == input$z))
  #   
  #   #ifelse(input$Park_Code == "APCO"| input$Park_Code== "BOWA"| input$Park_Code == "FRSP"| input$Park_Code == "GETT"| input$Park_Code == "HOFU"| input$Park_Code =="VAFO", df[df$Park_Code == input$Park_Code & df$Stream_Name == input$Stream_Name & df$Microhabitat == "RIF" & df$variable == input$variable, ],
  #    #      df[df$Park_Code == x & df$Stream_Name == y & df$Microhabitat == "multi" & df$variable == z, ])  
  #     
  #   return(a)
  # 
  # })
 
  
   output$plot <- renderPlot({
    
    if(input$x == "Appomattox Courthouse NHP"| input$x== "Booker T. Washington"| input$x == "Fredericksburg Spotsylvania MP"| input$x == "Gettsyburg"| input$x == "Hopewell Furnace"){
    
       a<- subset(df, Park == input$x & Microhabitat =="RIF" & variable == input$z)
       
       p<-ggplot(a, aes(x=as.factor(year), y= value)) + labs(y = paste(input$z), x= "Year") +ylim(0, max(a$value + a$value*.1))+
         geom_bar(stat="identity", width=.9) +geom_text(aes(label=round(value,1)), vjust=-.5, colour= "red", fontface= "bold", size= 4) +
         geom_bar(stat="identity",show.legend=FALSE, fill= "cornflower blue", width=.9,position=position_dodge())
       
       p<-(p+facet_wrap(~Stream_Name, ncol=1) +
             theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
             theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 18 * 0.8, face="bold")) +
             theme(strip.text.x= element_text(size=16, face=c("bold.italic"))) +
             theme(axis.title.x =element_text(size = 14,  vjust= 1, debug=F))+
             theme(axis.title.y =element_text(size = 16,face="bold",  vjust= 1, debug=F))+
             theme(panel.background =  element_rect(fill="white", colour="black")) +
             theme(panel.grid.major = element_line(colour = "grey90"))+
             theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
             theme(strip.background= element_rect(size=10, color="gray" )))
       print(p) 
       
    } 
     
     if(input$x == "Petersburg Battlefield MP"|input$x == "Richmond Battlefield MP") {
     
       a<- subset(df, Park == input$x & Microhabitat =="multi" & variable == input$z)
       p<-ggplot(a, aes(x=as.factor(year), y= value)) + labs(y = paste(input$z), x= "Year") +ylim(0, max(a$value + a$value*.1))+
         geom_bar(stat="identity", width=.9) +geom_text(aes(label=round(value,1)), vjust=-.5, colour= "red", fontface= "bold", size= 4) +
         geom_bar(stat="identity",show.legend=FALSE, fill= "cornflower blue", width=.9,position=position_dodge())
       
       p<-(p+facet_wrap(~Stream_Name, ncol=1) +
             theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
             theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 18 * 0.8, face="bold")) +
             theme(strip.text.x= element_text(size=16, face=c("bold.italic"))) +
             theme(axis.title.x =element_text(size = 14,  vjust= 1, debug=F))+
             theme(axis.title.y =element_text(size = 16,face="bold",  vjust= 1, debug=F))+
             theme(panel.background =  element_rect(fill="white", colour="black")) +
             theme(panel.grid.major = element_line(colour = "grey90"))+
             theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
             theme(strip.background= element_rect(size=10, color="gray" )))
       print(p) 
    }
          
    
    if(input$x == "Eisenhower"){
          
          a<- subset(df, Park == input$x & Microhabitat =="multi" & variable == input$z)
          
          p<-ggplot(a, aes(x=as.factor(year), y= value)) + labs(y = paste(input$z), x= "Year") +ylim(0, max(a$value + a$value*.1))+
            geom_bar(stat="identity", width=.9) +geom_text(aes(label=round(value,1)), vjust=-.5, colour= "red", fontface= "bold", size= 4) +
            geom_bar(stat="identity",show.legend=FALSE, fill= "cornflower blue", width=.9,position=position_dodge())
          
          p<-(p+ facet_wrap(~Stream_Name)+ 
                theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
                 theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 18 * 0.8, face="bold")) +
                 theme(strip.text.x= element_text(size=16, face=c("bold.italic"))) +
                 theme(axis.title.x =element_text(size = 14,  vjust= 1, debug=F))+
                 theme(axis.title.y =element_text(size = 16,face="bold",  vjust= 1, debug=F))+
                 theme(panel.background =  element_rect(fill="white", colour="black")) +
                 theme(panel.grid.major = element_line(colour = "grey90"))+
                 theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
                 theme(strip.background= element_rect(size=10, color="gray" )))
          
          print(p) 
          } 
        
          }, height = 800, width = "auto")
 
  
   output$plot2 <- renderPlot({
        
    if(input$h == "Individual metrics"){
    
           b<- subset(hab, Park == input$xx)
           b$variable<-droplevels(b$variable)
           
      p<-ggplot(b[b$variable != "Total Score",], aes(x=variable, y= as.numeric(mean))) + labs(y = "Average score + SE (2009 - 2015)", x= "") +
      geom_bar(stat="identity", width=.9) +
      #geom_text(aes(label=round(mean,1)), hjust= 0,nudge_x = 1, colour= "red", fontface= "bold", size= 4) + 
      geom_bar(stat="identity",show.legend=FALSE, fill= "cornflowerblue", width=.9,position=position_dodge()) + # map again to add boundary to bar but not caption (http://wiki.stdout.org/rcookbook/Graphs/Legends%20%28ggplot2%29/)
        geom_errorbar(aes(ymin=mean, ymax=mean+se),width=.6,position=position_dodge(.9))
        
        
    p<-(p+facet_wrap(~Stream_Name, dir="v") +coord_flip()+
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text( size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text( size = 12, face ="bold", vjust= 1, debug=F))+
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90"))+
           #ggtitle(title) +
           theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
           theme(strip.background= element_rect(size=10, color="lightblue")))
    
    print(p)
    
    } 
     
     if(input$h == "Total Habitat Score"){
      
    b<- subset(hab, Park == input$xx & variable != "Total Score" )
    
    b$variable<-droplevels(b$variable)
    
    myColours <- brewer.pal(nrow(as.data.frame(levels(b$variable))),"Set3")
    
    b$site<- paste(b$Stream_Name, " (",b$Loc_Name,")",sep="") 
    
    p<-ggplot(b, aes(x= Stream_Name, y= mean,  fill= variable)) + labs(y = "Total score (2009 - 2015)", x= "") + 
      geom_bar(stat="identity", width= 0.9) + scale_fill_manual( values= myColours , name= "Individual Metrics")
    
    p<-(p+ theme(legend.key.size= unit(1.5, "cm")) + theme(legend.text=element_text(size = 12, colour = "#757070"))+ 
          theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
          theme(axis.text.x = element_text(angle = 90,  vjust=0.5,size = 16 * 0.8, face= "bold")) +
          theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
          theme(axis.title.x =element_text( size = 20, face ="bold", vjust= 1, debug=F))+
          theme(axis.title.y =element_text( size = 16, face ="bold", vjust= 1, debug=F))+
          theme(panel.background =  element_rect(fill="white", colour="black")) +
          theme(panel.grid.major = element_line(colour = "grey90"))+
          #ggtitle(title) +
          theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
          theme(strip.background= element_rect(size=10, color="lightblue")))
    
    
    print(p)
    
     }
     if(input$hh == "Total Habitat Score by Stream Gradient Type"){
       
       b<- subset(hab, Gradient == input$hh & variable != "Total Score" )
       
       b$variable<-droplevels(b$variable)
       
       myColours <- brewer.pal(nrow(as.data.frame(levels(b$variable))),"Set3")
       
       b$site<- paste(b$Stream_Name, " (",b$Loc_Name,")",sep="") 
       
       p<-ggplot(b, aes(x=Stream_Name, y= mean,  fill= variable)) + labs(y = "Total score (2009 - 2015)", x= "") + 
         geom_bar(stat="identity", width=.9) + scale_fill_manual( values= myColours , name= "Individual Metrics")
       
       p<-(p+ facet_wrap(~Park_Code) +
             theme(legend.key.size= unit(1.5, "cm")) + theme(legend.text=element_text(size = 12, colour = "#757070"))+
             theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
             theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8, face= "bold")) +
             theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
             theme(axis.title.x =element_text( size = 20, face ="bold", vjust= 1, debug=F))+
             theme(axis.title.y =element_text( size = 16, face ="bold", vjust= 1, debug=F))+
             theme(panel.background =  element_rect(fill="white", colour="black")) +
             theme(panel.grid.major = element_line(colour = "grey90"))+
             #ggtitle(title) +
             theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
             theme(strip.background= element_rect(size=10, color="lightblue")))
       print(p)
       
     }
     
  } , height = 800, width = "auto")

}) ## end shiny serverfunc    



