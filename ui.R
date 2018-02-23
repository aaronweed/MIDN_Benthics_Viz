
# The user-interface (ui) script controls the layout and appearance of your app. 

library(shiny)
library(leaflet)
library(shinyjs)
library(DT)

ParkList<-c("Appomattox Courthouse NHP","Booker T. Washington", "Eisenhower", "Fredericksburg Spotsylvania MP", "Gettsyburg","Hopewell Furnace" ,"Petersburg Battlefield MP","Richmond Battlefield MP")
#SiteList<-as.character(unique(df[,"Stream_Name"]))
VarList<-c("State MMI", "EPT Family Richness", "Family Richness","Genus Richness", "Shannon-Weaver Index", "Intolerant taxa (%)",
           "Mayflies (%)","Hilsenoff biotic Index (Family)"," Top 2 taxa (%)", "Chironomidae (%)", "Scrapers (%)",
           "Modified Becks Index", "Total animals")
HabList<-c("Epifaunal Substrate","Embeddedness","Pool Substrate","Velocity Depth Regime", "Pool Variability","Sediment Deposition","Channel Flow Status","Channel Alteration",
          "Riffle frequency","Channel Sinuosity","Bank Stability", 
          "Vegetation","Riparian Veg Zone")


shinyUI(navbarPage(title=HTML("<div> <a href='https://science.nature.nps.gov/im/units/midn/'> <img src='ah_small_black.gif',
          alt='Benthics Visualizer'> </a> Benthics Visualizer</div>"),position = "static-top", inverse=TRUE, collapsible = FALSE, fluid=TRUE, 
           windowTitle = "MIDN Benthics Visualizer", id="MainNavBar",
           
######################################### Benthics Panel ####################################################################
           
tabPanel(title="Benthics diversity data",
         #style="padding: 0",
                    useShinyjs(),
         div(class="outer",
             #tags$head(includeCSS("./www/mapstyles.css") ), # defines css file
             tags$head(HTML('<link rel="icon", href="AH_small_flat_4C_12x16.png", type="image/png" />'))
              #puts up icon on tab
            #, tags$head(includeScript("https://www.nps.gov/common/commonspot/templates/js/federated-analytics.js"))#,
         ),
         
fluidPage(
  sidebarPanel(
    h4("Plot summary stats from each sampled site within a park."),
    br(),
    tags$div(title="Choose the park you want to work with",selectInput(inputId='x', label='Select Park', choices= ParkList, selected = "Appomattox Courthouse NHP")),
    #selectInput(inputId='y',label= 'Select Stream', choices=as.character(unique(df[,"Stream_Name"]))),
    tags$div(title="Choose the metric you want to work with",selectInput(inputId='z', label='Select metric to plot', choices=VarList, selected = "State Multi-metric")),
    
    br(),
    p("Metrics are calculated from riffle samples in high gradient streams (APCO, BOWA, FRSP, GETT, and HOFU) and from multiple in-stream habitats in low gradient streams (EISE, RICH, PETE)"),
    br(),
    img(src = "BMI_sampling.jpg", height = 140, width = 180),
    br(),
    br(),
    img(src = "mayfly.jpeg", height = 140, width = 180),
    p("For further information about this sampling protocol, visit the ", 
    a("MIDN protocol page.", href= "http://science.nature.nps.gov/im/units/midn/monitor/benthic_macroinvertebrates.cfm")),
    br()
    ),

    mainPanel(plotOutput("plot"))
  )
),  ## end of benthjics page

######################################## Habitat Assessment Panel ##########################################################
 
 tabPanel(title="Graph Habitat Assessment Data", 
                    
                    useShinyjs(),

shinyUI(fluidPage(
  sidebarPanel(
    h4("Plot a summary of the habitat assessment within a park."),
    tags$div(title="Choose the park", selectInput(inputId='xx', label='Select Park', choices= ParkList, selected = "Appomattox Courthouse NHP")),
    tags$div(title="Choose the metric", radioButtons(inputId='h', label='Select metric to plot', choices=c("Total Habitat Score", "Individual metrics","Total Habitat Score by Stream Gradient Type"), selected = "Total Habitat Score")),
    conditionalPanel(
      condition="input.h=='Total Habitat Score by Stream Gradient Type'",selectInput(inputId='hh', label='Select stream type', choices= c("Low gradient",  "High gradient"), selected = "High gradient")),
    
    br(),
    p("For further information about this sampling protocol, visit the ", 
      a("MIDN protocol page.", href= "http://science.nature.nps.gov/im/units/midn/monitor/benthic_macroinvertebrates.cfm")),
    br()
  ),
  
  mainPanel(plotOutput("plot2"))
    )
   )
  )
)#end navbarPage()
)