#Scripts licensed under GPL v3 - check license for more info
####Pre-prep---#####
#9.052196 s
st <- Sys.time()
library(tidyverse)
library(lubridate)
library(igraph)
library(RColorBrewer)
library(readxl)

`%notin%` <- Negate(`%in%`)
every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})     #Just for ggplot
} 


integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}


#Downloading files
data_read <- #Your data here -> " "
temp = tempfile(fileext = ".xlsx")
download.file(data_read, destfile = temp, mode = "wb")

source("Day_making.R")

dcbc <- read_xlsx(temp, sheet = 1)
dcbc$Date <- ymd(dcbc$Date)
edges <- read_xlsx(temp, sheet = 3)
edges$Date <- ymd(edges$Date)
benzo <- read_xlsx(temp, sheet = 4)
node_col <- read_xlsx(temp, sheet =2)
end_read <- Sys.time()
dcbc$Week.val <- poss.w$Days2[match(dcbc$Date, poss.w$Days)]
edges$Week.val <- poss.w$Days2[match(edges$Date, poss.w$Days)]
benzo$Week.val <- poss.w$Days2[match(benzo$WeekID, poss.w$ID)]
#Creates All of BC 
end.time <- Sys.time()

benzo <- benzo[order(benzo$Week.val, benzo$Expected.Substance),] %>%
  select(-WeekID)

closure <-  c("Mar 16-\nMar 22\n2020", "Mar 23-\nMar 29\n2020", "Mar 30-\nApr 05\n2020")
benzo$Percent[which(benzo$Week.val %in% closure)] <- NA

op2 <- unique(dcbc$Expected.Substance[grepl("[Ff]ent", dcbc$Expected.Substance)])

d_all <- dcbc %>%
  mutate(City.Town = "All of BC",
         ID = ID*max(ID))
Edge_all <-edges %>%
  mutate(City.Town = "All of BC",
         ID = ID*max(ID))
dcbc <- rbind(dcbc, d_all)
edges <- rbind(edges, Edge_all)

all_opioids <- edges%>%
  dplyr::filter(Expected.Substance %in% op2) %>%
  mutate(Expected.Substance = "All Opioids (Grouped)",
         ID = ID+1000000)

opio2 <- dcbc %>%
  dplyr::filter(Expected.Substance %in% op2) %>%
  mutate(Expected.Substance = "All Opioids (Grouped)",ID = ID+1000000)

#Limits the number of in the dropdown


dcbc <- rbind(dcbc, opio2)
edges <- rbind(edges, all_opioids)



#Called here instead to not load into R until needed
#This file was written by hand, and could use some additions
#Categories were based off looking at fentanyl samples

coul  <- brewer.pal(length(unique(node_col$Classification)), "Set3")
my_colors <- coul[as.numeric(as.factor(unique(node_col$Classification)))]
names(my_colors) <- unique(node_col$Classification)


node <- dcbc %>%
  select(ID, value, Date, Week.val, City.Town, Expected.Substance)  %>%
  dplyr::rename("col_ID" = "ID", "Names" = "value") %>%
  left_join(node_col)


edge <- edges %>%
  select(ID, V1, V2, Expected.Substance, Week.val, City.Town)
colnames(edge) <- c("col_ID", "To", "From", "Expected.Substance","Week.val", "City.Town")

###---####


dc_totals <- dcbc %>%
  pivot_wider(names_from = name, values_from = value) %>%
  group_by(Week.val, Expected.Substance, City.Town) %>%
  count()


poss.w <- poss.w %>%
  dplyr::filter(Days <= max(dcbc$Date)) %>%
  select(ID, Days2) %>%
  distinct(.)
max_id <- c(max(poss.w$ID), max(poss.w$ID)-1)



benzo_tests <- c("Fentanyl/Down", "Alprazolam", "Methamphetamine", 
                 "Cocaine HCl", "MDMA", "Ketamine", "All Fentanyl & Fentanyl Analogues (Grouped)", "Crack Cocaine")

city_all <- unique(c(unlist(node$City.Town), "All of BC"))
get_id <-c(max(poss.w$ID), max(poss.w$ID)-1)



regrouped <- unique(node$Classification)
####----TO SHINY---####

library(ggraph)
library(tidygraph)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)

###UI-#####
ui <- navbarPage(title = "BC Drug Checking Visualizations",theme = shinytheme("flatly"),
                 ####----------INSTRUCTIONS-----#####
                 tabPanel("Instructions",
                          
                          mainPanel(
                            h1("Vancouver Drug Checking"),
                            p("It has been 5 years since BC first declared its overdose crisis. In 2017, after years of work by activists, small drug checking pilot projects began in Vancouver, where people who bought drugs from the illegal market could have them tested. They would then be relayed these results and told the approximate composition of the drug sample they submitted."),
                            p("In 2020 the team at the BCCSU made this data available here in a table:",
                              a("drugcheckingbc.ca", href = "https://drugcheckingbc.ca/")),
                            h4("Update Jan 2022: The BCCSU drug checking team has made their own app has made their own drug checking app:", a("DrugSense!", href = "https://drugcheckingbc.ca/dashboard/")),
                            h5("It's really cool and far more professional than my app. Go check out"),
                            p("I will keeping this app going mostly to try out some new things, but development on it will be slow. Things I'm thinking of for the future, include heatmaps, adjusting data/month, an area graph of the different benzo's found in the opioid supply, as well as dabbling with some stats... Maybe..."),
                            p("This work could not have been done without extensive feedback, as well as advice from Karen Ward, who initially asked me for a weekly readout of the drug checking data. She's provided feedback throughout the design of the initial graphs - as well as requested the graphs found on the 'Benzo's and Opioids' page, specifically looking at the effects on supply & cheque day. I've also received some technical (and emotional) support from Gjalt-Jorn Peters, and Adam Palayew."),
                            p("While I've had some help this is a completely independent project I've done in my free time during the pandemic. All mistakes are mine."),
                            h1("How to Use this Drug Checking Tool"),
                            h2("One Caution"),
                            p("Note that all visualizations in this project are descriptive, they should not be used for inferential analysis."),
                            h2("Drug Checking Data Page"),
                            p("This page includes the weekly drug checking results in BC by city. There are 2 different graphs that can be used to explore the data. The first is a network graph, the second is a bar graph. The 'network graph' shows how often 2 drugs were found together. Bar graphs demonstrate the frequency that individual substances are found, whereas the network graph shows the co-occurrence. The network graph attempts to visualize the relationship between all substances found by the FTIR."),
                            p("Like so:"),
                            img(src='explanation_image.png', align = "left", height = "50%", width = "50%"),
                            p("The size of the",strong("node"),"represents the number of times that substance was found. The width of the", strong("edges"),"demonstrates how often they occurred together."),
                            p(strong("This doesn't mean that every sample contained all of these drugs."), "It just means that all these substances have been found with the drug when someone has brought in a sample to be tested."),
                            h3("Currently you can explore the data by:"),
                            p("- Using the slider you can look at 1 week at a time, or multiple weeks. Note that the network graph is only interpretable at a lower number of tests. If a substance is found more than 400 times the node will disappear. You can slide the slider at 3 week intervals by putting your mouse on the blue part while dragging."),
                            p("- The Expected Substance, or what someone thought they were testing"),
                            p("- You can regroup drugs by pre-made classifications"),
                            p("- You can select different cities. Note, the data works best for Vancouver. If looking at other areas I suggest you pick 'Multiple' rather than looking 1 week at a time."),
                            p("- The FTIR has a detection limit of ~5%. When a result was positive on the fentanyl/Benzo test strip but not on the FTIR I have listed it as Benzodiazepine/Fent < 5%. Note 'Fent' here could be any of the analogues the strips can test for, and doesn't mean it is necessarily fentanyl."),
                            p("- Currently the network graph works best for Fentanyl - and worse for the others... Such is life."),
                            h4("Notes about the classifications"),
                            p("The classifications are written by hand by me.  -'Other or N/A' = Something irrelevant eg. 'water'. new_val = something I haven't coded. In making the choice whether something was 'buff','stimulant' or 'other', I've chosen whether they have a notable effect. Caffeine for eg. is included under stimulant because it has stimulant properties. In fentanyl it might be closer to a buff, but in MDMA or methamphetamine it has compounding effects. Buff includes any non-psychoactive substances that may work as fillers/excipients or inactive adulterants."),
                            br(""),
                            h1("Benzos and Opioids Page"),
                            p("Page 2 is more straightforward. It calculates the % or Count of fentanyl/benzo's by week. The percentages are only available if the median number of tests per week is greater than 30. Percentages at less than 30 samples/week are more likely to be skewed. For example, if 1/5 tests of MDMA in a week were positive for fentanyl the % would be 20% - likely not reflective of the actual rate."),
                            h3("Exploring the Data"),
                            p("You can also explore this data by city, and (by request) now look at cheque week. For cities with smaller average tests I've limited the graph to counts as %'s are unreliable at such small numbers."),
                            h1("Final Thoughts"),
                            p("Any visualization is a choice about how to cut, or a tool for thinking with data. Rather than just having these particular ways of visualizing the data, I'd like to create more options. If you are interested in learning more about R or about network visualization/analysis, I recommend Katherine Ognyanova's tutorial available",a("here", href = "https://kateto.net/netscix2016.html"), "or Jesse Sadler's tutorial", a("here", href = "https://www.jessesadler.com/post/network-analysis-with-r/")),
                            p("This is a first attempt at thinking about this question, but I'd love for others to join and help. I also have spent an incredible amount of time learning R to do this - If you're interested in helping develop this out, you can email me: alex.betsos@gmail.com"),
                            p("This code is made available under the GNU General Public License 3.0. The code is publicly available, but it comes with the stipulation that if you use it, changes you make must too be made available to the public. Drug data is part of our Common - it is made from the knowledge of people who use drugs, and as such, should be available to them. Where I have asked for help on stackoverflow, I have provided the link in the code comments. If you want to see/access the code, my github repo can be found here:", a("My Github Repo", href = "https://github.com/ADrugResearcher/DC_Process_Shiny"))
                          )),
                 tabPanel("Drug Checking Data",
                          
####----------------------------Page 1----------------------------------------------####
                          sidebarLayout(
                            sidebarPanel(width = 2,
                                         selectInput("Drug",
                                                     "Expected Substance",
                                                     choices = unique(node$Expected.Substance),
                                                     selected = "Fentanyl/Down"),
                                         selectInput("City",
                                                     "City",
                                                     choices = city_all,
                                                     selected = "Vancouver"),
                                         radioButtons("duration",
                                                      label = "1 Week or Multiple",
                                                      choices = c("1 Week", "Multiple"),
                                                      selected ="1 Week"),
                                         checkboxGroupInput("regroup",
                                                            label = "Regroup Variables",
                                                            choices = regrouped,
                                                            selected = NULL)
                            ),
                            mainPanel(width = 9,
                                      fluidRow(
                                        uiOutput("myList")),
                                      tabsetPanel(
                                        tabPanel("Network Graph",
                                                 fluidRow(tabstyle='padding:0px',
                                                          box(width = 12, 
                                                              offset = 0,
                                                              plotOutput("net", width = "100%",
                                                                         height = "750px",
                                                                         brush = "plot_brush")))
                                                 
                                 #Bar Chart
                                        ),
                                 tabPanel(
                                   "Bar Graph",
                                   fluidRow(
                                          plotOutput("single", width = "100%", height = "750px"))
                                 ))
                                        )
                            )
                          ),
                 tabPanel("Benzos + Opioids",
                          fixedRow(height = "200 px", column(width = 3,
                                          radioButtons("DC", "Select Expected Drug", 
                                                       choices = benzo_tests,
                                                       selected = "Fentanyl/Down")),
                                   column(width = 1, offset = 0,
                                          radioButtons("BF", "Percent/Count for\nBenzos/Fentanyl",
                                                       choices = c("% Fentanyl", "% Benzo",
                                                                   "Fentanyl Count", 
                                                                   "Benzo Count"),
                                                       selected = "Benzo Count")),
                                   column(width  = 2, selectInput("City2",
                                                                  "City",
                                                                  choices = unique(benzo$City.Town),
                                                                  selected = "Vancouver"),
                                          radioButtons("Cheque", "Highlight\nCheque Week? (in red)",
                                                       choices = c("Yes", "No"), selected = "No")),
                                   
                                   column(width = 6, offset = 0, verbatimTextOutput(outputId='ggplot_warnings'))),
                          fixedRow(plotOutput("Perc", width = "100%", height = 500))
                 )
)
####----####

#Server still needs to be fixed...
server <- function(input, output, session) {

## Steps
## 1. Build slider UI
##  
  # Need to exclude the buttons from themselves being bookmarked
####------------------------SETUP-----------------------####
 
  #Create reactive value to hold slider info
  slidertype <- reactiveValues()
  
  slidertype$type <- "default"
  observeEvent(input$duration, {
    #When person changes from 1 week to multiple it will change slider
    if(input$duration == "1 Week"){
      slidertype$type <- "1 Week"
    } else if(input$duration == "Multiple"){
      slidertype$type <- "Multiple"
    } else {
      slidertype$type <- "default"
    }
  })

  the_change <- reactive({
    if(is.null(input$Change)){
      as.character(poss.w$Days2[poss.w$ID == max(get_id)])}else{
        return(input$Change)
        }
  }
)
  output$myList <- renderUI({
    #Changes based on whether someone selects output
    if(slidertype$type == "1 Week"){
      sliderTextInput("Change",
                      label = NULL,
                      choices = as.character(poss.w$Days2),
                      selected = as.character(poss.w$Days2[poss.w$ID == max(get_id)]), force_edges = TRUE,
                      width = "1200px")
    } else if(slidertype$type == "Multiple") {
      sliderTextInput("Change",
                      label = NULL,
                      choices = as.character(poss.w$Days2),
                      selected = as.character(poss.w$Days2[poss.w$ID %in% get_id]),
                      force_edges = TRUE,
                      width = "1200px" )
    } else{
      sliderTextInput("Change",
                      label = NULL,
                      choices = as.character(poss.w$Days2),
                      selected = as.character(poss.w$Days2[poss.w$ID == max(get_id)]),
                      force_edges = TRUE,
                      width = "1200px" )
    }
  })
  
  poss_e <- reactive({poss.w[poss.w$Days2 <= max(dcbc$Week.val[dcbc$City.Town == input$City]) & poss.w$Days2 >= min(dcbc$Week.val[dcbc$City.Town == input$City]),]})
  
  observeEvent(input$City,{
    if(slidertype$type != "Multiple"){
      new_id <- max(poss_e()$ID)
      updateSliderTextInput(session,inputId = "Change", choices = as.character(poss_e()$Days2), selected = as.character(poss_e()$Days2[poss_e()$ID == max(new_id)]))
    } else if (slidertype$type == "Multiple") {
      new_id <- c(max(poss_e()$ID)-1, max(poss_e()$ID))
      updateSliderTextInput(session,inputId = "Change", choices = as.character(poss_e()$Days2), selected = as.character(poss_e()$Days2[poss_e()$ID %in% get_id]))
    } else {
      updateSliderTextInput(session,inputId = "Change", choices = as.character(poss_e()$Days2), selected = as.character(poss_e()$Days2[poss_e()$ID == max(get_id)]))
    }
  }, ignoreInit = TRUE)
  
  re_group <- function(.data, Classification, Names){
    .data %>%
      mutate(Names = ifelse(Classification %in% input$regroup,                            Classification, Names))
  }
  #Takes the ID's from slidertext and converts them
  #Back into Days for the purpose of selecting variables
  up_id <- reactive({
    if(!is.null(input$Change)){
    n_id <- poss.w$ID[poss.w$Days2 %in% input$Change]
    } else {
      n_id <- poss.w$ID[poss.w$ID %in% max(get_id)]
    } 
    if(length(n_id) > 1.5){
      n_id <- rev(seq(n_id[1], n_id[2]))
    }
    n_id <- poss.w$Days2[poss.w$ID %in% n_id]
    return(n_id)
    
  })
#Gets the IDS that are used based on user input
  combined <- reactive({
      nod = node %>%
       dplyr::filter(Week.val %in% up_id() &
                 Expected.Substance %in% input$Drug &
                   City.Town == input$City
                   ) %>%
        select(col_ID)
    nod = unique(nod$col_ID)
    return(nod)
  }) 
##JUST GOTTA FINSH ORGANIZING THIS

#Creates nodes & also provides a weight based on n of occurrence
  nodes2 <-reactive({ 
    combo <- combined()
    n2 <- node_col %>%
      re_group(Classification, Names) %>%
      group_by(Names, Classification) %>%
      summarise(ID =max(ID)) %>%
      ungroup() 
    n3 <- node[node$col_ID %in% combo,] %>% 
      re_group(Classification, Names) %>%
      group_by(Names, Classification) %>%
      summarise(Weight = n())%>%
      ungroup() 
    if(nrow(n3>1)){
      n3 <- n3 %>%
      left_join(n2) %>%
      select(ID, Names, Weight, Classification)
    n3$Weight[grepl("No Cuts", n3$Names)] <- n3$Weight[grepl("No Cuts", n3$Names)]/2}
    return(n3)
  })


  edges2 <- reactive({ 
    combo <- combined()
    e2 <- edge[edge$col_ID %in% combo,] %>%
      select(To, From, col_ID) %>%
      rowid_to_column("P_ID")  %>%
      pivot_longer(To:From, names_to = "col_n",
                   values_to = "Names") 

    e2$Classification <- node_col$Classification[match(e2$Names, node_col$Names)]
    
    e3 <- e2 %>%
      re_group(Classification, Names)
    if(nrow(e3>1)){
      e3 <- e3 %>%
      left_join(node_col[,-c(3)], by = "Names") %>%
      select(-Classification, -Names) %>%
      pivot_wider(id_cols = P_ID, names_from = col_n, values_from = ID) %>%
      group_by(To, From) %>%
      summarise(weight = n()) %>%
      ungroup()}
    return(e3)
  })

  
  the.graph <- reactive({
    edges1 <- edges2()
    node_n <- nodes2()
    #Learned about scoping! Solution found here: https://twitter.com/dr_lauren_a/status/1423566060050132994
    g <<- graph_from_data_frame(d = edges1, vertices = node_n, directed = FALSE) 
    g <<- simplify(g, remove.loops = TRUE)
    return(g)
  })
####----------------------------END SETUP BEGIN GRAPH------------#####

  
####-----------------------NETWORK GRAPH------------------------#####

  output$net <- renderPlot({
    start_time <- Sys.time()
    e1 <- edges2()
    validate(
      need(nrow(e1) >0.9, "Not tested During this Time")
    )
    the_city <- input$City
    t_change <- the_change()
    Exp.Drug <- input$Drug
    the_groups <- input$regroup

    if(is.null(the_groups)){
      is.op <- FALSE
    } else {
      is.op <- grepl("Opioid", the_groups)
    }
    if(length(t_change) == 1){
      d1 <- gsub("\\Q\n\\E", " ", t_change)
      the_n <- dc_totals$n[dc_totals$Week.val == t_change & 
                             dc_totals$City.Town == the_city &
                             dc_totals$Expected.Substance == Exp.Drug]
      the_title1 <-  paste("Network Graph of Expected", Exp.Drug, sep = " ") 
      the_title2 <- paste("Samples Submitted Between", d1, "N =", the_n, sep = " ")}
    else if(length(t_change) >1) {
      #First select max & min dates & merge them together to show the range, min & max date
      d1 <- t_change[1]
      d2 <- t_change[2]
      d3 <- paste("from", str_extract(d1, "[A-Za-z]+ [0-9]+"), "to", str_extract(d2, "[A-Za-z]+ [0-9]+\\Q\n\\E20[0-9]+"), sep = " ")
      #find the n of tests conducted for the Expected Substance
      d3 <- gsub("\\Q\n\\E", " ", d3)
      the_n <- dc_totals %>%
        filter(Expected.Substance == Exp.Drug & Week.val %in% up_id() & City.Town == the_city) %>%
        rename("new_n"= "n")
      the_n <- as.vector(sum(the_n$new_n))
      the_title1 <- paste("Expected", Exp.Drug, sep = " ")
      the_title2 <- paste("Samples Submitted Between",  d3, "N =", the_n, sep =" ") 
    } else {
      the_title1 <- paste("Expected", Exp.Drug, sep = " ")
      the_title2 <- paste("Samples Submitted Between", t_change, sep = " ")
    }
    the_title1 <- gsub("\\Q\n\\E", " ", the_title1)
    the_title2 <- gsub("\\Q\n\\E", " ", the_title2)
    
    node3 <- nodes2()
    g <- the.graph() 
    if(Exp.Drug %in% c(V(g)$Names, "Fentanyl/Down", "All Opioids (Grouped)") & 
       nrow(edges) >=6 & nrow(node3) >=10){
      #Checks if there is just one graph or several
      if(is.connected(g) == FALSE){
        #if FALSE then, it splits the main graph from the subgraphs
        c <- clusters(g); cn <- cbind(V(g), c$membership)
        lc <- which(which.max(c$csize)==c$membership);
        gs <- induced.subgraph(g, lc)
        if((Exp.Drug == "All Opioids (Grouped)"|Exp.Drug == "Fentanyl/Down") &
           (is.op == FALSE) ){
          st1 <- layout_as_star(gs, center = V(gs)$Names == "Fentanyl Or Analog")
        }else if((Exp.Drug == "All Opioids (Grouped)"|Exp.Drug == "Fentanyl/Down") &
                 is.op == TRUE){
          st1 <- layout_as_star(gs, center = V(gs)$Names == "Opioid")
        }else{
          st1 <- layout_as_star(gs, center = V(gs)$Names == Exp.Drug)
        }

        st1 <- norm_coords(st1, xmin = -0.6, xmax = 0.6, 
                           ymin = -0.6, ymax = +0.6,
                           zmin = -0.6, zmax = +0.6)
        #Normalize even and odd rows at different min & max to stagger nodes
        st1[seq(2, nrow(st1),2),] <- norm_coords(st1[seq(2, nrow(st1),2),],
                                                 xmin = -0.45, xmax = 0.45, 
                                                 ymin = -0.45, ymax = +0.45,
                                                 zmin = -0.45, zmax = +0.45)
        lc2 <- which(!which.max(c$csize)==c$membership)
        gs2 <- induced.subgraph(g, lc2)
        circ <- layout_in_circle(gs2)
      
        circ <- norm_coords(circ, xmin = -0.8, xmax = 0.8, 
                            ymin = -0.8, ymax = +0.8,
                            zmin = -0.8, zmax = +0.8)
        test2 <- rbind(st1,circ) 
        g <- gs %du% gs2
        t_lay <- create_layout(g, test2)
         
      }else{
        if( (Exp.Drug == "All Opioids (Grouped)"|Exp.Drug == "Fentanyl/Down") &
           (is.op == FALSE) ){
          st1 <- layout_as_star(g, center = V(g)$Names == "Fentanyl Or Analog")
          st1 <- norm_coords(st1, xmin = -0.8, xmax = 0.8, 
                             ymin = -0.8, ymax = +0.8,
                             zmin = -0.8, zmax = +0.8)
    
        }else if((Exp.Drug == "All Opioids (Grouped)"|Exp.Drug == "Fentanyl/Down") &
                 is.op == TRUE){
          st1 <- layout_as_star(gs, center = V(gs)$Names == "Opioid")
          st1 <- norm_coords(st1, xmin = -0.8, xmax = 0.8, 
                             ymin = -0.8, ymax = +0.8,
                             zmin = -0.8, zmax = +0.8)
        }else{
        st1 <- layout_as_star(g, center = V(g)$Names == Exp.Drug)

        st1 <- norm_coords(st1, xmin = -0.8, xmax = 0.8, 
                           ymin = -0.8, ymax = +0.8,
                           zmin = -0.8, zmax = +0.8)
        
         }
        t_lay <- create_layout(g, st1)
      }
      #For every other drug sample - still WIP
    } else {
      t_lay <- create_layout(g, layout = "nicely")
      
    }
    #Set graph space limits
    x_max <- max(t_lay$x)+0.1
    x_min <- min(t_lay$x)-0.1
    y_min <- min(t_lay$y)-0.1
    y_max <- max(t_lay$y)+0.1
    par(mar = c(0, 0, 0, 0))
    pre_graph <- Sys.time()
    time_takes <- pre_graph-start_time
    cat(file=stderr(), "Time to do pre-graph calculations", time_takes, "\n")
    ggraph(t_lay) +
      geom_edge_link0(aes(width = E(g)$weight), colour = "grey") +   # add edges to the plot
      scale_edge_width_continuous(breaks = c(1, 5, 10, 25, 50,100),
                                  label = c(1, 5, 10, 25, 50, 100),
                                  range = c(1,20), name = "Frequency Found Together",
                                  limits = c(0,400),
                                  guide = guide_legend(order = 2, 
                                                       nrow = 1,
                                                       ncol =7)) +
      geom_node_point(aes(size = V(g)$Weight, color = V(g)$Classification)) +
      scale_color_manual(values = my_colors, name = "Class of Drug",
                         guide = guide_legend(order = 3, 
                                              ncol = 2,
                                              nrow = 5)) +
      coord_cartesian(ylim = c(y_min, y_max), xlim = c(x_min, x_max)) +
      geom_node_text(aes(label = V(g)$Names), angle = 30, size = 5) +
      scale_size(breaks = c(1,10,20,40, 60,80, 100), label=scales::number,
                 range = c(1,60), limits = c(1,400), name = "# of Times Drug Found \n in Test Results",
                 guide = guide_legend(order = 1,
                                      nrow = 4,
                                      ncol = 2,
                                      label.hjust =0.5)) +
      labs(caption = "Fent/Benzodiazepine < 5% means substance tested positive on test strip", title = paste(the_title1, the_title2,sep = "\n")) +
      theme(legend.position= "right",
            legend.box.background = element_blank(),
            legend.direction = "vertical",
            legend.key = element_blank(),
            legend.background = element_blank(),
            legend.text = element_text(size=12, hjust  = 0.4, inherit.blank = TRUE),
            legend.box.just = "top",
            legend.box = "vertical",
            legend.justification = "right",
            legend.box.spacing = unit(0.5,"cm"),
            plot.caption = element_text(size = 14),
            legend.text.align = 0.4,
            plot.title = element_text(size = 15, hjust = 0.5),
            legend.title=element_text(size=14),
            legend.key.width = unit(0.5, "cm"),
            legend.key.height = unit(0.2, "cm"),
            legend.spacing = unit(0.5, "cm"),
            panel.background = element_blank(),
            legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
            legend.margin = margin(0,0, 0, 0, unit = "cm"))+
      guides(color = guide_legend(override.aes = list(size=10),
                                  nrow = 6,
                                  ncol = 3))
    
  })
  
  
  
####--------------BAR GRAPH-------------------------------####
  output$single <- renderPlot({
#Need to change "no cuts" - to just their regular name
    top_node2 <- nodes2()
    validate(
      need(nrow(top_node2) >1, "Not tested During this Time")
    )
    the_city <- input$City
    t_change <- the_change()
    Exp.Drug <- input$Drug
    the_groups <- input$regroup
    is.op <- grepl("Opioid", the_groups)
    if(length(t_change) == 1){
      d1 <- gsub("\\Q\n\\E", " ", t_change)
      the_n <- dc_totals$n[dc_totals$Week.val == t_change & 
                             dc_totals$City.Town == the_city &
                             dc_totals$Expected.Substance == Exp.Drug]
      the_title1 <-  paste("Network Graph of Expected", Exp.Drug, sep = " ") 
      the_title2 <- paste("Samples Submitted Between", d1, "N=", the_n, sep = " ")}
    else if(length(t_change) >1) {
      #First select max & min dates & merge them together to show the range, min & max date
      d1 <- t_change[1]
      d2 <- t_change[2]
      d3 <- paste("from", str_extract(d1, "[A-Za-z]+ [0-9]+"), "to", str_extract(d2, "[A-Za-z]+ [0-9]+\\Q\n\\E20[0-9]+"), sep = " ")
      #find the n of tests conducted for the Expected Substance
      d3 <- gsub("\\Q\n\\E", " ", d3)
      the_n <- dc_totals %>%
        filter(Expected.Substance == Exp.Drug & Week.val %in% up_id() & City.Town == the_city) %>%
        rename("new_n"= "n")
      the_n <- as.vector(sum(the_n$new_n))
      the_title1 <- paste("Expected", Exp.Drug, sep = " ")
      the_title2 <- paste("Samples Submitted Between",  d3, "N=", the_n, sep =" ") 
    } else {
      the_title1 <- paste("Expected", Exp.Drug, sep = " ")
      the_title2 <- paste("Samples Submitted Between",sep = " ")
    }
    
    date_change <- gsub("\\Q\n\\E", " ", input$Change)
    top_node2$Names <- gsub("\n", " ", top_node2$Names)
    top_node2%>%
      arrange(Classification, Weight) %>%
      mutate(Names = factor(Names, levels = Names)) %>%
      ggplot(., aes(x =Names, y=Weight, fill = Classification))+
      geom_bar(stat = "identity") +
      scale_fill_manual(values = my_colors, name = "Class of Drug",
                        guide = guide_legend(order = 2, 
                                             nrow = 13,
                                             aes.overide = list(hjust = 0.9))) +
      scale_x_discrete(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0), breaks = waiver(), n.breaks = 10,
                         limits = c(0, max(nodes2()$Weight)+5)) +
      theme_classic() +
      labs(title = paste(the_title1, the_title2, sep = "\n"), x = "Drugs",
           y = "Number of Occurrences") +
      coord_flip() +
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            axis.text = element_text(size = 15),
            axis.title = element_text(size = 15),
            legend.title=element_text(size=17),
            plot.caption = element_text(size = 12),
            legend.text = element_text(size=12, hjust  = 0.4, inherit.blank = TRUE))
  })
  
####---------------TABLE--------------------------------####
  #Second Page data
  shiny.benzo <-reactive({benzo %>%
      filter(Expected.Substance == input$DC & name == input$BF & City.Town == input$City2) %>%
      select(Week.val, name, Percent, Cheque, Size,  tot.not.test)
  })
  

  output$Perc <- renderPlot({
    #Creates conditional to not run % if n too small
    missing_n <- unique(poss.w$ID[poss.w$Days2 %notin% shiny.benzo()$Week.val])
    b.missing <- data.frame(Week.val = unique(poss.w$Days2[poss.w$ID %in% missing_n]), name = input$BF, 
                            Percent = -1,
                            Cheque = "Not Cheque",
                            Size = 4, 
                            tot.not.test = 0)
    new_benzo <- rbind(b.missing,shiny.benzo())
    a <- "a"
    perc_work <- c("Vancouver", "All of BC")
    count.perc <- input$BF
    the_mean <- mean(new_benzo$tot, na.rm = TRUE)
    if(str_detect(count.perc, "Count")){
      a <- "a"
    }else if(str_detect(count.perc, "%") & 
             the_mean > 40){
      a <- "a"
      }else if(str_detect("%", count.perc) &
               the_mean <= 40){
        a <- "b"
      }else{
        a<- "b"
        
      }
    validate(
      need(a == "a",
           "% too unreliable Please Use Count")
    )
     

    
    if(grepl("Count", input$BF) == FALSE){
      benzo.max <- shiny.benzo() %>%
        filter(name == input$BF)
      benzo.max <- max(benzo.max$Percent, na.rm = TRUE)
    } else{
      benzo.max <- max(new_benzo$Percent, na.rm = TRUE)
    }
    the_title1 <- paste(str_extract(count.perc, "%|Count"), "of Substances that Tested Positive for", sep = " ")
    the_title2 <- paste(ifelse(str_detect(count.perc, "Benzo") == TRUE, "Benzodiazepines", "Fentanyl"), "in", input$DC, "Samples", "From", input$City2, sep = " ")
    if(input$Cheque == "Yes"){
      graph <- ggplot(new_benzo, aes(x =Week.val, y = Percent)) +
        geom_line(group = 1, size = 1) + 
        geom_point(aes(color = Cheque, size = factor(Size))) +
        scale_size_manual(values = c(2,4), guide = "none") +
        scale_color_manual(values = c("red", "black"), guide = "none")+
        scale_x_discrete(breaks = every_nth(n = 3), drop = FALSE) +
        scale_y_continuous(breaks = integer_breaks(), limits = c(0, benzo.max+5)) +
        labs(title = paste(the_title1, the_title2, sep = "\n"), 
             x = "Weeks", y = count.perc, caption = "Note, 'Expected' refers to what people thought the drug they had was. Not all samples may have been sold or contained the drug in question")
    } else{
      graph <- ggplot(new_benzo, aes(x =Week.val, y = Percent)) +
        geom_line(group = 1, size = 1) + 
        geom_point(size = 2) +
        scale_x_discrete(breaks = every_nth(n = 3), drop = FALSE) +
        scale_y_continuous(breaks = integer_breaks(), limits = c(0, benzo.max+5)) +
        labs(title = paste(the_title1, the_title2, sep = "\n"), 
             x = "Weeks", y = count.perc, caption = "Note, 'Expected' refers to what people thought the drug they had was. Not all samples may have been sold or contained the drug in question\n")
    }
    graph +       
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 30),
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 20),
            plot.caption = element_text(size = 12)) +
      annotate(geom = "text", x = 13, 
               y = 12, label = "Services Closed")
  })
  
####----------------------FIN--------------------------####

}

shinyApp(ui = ui, server = server)



