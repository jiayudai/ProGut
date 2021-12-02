# https://rdrr.io/cran/shinyWidgets/man/actionBttn.html
# Layout credit for homepage: https://davidruvolo51.github.io/shinytutorials/tutorials/landing-page/

library(shiny)
library(shinydashboard)
library(DT)


library(tidyverse) 
library(spData) # For getting spatial data
library(sf) # For preserving spatial data
library(leaflet) # For making maps
library(dplyr)
library(datasets)
library(reshape2)

library(shinyWidgets)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
Shiny_big_table <- read.csv("CleanShiny_Original_New_completed_JR_correct.csv")
Shiny_big_table <- Shiny_big_table[,c(4,6,5,12,16,17,18,20,22,24,25,26)]

for(i in 1:nrow(Shiny_big_table)){
  Shiny_big_table$rownames[i] <- paste0("Bacteria",i)
}
rownames(Shiny_big_table) <- Shiny_big_table$rownames
Shiny_big_table <- Shiny_big_table[,-13]
colnames(Shiny_big_table) <- c("Name","Strain","Brand","Used_Bacteria","Species.Category","Status","Effect","Class","Reference","Detailed.Effect","NCBI.Genome","mbgd.Genome")

Shiny_big_table$NCBI.Genome <- paste0("<a href='",Shiny_big_table$NCBI.Genome,"'>",Shiny_big_table$NCBI.Genome,"</a>")
Shiny_big_table$mbgd.Genome <- paste0("<a href='",Shiny_big_table$mbgd.Genome,"'>",Shiny_big_table$mbgd.Genome,"</a>")

Group_result_l3 <- read.csv("groupID_bacteria_genus_pathway.csv")
Group_result_l3_reshape <- dcast(Group_result_l3, pathway~Group)
Group_result_l3_reshape$Group1[Group_result_l3_reshape$Group1>0] <- "Yes" 
Group_result_l3_reshape$Group2[Group_result_l3_reshape$Group2>0] <- "Yes" 
Group_result_l3_reshape$Group3[Group_result_l3_reshape$Group3>0] <- "Yes" 
Group_result_l3_reshape$Group4[Group_result_l3_reshape$Group4>0] <- "Yes"  
Group_result_l3_reshape$Group1[Group_result_l3_reshape$Group1==0] <- "No" 
Group_result_l3_reshape$Group2[Group_result_l3_reshape$Group2==0] <- "No" 
Group_result_l3_reshape$Group3[Group_result_l3_reshape$Group3==0] <- "No" 
Group_result_l3_reshape$Group4[Group_result_l3_reshape$Group4==0] <- "No" 

Group_result_l3_bacteria <- read.csv("groupID_bacteria_pathway.csv")
# WIDE - Bacteria
#Group_result_l3_bacteria$value <- "Yes"
Group_result_l3_bacteria_reshape <- dcast(Group_result_l3_bacteria, pathway~Group)
Group_result_l3_bacteria_reshape$Group1[Group_result_l3_bacteria_reshape$Group1>0] <- "Yes" 
Group_result_l3_bacteria_reshape$Group2[Group_result_l3_bacteria_reshape$Group2>0] <- "Yes" 
Group_result_l3_bacteria_reshape$Group3[Group_result_l3_bacteria_reshape$Group3>0] <- "Yes" 
Group_result_l3_bacteria_reshape$Group4[Group_result_l3_bacteria_reshape$Group4>0] <- "Yes"  
Group_result_l3_bacteria_reshape$Group1[Group_result_l3_bacteria_reshape$Group1==0] <- "No" 
Group_result_l3_bacteria_reshape$Group2[Group_result_l3_bacteria_reshape$Group2==0] <- "No" 
Group_result_l3_bacteria_reshape$Group3[Group_result_l3_bacteria_reshape$Group3==0] <- "No" 
Group_result_l3_bacteria_reshape$Group4[Group_result_l3_bacteria_reshape$Group4==0] <- "No" 


ratings2 <- read.csv("all_path_fuzzy_pathnames_sorted.csv")
ratings2$Value <- as.numeric(ratings2$Value)
ratings2$Bacterium <- as.character(ratings2$Bacterium)

# Pie Chart
pie_chart <- read.csv("shiny_pie_chart_data.csv")
colnames(pie_chart) <- c("count_class","bacteria","pathway_class","count_sum","count_percentage%")


# The home button on the top right credit: https://stackoverflow.com/questions/47569992/home-button-in-header-in-r-shiny-dashboard
ui <- dashboardPage( skin = "purple",
                     dashboardHeader(title = "ProGut",tags$li(class = "dropdown", actionButton("home", "Home"))),
                     dashboardSidebar(
                       sidebarMenu(id = "sidebar",
                         menuItem("Home", tabName = "home", icon = icon("home")),
                         menuItem("Top", tabName = "top", icon = icon("cloud")),
                         menuItem("Pie Chart", tabName = "pie", icon = icon("sun")),
                         menuItem('Start from Pathways',
                                  icon = icon('moon'),
                                  menuItem("Start from Genus Pathways", tabName = "group", icon = icon("car")),
                                  menuItem("Start from Bacteria Pathways", tabName = "bacteria", icon = icon("motorcycle"))
                         ),
                         menuItem('Start from Probiotics',
                                  icon = icon('star'),
                                  menuItem("Start from Probiotics (Genus)", tabName = "probiotics", icon = icon("leaf")),
                                  menuItem("Start from Probiotics (Bacteria)", tabName = "probiotics_species", icon = icon("spa"))
                         ),
                         menuItem('Intergrated',
                                  icon = icon('line-chart'),
                                  menuSubItem("Reference Probiotics", tabName = "ref_probiotics_table", icon = icon("baby")),
                                  menuSubItem("All Probiotics", tabName = "probiotics_table", icon = icon("dog"))
                         ),
                         menuItem("Help", tabName = "help", icon = icon("question"))
                         
                       )
                     ),
                     dashboardBody(
                       tabItems(
                         # Homepage
                         tabItem("home", 
                                 tagList(
                                   # head
                                   tags$head(
                                     #tags$link(href="styles.css")
                                   ),
                                   
                                   # parent
                                   tags$div(class="landing-wrapper",
                                            
                                            
                                            # child: images
                                            tags$div(class="landing-block background-content",
                                                     tags$img(class = "home_img", src = "https://upload.wikimedia.org/wikipedia/commons/9/9b/Man_Drinking_Water_Cartoon.svg"),
                                                     tags$img(class = "home_img", src = "https://upload.wikimedia.org/wikipedia/commons/3/31/Man_on_a_Treadmill_Cartoon.svg"),
                                                     tags$img(class = "home_img", src = "https://upload.wikimedia.org/wikipedia/commons/e/e4/Doctor_with_Patient_Cartoon.svg"),
                                                     tags$img(class = "home_img", src = "https://upload.wikimedia.org/wikipedia/commons/7/79/Man_Working_at_his_Desk_Cartoon_Vector.svg")
                                            ),
                                            
                                            # child: content
                                            tags$div(class="landing-block foreground-content",
                                                     tags$div(class="foreground-text",
                                                              tags$h1("ProGut"),
                                                              tags$h3(class="small_words","ProGut is an awesome place where you can know more about the functions of probiotics!"),
                                                              #actionButton(class="button", "top", "Get Started >>")
                                                              actionBttn(
                                                                inputId = "help",
                                                                label = "Get Started!",
                                                                color = "primary",
                                                                style = "bordered",
                                                                icon = icon("sliders"),
                                                              ),
                                                     )
                                            ),
                                            tags$div(
                                              tags$h5("Images credit: (1) https://commons.wikimedia.org/wiki/File:Man_Drinking_Water_Cartoon.svg, (2) https://commons.wikimedia.org/wiki/File:Man_on_a_Treadmill_Cartoon.svg, (3) https://commons.wikimedia.org/wiki/File:Doctor_with_Patient_Cartoon.svg, (4) https://commons.wikimedia.org/wiki/File:Man_Working_at_his_Desk_Cartoon_Vector.svg"),
                                              tags$h5("Layout credit: https://davidruvolo51.github.io/shinytutorials/tutorials/landing-page/")
                                            ),
                                            
                                            
                                            tags$style(HTML("
                         
                                              .landing-block {
                                                  width: 100%;
                                                  height: 90vh;
                                                  
                                                  
                                              }
                                        
                                              .background-content {
                                                  display: grid;
                                                  grid-template-columns: 50% 50%;
                                                  height: 40%;
                                                  /* height: 50;
                                                  width: 150;
                                                  display: center;
                                                  grid-template-columns: 100% */
                                              }
                                         
                                              .background-content img {
                                                  display: block;
                                                  width: 100%;
                                                  margin: 20;
                                                
                                              }
                                          
                                              .foreground-content{
                                                position: absolute;
                                                top: 5%;
                                                left: 8%;
                                                float: left;
                                                z-index: 9999;
                                                display: flex;
                                                justify-content: center;
                                                align-items: center;
                                                
                                              }
                         
                                             .foreground-content .foreground-text {
                                                width: 40%;
                                                padding: 7.5%;
                                                color: black;
                                                background-color: white;
                                                text-align: center;
                                                box-shadow: 0 4px 6px 2px hsla(0, 0%, 0%, 0.18);
                                                background-color: #e6e6ff;
                                             }
                                         
                                     
                                             .button{
                                              height: 60px;
                                              width: 60px;
                                              border-radius: 50%;
                                              border: 1px solid red;
                                             }
                                         
                                             .small_words{
                                              font-family: BentonSans Book;
                                              font-style: italic
                                             }"
                                                            
                                            ))
                                            
                                            
                                            
                                   ) # end parent div
                                 ) ),
                      
                         
                         # Top 10
                         tabItem("top",
                                 fluidPage(theme = "styles.css",
                                           
                                           # CSS
                                           tags$head(
                                             tags$style(HTML("
                                                body { background-color: #f2efe9; }
                                                .container-fluid { background-color: #fff; width: 1100px; padding: 60px; }
                                                .topimg { width: 300px; display: block; margin: 0px auto 40px auto; }
                                                .title { text-align: center; }
                                                .toprow { margin: 60px 0px; padding: 30px; background-color: #fae8bb; }
                                                .filters { margin: 0px auto; }
                                                .shiny-input-container { width:100% !important; }
                                                .table { padding: 30px; margin-top: 30px; }
                                                .leaflet-top { z-index:999 !important; }
      
                                              "))
                                           ),  
                                           
                                           # Application title
                                           h1("Probiotics Review", class = "title"),
                                           
                                           # Top image https://imgbb.com
                                           img(class = "topimg", src = "https://upload.wikimedia.org/wikipedia/commons/f/fb/Cute_cartoon_clouds_with_stars.svg"),
                                           
                                           
                                           
                                           # Add the description
                                           h5("This tab provides information about the importance of probiotics based on the ranking values of bacteria pathways. The value assigned to each probiotic explains how much that bacteria contributes to the biochemical pathway. Specifically, a higher value indicates more contribution."),
                                           h5("The results are organized in three different ways, including a bar plot for the top 10 bacteria that strongly contribute to specific metabolic pathways, a density plot showing the distribution of bacterial contribution to metabolic pathways, and a table with detailed information for each bacterium."),
                                           tags$div(
                                             tags$h5(em("Images credit: https://commons.wikimedia.org/wiki/File:Cute_cartoon_clouds_with_stars.svg"))
                                           ),
                                           
                                           fluidRow(
                                             fluidRow(
                                               column(6,
                                                      # Pathway Class menu
                                                      selectInput("pathway_class3", "Pathway Class", choices = ratings2$Pathway_Class)
                                                      
                                               ),
                                               
                                               column(6,
                                                      
                                                      selectInput("pathway3", "Pathway", choices = NULL), # Sort options alphabetically
                                               )
                                               
                                               
                                             )
                                           ),
                                           
                                           fluidRow(
                                             
                                             column(10, align="center", class = "bar",
                                                    # Bar Chart
                                                    plotOutput("brandBar3")
                                             ),
                                             br(),
                                             br(),
                                             column(11, align="center", class="density",
                                                    plotOutput("density")
                                                    
                                             ),
                                             
                                             fluidRow(class = "table",
                                                      # Table
                                                      dataTableOutput("table3")
                                             )
                                             
                                             
                                           )
                                           
                                          
                                 )
                           
                         ),
                         
                         # Pie Chart
                         tabItem("pie",
                                 fluidPage( theme = "styles.css",
                                            
                                            # CSS
                                            tags$head(
                                              tags$style(HTML("
                                                body { background-color: #f2efe9; }
                                                .container-fluid { background-color: #fff; width: 1100px; padding: 60px; }
                                                .topimg2 { width: 300px; display: block; margin: -10px auto 10px auto; }
                                                .title { text-align: center; }
                                                .toprow { margin: 60px 0px; padding: 30px; background-color: #fae8bb; }
                                                .filters { margin: 0px auto; }
                                                .shiny-input-container { width:100% !important; }
                                                .pie_table { padding: 30px; margin-top: 30px; }
                                                .leaflet-top { z-index:999 !important; }
                                              "))
                                            ),              
                                            
                                            
                                            h1("Pie Chart for Probiotics", class = "title"),
                                            # Top image
                                            img(class = "topimg2", src = "https://upload.wikimedia.org/wikipedia/commons/f/fb/Cute_cartoon_clouds_with_stars.svg"),
                                            
                                            # Application title
                                            
                                            h5("This tab offers two kinds of output, including a pie chart and a detailed table."),
                                            h5("The pie chart presents the relative sizes of pathway classes for each bacterium. Pathways with many enzymes are likely to be important to their respective bacteria. In addition, the result page contains a detailed table with normalized enzyme counts for each bacteria pathway."),
                                            tags$div(
                                              tags$h5(em("Images credit: https://commons.wikimedia.org/wiki/File:Cute_cartoon_clouds_with_stars.svg"))),
                                            
                                            fluidRow(
                                              fluidRow(
                                                
                                                column(6,
                                                       selectInput('bacteria', 'Select a bacterium',
                                                                   choices = pie_chart$bacteria)
                                                ),
                                                
                                              )
                                            ),
                                            
                                            fluidRow(
                                              
                                              column(12, class = "bar",
                                                     # Pie Chart
                                                     plotOutput("pie_brandBar")
                                              ),
                                              
                                            ),
                                            
                                            fluidRow(class = "pie_table",
                                                     # Table
                                                     dataTableOutput("pie_table")
                                                     #DT::datatable("pie_table")
                                            )
                                          
                                            
                                 )
                           
                         ),
                         
                         
                         # Genus level
                         # tabItem("group",
                         #         h5("This tab allows users to obtain a bacteria list based on specific genus pathways. According to the results, the user can know what genera potentially possess the pathways of interest."),
                         #         sidebarLayout(
                         #           sidebarPanel(
                         #             selectInput('groupPathway', 'Select a pathway',
                         #                         #choices = Group_result_l3$Common_PathName
                         #                         choices = Group_result_l3$pathway)
                         #             
                         #           ),
                         #           mainPanel(
                         #             tableOutput('groupPathwayData'),
                         #             style = "overflow-y: scroll;overflow-x: scroll;"
                         #             # style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                         #           )
                         #         )
                         #         
                         # ),
                         
                         # WIDE - Genus
                         tabItem("group",
                                 h5("This tab allows users to understand the functions of bacteria groups based on specific genus pathways. According to the results, the user can know which potential bacteria groups can be used to complete the pathways of interest. It requires the user to input any search criteria such as the genus pathways of interest into a search box. In this case, genus pathways for each bacteria group were obtained based on common pathways of all genera within the group. The results are displayed in a table where you can see all entries that match the search box criteria."),
                                 h5("Arrows next to the column names can be used to quickly sort the data."),
                                 fluidPage(
                                   h1("Genus Functions of Bacteria Groups"),
                                   #dataTableOutput("probiotics_group_table"),
                                   DT::datatable(Group_result_l3_reshape, options = list(language = list(searchPlaceholder = "Genus Pathway"))),
                                   style = "overflow-y: scroll;overflow-x: scroll;"
                                   #style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                                 )
                                 
                         ),
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         # Bacteria level
                         # tabItem("bacteria",
                         #         h5("This tab allows users to obtain a bacteria list based on their individual pathways. According to the results, the user can know what bacteria (groups) potentially have the pathways of interest."),
                         #         sidebarLayout(
                         #           sidebarPanel(
                         #             selectInput('bacteriaPathway', 'Select a pathway',
                         #                         choices = Group_result_l3_bacteria$pathway)
                         #             
                         #           ),
                         #           mainPanel(
                         #             tableOutput('bacteriaPathwayData'),
                         #             style = "overflow-y: scroll;overflow-x: scroll;"
                         #             # style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                         #           )
                         #         )
                         #         
                         # ),
                         
                         # WIDE - bacteria
                         tabItem("bacteria",
                                 h5("This tab allows users to understand the functions of bacteria groups based on specific individual pathways. It requires the user to input any search criteria such as bacteria pathways of interest into a search box. Bacteria pathways for each bacteria group refer to union pathways bacteria within the group possess. The results are displayed in a table where you can see all entries that match the search box criteria."),
                                 h5("Arrows next to the column names can be used to quickly sort the data."),
                                 fluidPage(
                                   h1("Functions of Bacteria Groups"),
                                   #dataTableOutput("probiotics_group_table"),
                                   DT::datatable(Group_result_l3_bacteria_reshape, options = list(language = list(searchPlaceholder = "Bacteria Pathway"))),
                                   style = "overflow-y: scroll;overflow-x: scroll;"
                                   #style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                                 )
                                 
                         ),
                         
                         
                  
                         tabItem("probiotics",
                                 h5("This tab requires the user to input any search criteria into a search box. It can be bacteria group ids, bacteria names or the genus names. The results are displayed in a table where you can see all entries that match the search box criteria."),
                                 h5("The table possesses four columns: group (bacteria group ids), bacteria, genus pathways, and genus names."),
                                 fluidPage(
                                   h1("Probiotics with Genus-Level Pathways"),
                                   #dataTableOutput("probiotics_group_table"),
                                   DT::datatable(Group_result_l3, options = list(language = list(searchPlaceholder = "Group id / Pathway"))),
                                   style = "overflow-y: scroll;overflow-x: scroll;"
                                   #style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                                 )
                                 
                         ),
                         
                         tabItem("probiotics_species",
                                 h5("This tab requires the user to input any search criteria into a search box. It can be bacteria group ids or bacteria names. The results are displayed in a table where you can see all entries that match the search box criteria."),
                                 h5("The table possesses four columns: group (bacteria group ids), bacteria, and pathways."),
                                 fluidPage(
                                   h1("Probiotics with Bacteria Pathways"),
                                   #dataTableOutput("probiotics_group_table"),
                                   DT::datatable(Group_result_l3_bacteria, options = list(language = list(searchPlaceholder = "Group id / Pathway"))),
                                   style = "overflow-y: scroll;overflow-x: scroll;"
                                   #style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                                 )
                                 
                         ),

                         
                         
                         tabItem("probiotics_table",
                                 fluidPage(
                                   h5("This tab offers a search box which allows the user to retrieve the probiotic data of interest conveniently."),
                                   h1("Probiotics"),
                                   #dataTableOutput("bigtable"),
                                   DT::datatable(Shiny_big_table, options = list(language = list(searchPlaceholder = "Bacteria / Brand / Effect")),rownames=FALSE,escape = FALSE),
                                   style = "overflow-y: scroll;overflow-x: scroll;width:100%"
                                   
                                 )
                                 
                         ),
                         
                         
                         # Reference
                         tabItem("ref_probiotics_table",
                                 
                                 h5("The Reference Probiotics under the Integrated tab offers information such as in-depth references, alternative names, and related effects of the bacteria chose by the user in a tabular display."),
                                   
                                   fluidRow(
                                       #class = "test",
                                       selectInput('refProbiotics', 'Select a bacteria',
                                                   #choices = Shiny_big_table$All_Real_Name
                                                   choices = Shiny_big_table$Used_Bacteria
                                       
                                        ),
                                  ),
                                 fluidRow(
                                   tableOutput('refProbioticsData'),
                                   style = "overflow-y: scroll;overflow-x: scroll;"
                                   # style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                                 )
                                 
                         ),
                         
                         # Help
                         tabItem("help", 
                                 #"This is Tab1"
                                 tags$div(
                                   
                                   tags$div(
                                     tags$h4(strong("1. WHAT CAN I DO ON THE PROGUT?")),
                                     tags$h4("> Obtain the general introduction of the ProGut."),
                                     tags$h4("> Get information about top 10 bacteria with the ability to catalyze biochemical metabolism associated with the pathway classes and pathways selected by the user."),
                                     tags$h4("> Identify various biochemical pathway classes for the bacteria chosen by the user."),
                                     tags$h4("> Explore bacteria groups/communities associated with the biochemical pathway queried by the user."),
                                     tags$h4("> Explore members in bacteria groups/communities and common biochemical pathways associated with bacteria groups/communities."),
                                     tags$h4("> Explore detailed information about the bacteria selected by the user."),
                                     tags$h4("> Explore detailed information about the bacteria categorized by generic search criteria input by the user.")
                                     
                                   ),
                                   
                                   tags$br(),
                                   
                                   tags$div(
                                     tags$h4(strong("2. CONTENTS")),
                                     
                                     # 2.1
                                     tags$h4(strong("2.1 Obtain the general introduction of the ProGut.")),
                                     tags$h4("Display an overview of ProGut from the home page."),
                                     tags$h4("There are two options can be picked to get to the home page:"),
                                     
                                     tags$h4(
                                       tags$ul(
                                         tags$li(tags$span(strong("Option 1:"),"Click the \"Home\" tab from the sidebar on the left of the page.")),
                                         tags$li(tags$span(strong("Option 2:"), "Click the \"Home\" button located at the right top corner of the ProGut."))
                                       )
                                     ),
                                     tags$h4("The user can start from the home page and get some detailed descriptions of each tab (from the \"Help\" tab) by clicking the button in the middle of the home page or click the \"Help\" link from the sidebar directly.")
                                   ),
                                   
                                   tags$br(),
                                   
                                   # 2.2
                                   tags$h4(strong("2.2 Get information about top 10 bacteria with the ability to catalyze biochemical metabolism associated with the pathway classes and pathways selected by the user.")),
                                   tags$h4("The \"Top\" tab provides information about the importance of probiotics based on the ranking values of bacteria pathways. The value assigned to each probiotic explains how much that bacteria contributes to the biochemical pathway, where a higher value indicates more contribution."),
                                   tags$h4("Three types of results, including a bar plot, a density plot, and a detailed table can be obtained using the following steps:"),
                                   
                                   tags$h4(
                                     tags$ul(
                                       tags$li(tags$span(strong("Step 1:"),"Click the \"Top\" tab from the sidebar on the left.")),
                                       tags$li(tags$span(strong("Step 2:"), "Select one pathway class from the “Pathway Class” drop-down list.")),
                                       tags$li(tags$span(strong("Step 3:"), "Select one pathway from the “Pathway” drop-down list."))
                                     )
                                   ),
                                   
                                   tags$h4("From the density plot, users can know what the top 10 bacteria are for the selected pathway. If users want to explore contributions of all bacteria to a pathway, they can continue to check the density plot and detailed table on the same page. The density plot presents the distribution of the ranking values of all the bacteria with the selected pathway while the detailed table assists the user to keep track of the real raking value that each bacterium has."),
                                   tags$h4("Additionally, results in the table can also be sorted by clicking the arrows next to the column names."),
                                   
                                   tags$br(),
                                   
                                   # 2.3
                                   tags$h4(strong("2.3 Identify various biochemical pathway classes for the bacteria chosen by the user.")),
                                   tags$h4("The \"Pie Chart\" tab offers two kinds of output, including a pie chart and a detailed table."),
                                   tags$h4("The pie chart presents the importance of functions/pathway classes for the selected bacteria. The importance of functions is evaluated by the enzyme counts involved in the functions. In addition, the result contains a detailed table with real enzyme counts and their percentages for the selected bacterium. 
                        Steps shown below are used to obtain the outcomes:"),
                                   tags$h4(
                                     tags$ul(
                                       tags$li(tags$span(strong("Step 1:"),"Enter the \"Pie Chart\" tab by clicking the \"Pie Chart\" link from the sidebar.")),
                                       tags$li(tags$span(strong("Step 2:"), "Select one of the bacteria from the drop-down menu on the page. A pie chart and detailed table will be generated based on the input from the drop-down menu.")),
                                       tags$li(tags$span(strong("Step 3:"),"Click the arrows next to the column headings of the items to sort the results in the detailed table by either ascending or descending. The column definitions of the detailed table are shown below:")),
                                       tags$h4(
                                         tags$ul(
                                           tags$li(tags$span(strong("pathway_class:"),"name of each pathway class")),
                                           tags$li(tags$span(strong("count_class:"), "enzyme count for each pathway class")),
                                           tags$li(tags$span(strong("count_percentage%:"), "calculated percentage of enzyme count compare to total enzyme count"))
                                         )
                                       )
                                     )
                                   ),
                                   
                                   tags$br(),
                                   
                                   # 2.4
                                   tags$h4(strong("2.4 Explore bacteria groups/communities associated with the biochemical pathway queried by the user.")),
                                   tags$h4("The \"Start from Pathways\" tab contains two subtabs, which allows users to understand the functions of bacteria groups/communities at two levels: genus and bacteria (strain) levels, respectively. With this tab, the user can know which potential bacteria groups they can choose to complete the pathways of interest. The following steps are taken to attain the results:"),
                                   tags$h4(
                                     tags$ul(
                                       tags$li(tags$span(strong("Step 1:"),"Click the \"Start from Pathways\" link from the sidebar.")),
                                       tags$li(tags$span(strong("Step 2:"),"Click the \"Start from Genus Pathways\" subtab under the \"Start from Pathways\" tab."))
                                     )
                                   ),
                                   tags$h4("The outcomes will be organized in a table on the same page. Several kinds of information, including the genus pathways and the status (absence or presence) of 4 bacterial communities for genus pathways can be retrieved from the table. Genus pathways for each bacteria group refer to common pathways of all genera within the same bacteria group."),
                                   tags$h4("The outcomes at the bacteria (strain) level can be obtained from the \"Start from Bacteria Pathways\" subtab using similar steps listed above. Bacteria pathways for each bacteria group refer to union pathways that all the bacteria within the group possess."),
                                   tags$br(),
                                   
                                   # 2.5
                                   tags$h4(strong("2.5 Explore members in bacteria groups/communities and common biochemical pathways associated with bacteria groups/communities.")),
                                   tags$h4("The \"Start from Probiotics\" tab contains two subtabs, which allows users to further retrieve specific bacteria names from each community with the pathways at two levels accessible through two subtabs: \"Start from Probiotics (Genus)\" and \"Start from Probiotics (Bacteria)\". Both of two subtabs require the user to input any search criteria into a search box on the corresponding subtab. The search query can be bacteria group ids, bacteria names, or pathways. The results are displayed in a table where you can see all entries that match the search box criteria. "),
                                   tags$h4("The table possesses four columns: group (bacteria group ids), bacteria, genus pathway, and genus names. The following steps can be utilized to get the results:"),
                                   tags$h4(
                                     tags$ul(
                                       tags$li(tags$span(strong("Step 1:"),"Select the \"Start from Probiotics\" tab from the sidebar.")),
                                       tags$li(tags$span(strong("Step 2:"),"Select the \"Start from Probiotics (Genus)\" subtab under the \"Start from Probiotics\" tab.")),
                                       tags$li(tags$span(strong("Step 3:"), "Type the key words of interest in the search box."))
                                     )
                                   ),
                                   tags$h4("All the relevant results will be displayed in the table shown below the search box."),
                                   tags$h4("The outcomes at the bacteria (strain) level can be obtained from the \"Start from Probiotics (Bacteria)\" subtab using similar steps listed above."),
                                   tags$br(),
                                   
                                   # 2.6
                                   tags$h4(strong("2.6 Explore detailed information about the bacteria selected by the user.")),
                                   tags$h4("The \"Reference Probiotics\" under the \"Integrated\" tab offers information such as in-depth references, alternative names, and related effects of the bacteria chose by the user in a tabular display. The following steps can be used to complete this function:"),
                                   tags$h4(
                                     tags$ul(
                                       tags$li(tags$span(strong("Step 1:"),"Click the \"Reference Probiotics\" tab from the sidebar on the left.")),
                                       tags$li(tags$span(strong("Step 2:"), "Choose one bacterium from the drop-down list and the results will be shown in a table below the input menu."))
                                     )
                                   ),
                                   tags$h4("The definitions of the features shown in the table are listed below:"),
                                   tags$h4(
                                     tags$ul(
                                       tags$li(tags$span(strong("Name:"),"bacteria name used in the public database Probio. A star (\"*\") is attached to the bacteria if it is represented by other reference/representative genome")),
                                       tags$li(tags$span(strong("Strain:"), "strain information of bacteria")),
                                       tags$li(tags$span(strong("Brand:"),"brand that develops the health supplements using the known probiotics")),
                                       tags$li(tags$span(strong("Used_Bacteria:"),"reference or representative bacteria used in this study")),
                                       tags$li(tags$span(strong("Species.Category:"),"the object where the probiotic uses")),
                                       tags$li(tags$span(strong("Status:"),"the status of the health supplement")),
                                       tags$li(tags$span(strong("Effect:"),"general effect that the probiotic brings")),
                                       tags$li(tags$span(strong("Class:"),"the class of the disease that the probiotic works for")),
                                       tags$li(tags$span(strong("Reference:"),"the reference where probiotic effects were obtained")),
                                       tags$li(tags$span(strong("Detailed Effect:"),"detailed influence brought by the probiotic")),
                                       tags$li(tags$span(strong("NCBI.Genome:"),"link to attain the relevant bacteria NCBI genome")),
                                       tags$li(tags$span(strong("mbgd.Genome:"),"MBGD database link of the related probiotic"))
                                     )
                                   ),
                                   
                                   tags$br(),
                                   
                                   # 2.7
                                   tags$h4(strong("2.7 Explore detailed information about the bacteria categorized by generic search criteria input by the user.")),
                                   tags$h4("Another subtab \"All Probiotics\" tab offers a search box which allows the user to retrieve the probiotic data of interest conveniently."),
                                   tags$h4("The results are organized into a table with 12 columns which have the same definitions as the columns identified in", strong("Function 2.5"),". The user can use the steps below to complete this function:"),
                                   tags$h4(
                                     tags$ul(
                                       tags$li(tags$span(strong("Step 1:"),"Select the \"All Probiotics\" tab from the sidebar.")),
                                       tags$li(tags$span(strong("Step 2:"), "Type the key words of interest such as potential effects of probiotics and the health supplements containing specific types of probiotics in the search box on the right side. All the relevant results that contain the key words will be displayed in the table below the search box."))
                                     )
                                   )
                                 )
                         )
                         
                       )
                       
                       
                       
                     )
)
server <- function(session,input, output){
  observe({
    # Filter data based on selected Style
    
    x <- ratings2$Pathway[ratings2$Pathway_Class == input$pathway_class]
    
    updateSelectInput(session, "pathway2", choices = x)
    
    y <- ratings2$Pathway[ratings2$Pathway_Class == input$pathway_class3]
    
    updateSelectInput(session, "pathway3", choices = y)
    
    
  })
  
  
  # Homepage
  observeEvent(input$home, {
    updateTabItems(session, "sidebar", "home")
  })
  
  observeEvent(input$help, {
    updateTabItems(session, "sidebar", "help")
  })
  
  
  output$brandBar <- renderPlot({
    
    if (input$pathway != "All") {
      ratings2 <- filter(ratings2, Pathway == input$pathway2)
    }
    
    
    validate (
      need(nrow(ratings2) > 0, "No reviews found. Please make another selection." )
    )
    d <- density(ratings2$Value)
    plot(d)
  })
  
  output$bigtable <- renderDataTable(Shiny_big_table,escape = FALSE,rownames=FALSE)
  
  # Group
  output$groupPathwayData <- renderTable({
    grouppathwayFilter <- subset(Group_result_l3, Group_result_l3$pathway == input$groupPathway)
  })
  
  # Bacteria
  output$bacteriaPathwayData <- renderTable({
    bacteriapathwayFilter <- subset(Group_result_l3_bacteria, Group_result_l3_bacteria$pathway == input$bacteriaPathway)
  })
  
  # Reference
  output$refProbioticsData <- renderTable({
    #Shiny_big_table$NCBI.Genome <- paste0("<a href='",Shiny_big_table$NCBI.Genome,"'>",Shiny_big_table$NCBI.Genome,"</a>")
    refprobioticsFilter <- t(subset(Shiny_big_table, Shiny_big_table$Used_Bacteria == input$refProbiotics))
    Features <- c("Name","Strain","Brand","Used_Bacteria","Species.Category","Status","Effect","Class","Reference","Detailed.Effect","NCBI.Genome","mbgd.Genome")
    refprobioticsFilter <- cbind(Features,refprobioticsFilter)
   
  }, sanitize.text.function=function(x) x)
  
  #output$probiotics_group_table <- renderDataTable(Group_result_l3,escape = FALSE,rownames=FALSE)
  
  #Plot for Trace Explorer
  output$trace_plot <- renderPlot({
    plot(iris$Sepal.Length,iris$Sepal.Width)
  })

  # Create bar chart of brands
  output$brandBar3 <- renderPlot({ # Need to be careful here since we have the same name brandBar
    
    
    # Filter data based on selected Style
    if (input$pathway3 != "All") {
      # Be careful here - because the rating2 has the same names as before
      ratings2 <- filter(ratings2, Pathway == input$pathway3)
    }
    
    validate (
      need(nrow(ratings2) > 0, "No reviews found for the Bar Plot. Please make another selection." )
    )
    
    # Get top 20 brands
    top_bacteria <- group_by(ratings2, Bacterium) %>% 
      summarise(valRating = Value) %>% 
      arrange(desc(valRating)) %>% 
      top_n(10)
    
    # Bar chart
    ggplot(top_bacteria, aes(reorder(Bacterium, valRating))) +
      geom_bar(aes(weight = valRating), fill = "tomato3") + 
      coord_flip() +
      ggtitle("Top 10 Probiotics") +
      xlab("Bacteria") +
      ylab("Value") +
      theme_bw(base_size = 16) +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  output$table3 <- DT::renderDataTable({
    
    # Filter data based on selected Style
    if (input$pathway3 != "All") {
      ratings2 <- filter(ratings2, Pathway == input$pathway3)
    }
    
    # Hide table when user has filtered out all data
    validate (
      need(nrow(ratings2) > 0, "")
    )
    
    ratings2[,c(5,1,2,3)]
    
  },options = list(language = list(searchPlaceholder = "Pathway / Bacterium")))
  
  output$density <- renderPlot({
    if (input$pathway3 != "All") {
      ratings2 <- filter(ratings2, Pathway == input$pathway3)
    }
    
    validate (
      need(nrow(ratings2) > 0, "No reviews found for the Density Plot. Please make another selection." )
    )
    
    
    e <- density(ratings2$Value)
    plot(e, main="Density Plot For ALL Bacteria", xlab=expression("Value"), cex.lab = 1.5,cex.main = 1.5)
  })
  
  # Pie Chart
  # Create bar chart of brands
  output$pie_brandBar <- renderPlot({
    
    # Filter data based on selected Style
    if (input$bacteria != "All") {
      pie_chart <- filter(pie_chart, bacteria == input$bacteria)
    }
    
    validate (
      need(nrow(pie_chart) > 0, "No reviews found. Please make another selection." )
    )
    
    # Pie Chart
    ggplot(pie_chart, aes(x="", y=count_class, fill=pathway_class)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0) + theme_void()
  })
  
  
  
  # Create data table
  output$pie_table <- DT::renderDataTable({
    
    # Filter data based on selected Style
    if (input$bacteria != "All") {
      pie_chart <- filter(pie_chart, bacteria == input$bacteria)
    }
    
    # Hide table when user has filtered out all data
    validate (
      need(nrow(pie_chart) > 0, "")
    )
    pie_chart[,c(3,1,5)]
    
  },options = list(language = list(searchPlaceholder = "Pathway Class")))
  
  
  
  
}

shinyApp(ui=ui, server=server)

# dataTableOutput tableOutput
