############################################################################################
# 
#   Clean UI script
# 
#
############################################################################################

#necessary packages
library(shiny)
library(fresh)
library(shinyWidgets)
library(htmltools)
library(DT)
library(shinyjs)

#to get the vector of food items
source("TableauConstruction.R")

#create a theme
app_theme <- create_theme(
  #referenced from https://dreamrs.github.io/fresh/articles/vars-shiny.html
  bs_vars_button(
    font_weight = 500,
    border_radius_base = 0,
    default_color = "#112446",
    default_border = "#112446",
    primary_color = "#FFF",
    primary_bg = "#112446",
    primary_border = "#112446"
  ),
  
  bs_vars_color(
    brand_primary = "#112446",
    brand_success = "#7bc043",
    brand_info = "#0392cf",
    brand_warning = "#f37736",
    brand_danger = "#ee4035"
  ),
  
  bs_vars_navbar(
    padding_horizontal = "15px",
    default_bg = "#112446",
    default_color = "#FFFFFF",
    default_link_color = "#FFFFFF",
    default_link_active_color = "#FFFFFF",
    default_link_hover_color = "#A4A4A4"
  ),
  
  bs_vars_wells(
    bg = "#FFF",
    border = "#3f2d54"
  ),
  
  bs_vars_tabs(
    border_color = "#112446",
    active_link_hover_bg = "#FFF",
    active_link_hover_color = "#112446",
    active_link_hover_border_color = "#112446",
    link_hover_border_color = "#112446"
  )
)


#create the ui
ui <- navbarPage(
  "CSMC 150 Dashboard",
  id = "main",
  
  #Home Panel
  tabPanel(
    "Home",
    use_theme(app_theme),
    
    tags$head(tags$style(HTML(".heading{font-weight: bold;} .HomePanels { height: 450px;} .BottomPanel {height : 130px;} .HomeBttns { position: absolute; bottom: 40px; left: 50%; transform: translateX(-50%); font-size: 20px; padding: 10px 20px;} img{padding: 1px 1px;} .Aboutbttns{font-size: 15px; padding: 10px 20px; margin: 20px;}"))),
    
    fluidRow(
      column(12, wellPanel(
        tags$head(tags$style(HTML("h1,h5 { margin: 0;} h4 {font-weight: normal;} h1 {font-weight: bold;}"))),
        tags$h1("Welcome to CMSC 150 Dashboard!"),
        tags$hr(),
        tags$h4("This application/dashboard is done in fulfillment of the requirements of CMSC 150: Numerical and Symbolic Computation, and contains modules applying Polynomial Regression, Quadratic Spline Interpolation, and Simplex Method. Check out the modules to see what they can do!")
      ))
    ),
    
    fluidRow(
      column(4, wellPanel(
        class = "HomePanels",
        tags$head(tags$style(HTML("h3,h5 { margin: 0;} h5 {font-weight: normal; font-size: 17px; text-align: justify;}"))),
        tags$h3("Quadratic Spline Interpolation"),
        tags$hr(),
        tags$h5("This module implements Quadratic Spline Interpolation (QSI) on the user file input and outputs the set of quadratic equations, plot of the observations and the set of quadratic equations, and the interpolated value at the user specified input."),
        tags$img(src = "qsi_sample.jpg", width = "100%"),
        actionButton("QSI", "Quadratic Spline Interpolation", class = "HomeBttns", class = "btn-primary")
      )),
      column(4, wellPanel(
        class = "HomePanels",
        tags$head(tags$style(HTML("h3 { margin: 0;} h5 {font-weight: normal; font-size: 17px; text-align: justify;}"))),
        tags$h3("Polynomial Regression"),
        tags$hr(),
        tags$h5("This module implements Polynomial Regression on the user file input and outputs the fitted nth order polynomial function given the user function order input, plot of the observations and the fitted model, and the estimated value at the user specified input."),
        tags$img(src = "pr_sample.jpg", width = "100%"),
        actionButton("PR", "Polynomial Regression", class = "HomeBttns", class = "btn-primary")
      )),
      column(4, wellPanel(
        class = "HomePanels",
        tags$head(tags$style(HTML("h3 { margin: 0;} h5 {font-weight: normal; font-size: 17px; text-align: justify;}"))),
        tags$h3("Simplex Implementation"),
        tags$hr(),
        tags$h5("This module implements Simplex Method in a Diet Solver Problem, wherein users can select as many food items as they like, and the module outputs the optimal diet given the user input, at minimum cost, along with the computations."),
        tags$img(src = "sm_sample.png",  width = "100%", alt = "Sample Simplex Implementation Output"),
        actionButton("SM", "Simplex Implementation", class = "HomeBttns", class = "btn-primary")
      ))
    ),
    
    fluidRow(
      column(6, wellPanel(
        class = "BottomPanel",
        div(
          style = "display: flex; align-items: center;",
          tags$img(src = "me.png", width = "15%", style = "margin-right: 10px;"),
          div(
            style = "flex-grow: 1;",
            tags$h3("About the Developer"),
            tags$h5("Rhys Allen V. Abejay", style = "margin: 0px; font-weight: bold"),
            tags$hr(style = "margin: 5px"),
            tags$p("I am a 4th year BS Statistics student from the University of the Philippines Los Baños whose interests include spatial analysis and programming.", style = "margin: 0px; line-height: 1;"),
          )
        )
      )),
      column(6, wellPanel(
        class = "BottomPanel",
        shinyjs::useShinyjs(),
        style = "text-align: center; display: flex; align-items: center; justify-content: center;", 
        actionButton("About", "About", class = "Aboutbttns", class = "btn-default"),
        actionButton("LinkedIn", "LinkedIn", class = "Aboutbttns", class = "btn-default", onclick = "shinyjs.openLink('https://www.linkedin.com/in/rvabejay/')"),
        actionButton("GitHub", "GitHub", class = "Aboutbttns", class = "btn-default", onclick = "shinyjs.openLink('https://github.com/rvabejay')"),
        )
      )
    ),
  ),
  
  
  #QSI Panel
  tabPanel(
    "Quadratic Spline Interpolation",
    
    sidebarLayout(
      sidebarPanel(
        
        tags$head(tags$style(HTML("h3 { margin: 0;} #qsi_run{margin: 5px 0px 5px 0px;}"))),
        
        tags$h3("Quadratic Spline Interpolation"),
        
        helpText("This module performs quadratic spline interpolation on the user file input."),
       
        #File Input and Loading
        #Accepts a single csv file input
        h4("File Input", class = "heading"),
        
        fileInput("qsi_input", "Choose CSV File", multiple = FALSE, accept = c(".csv")),
        
        #Checkbox for the user to indicate if the file input has column names or none
        checkboxInput("qsi_header", "My file has headers.", FALSE),
        
        #Button to load the file
        actionButton("qsi_fileLoad", "Load File"),
        
        helpText("Make sure the file is in CSV format, has two columns with equal number of rows and are both of numeric type."),
        
        tags$hr(),
        
        #Quadratic Spline Interpolation and Plotting
        h4("Quadratic Spline Interpolation", class = "heading"),
      
        #Button to perform QSI
        actionButton("qsi_run", "Perform QSI", class = "btn-primary"),
        
        helpText("Click the button to perform QSI on the file input."),
        
        tags$hr(),
        
        #Find the estimate
        h4("Estimation", class = "heading"),
        
        #Input for the value to be estimated
        numericInput("qsi_interpolate", "Interpolate at x:", 0),
        
        actionButton("qsi_runInterpolation", "Interpolate", class = "btn-primary")
      ),
      
      mainPanel(
        
        tabsetPanel(
          type = "tabs",
          id = "QSI_tabs",
          
          tabPanel(
            "Input",
            
            tags$hr(),
            
            #output to indicate whether file is loaded or otherwise
            textOutput("qsi_fileLoading"),
          
            tags$hr(),
            
            #output input file
            DTOutput("qsi_fileInput")
          ),
          
          
          tabPanel(
            "Output",
            
            tags$head(tags$style(HTML("#estimatedQSIVal{ color: #ee4035; font-weight: bold; font-size: 20px}"))),
            
            tags$hr(),
            
            #output for the qsi
            #will show error message if parameters are invalid
            h4("QSI Equations:", class = "heading"),
            br(),
            DTOutput("fittedQSI"),
            
            tags$hr(),
            
            #output for the plot of the fitted model
            h4("QSI Equations Plot:", class = "heading"),
            plotOutput("QSIPlot"),
            
            tags$hr(),
            
            #output for the estimated value at the specified value by the user using the fitted model
            #will show error message if there is no model generated
            h4("Interpolated Value:", class = "heading"),
            textOutput("estimatedQSIVal"),
            
            br()

          )
          
        )
        
      )
      
    ),
  ),
  
  
  #Polynomial Regression Panel
  tabPanel(
    "Polynomial Regression",
    
    sidebarLayout(
      sidebarPanel(
        
        tags$head(tags$style(HTML("h3 { margin: 0;}"))),
        
        tags$h3("Polynomial Regression"),
        
        helpText("This module performs polynomial regression on the user file input."),
        
        #File Input and Loading
        #Accepts a single csv file input
        h4("File Input", class = "heading"),
        
        fileInput("pr_input", "Choose CSV File", multiple = FALSE, accept = c(".csv")),
        
        #Checkbox for the user to indicate if the file input has column names or none
        checkboxInput("pr_header", "My file has headers.", FALSE),
        
        #Button to load the file
        actionButton("pr_fileLoad", "Load File"),
        
        helpText("Make sure the file is in CSV format, has two columns with equal number of rows and are both of numeric type."),
        
        tags$hr(),
        
        h4("Polynomial Regression Fitting", class = "heading"),
        
        helpText("Acceptable function orders are integers and should be at most one less than the total number of data items."),
        
        #Input for the function order
        numericInput("pr_order", "Function Order:", 1, min = 1),
        
        #Button to run the model
        actionButton("pr_modelFitting", "Fit Model", class = "btn-primary"),
        
        tags$hr(),
        
        #Find the estimate
        h4("Estimation", class = "heading"),
        
        #Input for the value to be estimated
        numericInput("pr_estimate", "Estimate at x:", 0),
        
        actionButton("pr_runEstimate", "Estimate", class = "btn-primary")
        
      ),
      
      mainPanel(
        
        tabsetPanel(
          type = "tabs",
          id = "PR_tabs",
          
          tabPanel(
            "Input",
            
            tags$hr(),
            
            #output to indicate whether file is loaded or otherwise
            textOutput("pr_fileLoading"),
            
            tags$hr(),
            
            #Output Input File
            DTOutput("fileInput")
            
          ),
          
          tabPanel(
            "Output",
            
            tags$hr(),
            
            tags$head(tags$style(HTML("#estimatedVal{ color: #ee4035; font-weight: bold; font-size: 20px}"))),
            
            #output for the fitted model
            #will show error message if parameters are invalid
            h4("Fitted Model:", class = "heading"),
            br(),
            textOutput("fittedModel"),
            
            tags$hr(),
            
            #output for the plot of the fitted model
            h4("Model Plot:", class = "heading"),
            plotOutput("modelPlot"),
            
            tags$hr(),
            
            #output for the estimated value at the specified value by the user using the fitted model
            #will show error message if there is no model generated
            h4("Estimated Value:", class = "heading"),
            textOutput("estimatedVal"),
            
            br()
          )
        )
      )
    ),
  ),
  
  
  #Simplex Implementation Panel
  tabPanel(
    "Simplex Implementation",
    
    sidebarLayout(
      
      sidebarPanel(
        
        tags$head(tags$style(HTML("h3 { margin: 0;} h4 { margin: 0;} #checkboxes{ max-height: 470px; overflow-y: auto;}"))),
        
        tags$h3("Diet Problem Solver"),
        
        helpText("Want to have the balanced diet with minimum cost? This module helps you determine the optimal diet from the food items you would like to include in your diet!"),
        
        #Select all food items
        actionButton("select_all", "Select All"),
        
        #Reset food selection
        actionButton("reset", "Reset"),
        
        #Start Solving
        actionButton("solve_diet", "Start Solving", class = "btn-primary"),
        
        tags$hr(),
        
        tags$h4("Select Food Items", class = "heading"),
        checkboxGroupInput("checkboxes", "Food Items", choices = food_items)
        
      ),
      
      mainPanel(
        
        tabsetPanel(
          type = "tabs",
          id = "Simplex_tabs",
          
          tabPanel(
            "Inputs",
            
            tags$hr(),
            h4("Food Information", class = "heading"),
            br(),
            DTOutput("selected_food_data")
            
          ),
          
          tabPanel(
            "Initial Tableau",
            
            tags$hr(),
            h4("Initial Tableau", class = "heading"),
            
            br(),
            
            DTOutput("initTableau")
          ),
          
          tabPanel(
            "Computations",
            
            tags$hr(),
            h4("Simplex Computation", class = "heading"),
            
            tags$style(type='text/css', '#dietComputation {white-space: pre-wrap;overflow-y:scroll; max-height: 610px; background-color: #FFF; border: 1px solid #3f2d54; padding: 10px;margin: 10px;}'),
            
            verbatimTextOutput("dietComputation")
          ),
          
          tabPanel(
            "Output",
            
            tags$hr(),
            h4("Optimum Diet:", class = "heading"),
            br(),
            DTOutput("optimumDiet")
            
          )
        )
      )
    ),
  ),
  
  
  #About Panel
  tabPanel(
    "About",
    tags$head(tags$style(HTML(".abt-panel { margin: 0 auto; width: 100%; height: 50%;} .links{font-size: 13px; padding: 10px 20px; margin: 20px;}"))),
    fluidRow(column(width = 12, style = "height: 10vh;")),
    fluidRow(
      column(width = 3),
      column(width = 6, 
             wellPanel(
               class = "abt-panel",
               div(
                 style = "display: flex; align-items: center;",
                 tags$img(src = "me.png", width = "25%", style = "margin-right: 10px;"),
                 div(
                   style = "flex-grow: 1;",
                   tags$h1("About the Developer", style = "font-weight: bold; color: #0392cf; "),
                   tags$h3("Rhys Allen V. Abejay", style = "margin: 0px; font-weight: bold; color: #112446;"),
                   tags$hr(style = "margin: 5px"),
                   tags$p("Rhys is a fourth-year BS Statistics student from the University of the Philippines Los Baños, currently taking up CMSC 150: Numerical and Symbolic Computations as an elective course this academic year 2023-2024, and this dashboard is in partial fulfillment of the course requirements.", style = "font-size: 15px; margin: 0px; line-height: 1; text-align: justify;"),
                 )
               ),
               div(
                 style = "flex-grow: 1;",
                 tags$hr(style = "margin: 10px"),
                 tags$h3("Interests", style = "margin: 0px; font-weight: bold; color: #7bc043;"),
                 tags$p(HTML("His recent interests include <b>spatial and spatiotemporal data analytics</b>, and programming as well. He is an avid reader of webtoons/manhwa/manga/manhua and his current favorite works include <i>'Omniscient Reader' </i>, <i>'Debut or Die' </i>, and <i>'My In-Laws are Obsessed with Me'</i>. You can talk to him anything about K-pop or webtoons/manga related."), style = "font-size: 15px; margin: 0px; line-height: 1; text-align: justify;"),
               ),
               div(
                 style = "flex-grow: 1;",
                 tags$hr(style = "margin: 10px"),
                 tags$h3("Skills", style = "margin: 0px; font-weight: bold; color: #7bc043;"),
                 tags$p(HTML("His skills range from <b>Statistical Methods</b>, such as regression and correlation analysis; <b>Data Science and Analytics</b>, such as text mining and analysis; <b>Programming and Tools</b>, including R and Python; and <Miscellaneous Skills>, including proficiency in Microsoft 365, Google Workspace, and Canva."), style = "font-size: 15px; margin: 0px; line-height: 1; text-align: justify;"),
               ),
               div(
                 style = "flex-grow: 1;",
                 tags$hr(style = "margin: 10px"),
                 tags$h3("Get in touch", style = "margin: 0px; font-weight: bold; color: #7bc043;"),
                 tags$p(HTML("If you have any suggestions, questions, or anything to say, you may reach out to him via his emails <b>rvabejay@up.edu.ph</b> or <b>rhysallenabejay@gmail.com</b> or through the links below"), style = "font-size: 15px; margin: 0px; line-height: 1; text-align: justify;"),
                 div(
                   style = "text-align: center;",
                   actionButton("Facebook", "Facebook", class = "links", class = "btn-default", onclick = "shinyjs.openLink('https://www.facebook.com/reesesaleen')"),
                   actionButton("LinkedIn", "LinkedIn", class = "links", class = "btn-default", onclick = "shinyjs.openLink('https://www.linkedin.com/in/rvabejay/')"),
                   actionButton("GitHub", "GitHub", class = "links", class = "btn-default", onclick = "shinyjs.openLink('https://github.com/rvabejay')"),
                 )
               ),
             )
      ),
      column(width = 3)
    ),
    fluidRow(column(width = 12, style = "height: 10vh;"))
    
  )

)
















