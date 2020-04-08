# COVID-19 stats tracker--USA version
# 5 April 2020
# Mikaela Springsteen, contactmspringsteen@gmail.com

# including code adapted from
# https://github.com/ceefluz/radar

# packages

if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinyjs)) install.packages("shinyjs", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(shinycssloaders)) install.packages("shinycssloaders", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(withr)) install.packages("withr", repos = "http://cran.us.r-project.org")
if(!require(rintrojs)) install.packages("rintrojs", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")

# update data to be used
# source("jhu_data.R")

# import data
covid_cases <- read.csv("covid_cases_usa.csv")
covid_cases$Cases <- covid_cases$Totalper100_000
covid_cases$Tests <- covid_cases$atlTestsper100_000


# Shiny ui
ui <- dashboardPage(
  # header
  dashboardHeader(title = "Counting Covid-19: US States", titleWidth = 300,
                  
                  tags$li(a(tags$i("*compare national infection rates here*"), href = "https://mspringsteen.shinyapps.io/counting-covid19/"), class = "dropdown"),
                  dropdownMenu(type = "notifications", 
                    icon = icon("question"), 
                    badgeStatus = NULL,
                    headerText = tags$i("Questions? Suggestions? Want to request", tags$br(), 
                                        "a stat be added to the app? Get in touch at", tags$br(),
                                        "contactmspringsteen@gmail.com")),
                  tags$li(a("ABOUT THIS APP", href = "https://github.com/mikaelaspringsteen/counting-covid19-USA"), class = "dropdown")),
  # sidebar
  dashboardSidebar(
    useShinyjs(),
    introjsUI(),
    width = 300,
    tags$br(),
    h5("Select a single filter or combine", align = "center"),
    h5("several to visualize their impact on" , align = "center"),
    h5("tracking the spread of the virus.", align = "center"),
    tags$hr(),
    introBox(data.step = 3, data.intro = "Click here to update graphs with your selections.",
    fluidRow(
      column(1, offset = 3,
      actionButton("updategraph", tags$b("Update graph"))
      )
    )
    ),
    introBox(data.step = 2, data.intro = "Selecting variables here will highlight any states on the graph which match those characteristics.",
    sidebarMenu(
      introBox(data.step = 1, data.intro = "In most cases, you should include all states. To isolate the course of the virus in specific states, double click on the state name to the right of the graphs. However, for advanced exploration, if you have reason to restrict your exploration to certain states, you may do so here.",
      uiOutput("states")
      ),
      menuItem("Population statistics", tabName = "populationstatistics",
               checkboxInput(
                 inputId = "popcheck", 
                 label = "Population (in hundred thousands)", 
                 value = FALSE
                 ),
               sliderInput(
                 inputId = "popinput",
                 label = NULL,
                 min = floor(min(covid_cases$Population_hundthou, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$Population_hundthou, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$Population_hundthou, na.rm = TRUE)), ceiling(max(covid_cases$Population_hundthou, na.rm = TRUE))),
                 step = 5
                 ),
               checkboxInput(
                 inputId = "popdenscheck", 
                 label = "Population density (per square mile)", 
                 value = FALSE
               ),
               sliderInput(
                 inputId = "popdensinput",
                 label = NULL,
                 min = floor(min(covid_cases$Pop_psqMile, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$Pop_psqMile, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$Pop_psqMile, na.rm = TRUE)), ceiling(max(covid_cases$Pop_psqMile, na.rm = TRUE))),
                 step = 5
               ),
               checkboxInput(
                 inputId = "agecheck", 
                 label = "% of population aged 65+", 
                 value = FALSE
               ),
               sliderInput(
                 inputId = "ageinput",
                 label = NULL,
                 min = floor(min(covid_cases$Age, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$Age, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$Age, na.rm = TRUE)), ceiling(max(covid_cases$Age, na.rm = TRUE))),
                 step = 1
               ),
               checkboxInput(
                 inputId = "hscheck", 
                 label = "% of population with at least a high school education", 
                 value = FALSE
               ),
               sliderInput(
                 inputId = "hsinput",
                 label = NULL,
                 min = floor(min(covid_cases$HSgrad, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$HSgrad, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$HSgrad, na.rm = TRUE)), ceiling(max(covid_cases$HSgrad, na.rm = TRUE))),
                 step = 1
               ),
               checkboxInput(
                 inputId = "bacheck", 
                 label = "% of population with at least an undergraduate education", 
                 value = FALSE
               ),
               sliderInput(
                 inputId = "bainput",
                 label = NULL,
                 min = floor(min(covid_cases$BAgrad, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$BAgrad, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$BAgrad, na.rm = TRUE)), ceiling(max(covid_cases$BAgrad, na.rm = TRUE))),
                 step = 1
               ),
               checkboxInput(
                 inputId = "commutecheck", 
                 label = "Average commute time (in minutes)", 
                 value = FALSE
               ),
               sliderInput(
                 inputId = "commuteinput",
                 label = NULL,
                 min = floor(min(covid_cases$Commute_minutes, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$Commute_minutes, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$Commute_minutes, na.rm = TRUE)), ceiling(max(covid_cases$BAgrad, na.rm = TRUE))),
                 step = 1
               )
      ),
      menuItem("Economic statistics", tabName = "economicstatistics",
               checkboxInput(
                 inputId = "incomecheck", 
                 label = "Income ($ per capita)", 
                 value = FALSE
               ),
               sliderInput(
                 inputId = "incomeinput",
                 label = NULL,
                 min = floor(min(covid_cases$Income_pcap, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$Income_pcap, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$Income_pcap, na.rm = TRUE)), ceiling(max(covid_cases$Income_pcap, na.rm = TRUE))),
                 step = 100
               ),
               checkboxInput(
                 inputId = "povertycheck", 
                 label = "% of population below the poverty line", 
                 value = FALSE
               ),
               sliderInput(
                 inputId = "povertyinput",
                 label = NULL,
                 min = floor(min(covid_cases$Poverty, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$Poverty, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$Poverty, na.rm = TRUE)), ceiling(max(covid_cases$Poverty, na.rm = TRUE))),
                 step = 1
               ),
               checkboxInput(
                 inputId = "homecheck", 
                 label = "% of population who own their own home", 
                 value = FALSE
               ),
               sliderInput(
                 inputId = "homeinput",
                 label = NULL,
                 min = floor(min(covid_cases$OwnsHome, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$OwnsHome, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$OwnsHome, na.rm = TRUE)), ceiling(max(covid_cases$OwnsHome, na.rm = TRUE))),
                 step = 1
               ),
               checkboxInput(
                 inputId = "homepersonscheck", 
                 label = "Number of people living in a single home", 
                 value = FALSE
               ),
               sliderInput(
                 inputId = "homepersonsinput",
                 label = NULL,
                 min = floor(min(covid_cases$PersonsperHouse, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$PersonsperHouse, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$PersonsperHouse, na.rm = TRUE)), ceiling(max(covid_cases$PersonsperHouse, na.rm = TRUE))),
                 step = 1
               ),
               checkboxInput(
                 inputId = "rentcheck", 
                 label = "Rent (median gross $)", 
                 value = FALSE
               ),
               sliderInput(
                 inputId = "rentinput",
                 label = NULL,
                 min = floor(min(covid_cases$Rent_medgross, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$Rent_medgross, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$Rent_medgross, na.rm = TRUE)), ceiling(max(covid_cases$Rent_medgross, na.rm = TRUE))),
                 step = 50
               )
      ),
      menuItem("Health statistics", tabName = "healthstatistics",
               checkboxInput(
                 inputId = "insurancecheck", 
                 label = "% of under 65 population with no health insurance", 
                 value = FALSE
               ),
               sliderInput(
                 inputId = "insuranceinput",
                 label = NULL,
                 min = floor(min(covid_cases$NoHealthInsurance_under65, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$NoHealthInsurance_under65, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$NoHealthInsurance_under65, na.rm = TRUE)), ceiling(max(covid_cases$NoHealthInsurance_under65, na.rm = TRUE))),
                 step = 1
               )
      )
    )
    )
  ),
  # body
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    introBox(data.step = 4, data.intro = "Switch between tabs to see different Covid-19 metrics. A description of the graph is located below each panel.",
    tabsetPanel(
      tabPanel("Tests",
               introBox(data.step = 5, data.intro = "Each graph is interactive. Hover over points/lines for more information, or find more settings (including a home button to reset axes) at the top right of each graph.",
               fluidRow(column(12, uiOutput("tests_graph")))
               ),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               fluidRow(column(12, helpText("The grey line represents the conditional mean for all states, and the blue represents the mean for the highlighted states To interpret the number of cases and tests, raise 10 to the power of number displayed on the line (10 ^ that number).", tags$br(), "As more people are tested, the number of confimed cases increases. States who test a lot of people have more accurate rates of confirmed infection and case fatality.", tags$br(), "States with low testing and high case rates (toward the top left of the graph) likely have a large number of undetected cases in their population. States with high testing and low case rates (toward the bottom right of the graph) may be successfully containing the virus or, alternatively, may not be finding all of their positive cases.")))
      ),
      tabPanel("Cases",
               fluidRow(column(12, uiOutput("cases_graph"))),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               fluidRow(column(12, helpText("The grey dotted line represents the conditional mean for all states, and the blue dotted line represents the mean for the highlighted states To interpret the number of cases, raise 10 to the power of number displayed on the line (10 ^ that number). The light band around these lines represents the standard error.", tags$br(),"This number has been scaled to represent the number of confirmed cases for every 100,000 people in each state, in order to make comparison betweeen states easier.", tags$br(), "If a state is not testing a lot of people, this number is probably lower than that state's actual infection rate, as a large number of mild cases may go undetected.")))
      ),
      tabPanel("Deaths",
               fluidRow(column(12, uiOutput("case_fatality_graph"))),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               fluidRow(column(12, helpText("The grey dotted line represents the conditional mean for all states, and the blue dotted line represents the mean for the highlighted states To interpret the death rate, raise 10 to the power of number displayed on the line (10 ^ that number). The light band around these lines represents the standard error.", tags$br(),"Also known as the 'case fatality rate', this number is calculated by dividing the number of detected cases by the number of reported deaths.", tags$br(), "If a state is not testing a lot of people this number may be artifically high, as a large number of mild cases could go undetected. Confirmed cases may then represent the most severe cases—which are more likely to result in a death. If a state is not accurately recording Covid-19 deaths the case fatality rate may be artificially low for that state")))
      )
    )
    )
  )
)

# Shiny server
server <- function(input, output, session) {
  # intro message
  observeEvent("", {
    showModal(modalDialog(
      easyClose = TRUE,
      title = tags$b("Counting Covid-19"),
      tags$b("What we know about the infection or death rate of Covid-19 depends on one thing:"),
      tags$br(),
      tags$b("how good are states at counting the people who have Covid-19?"),
      tags$br(),
      tags$br(),
      "The number of people tested and confirmed to have the virus (usually described as 'cases' of the virus), is lower than the total number of people who have the virus (called 'infections') because not everyone who has the virus will be tested. Some people, especially those with mild symptoms or those unable to access the healthcare system, will not be tested. This difference between the number of 'cases' and the number of 'infections' can vary from region to region, and will impact that region's apparent number of cases and their apparent mortality rate. This is why there is such a range of rates across the country.", "For more, see ", tags$a(href = "https://www.npr.org/sections/goatsandsoda/2020/03/27/821958435/why-death-rates-from-coronavirus-can-be-deceiving", "Why 'Death Rates' From Coronavirus Can Be Deceiving"), "or ", tags$a(href = "https://www.bbc.com/future/article/20200401-coronavirus-why-death-and-mortality-rates-differ", "Coronavirus: Why death and mortality rates differ"), ".",
      tags$br(),
      tags$br(),
      "Testing more people will result in better, more accurate data about Covid-19's infection and mortality rate, but what is it that makes certain states better at testing than others? Is it money? Something about the population? Their system of healthcare?",
      tags$br(),
      tags$br(),
      tags$b("Exploring what characteristics are associated with increased testing, lower case rates, or lower case fatality rates might help explain what makes some states better at counting cases of Covid-19 than others."),
      tags$br(),
      tags$hr(),
      tags$b(tags$i("Please note: this app is based on a large dataset, and the graphs may take some time to load.")),
      tags$br(),
      tags$hr(),
      tags$i("For information about combating the spread of the virus, or about symptoms and treatment, there are a number of excellent resources run by infectious disease experts and medical professionals, including the ", tags$a(href = "https://www.who.int/emergencies/diseases/novel-coronavirus-2019", "WHO"), "and ", tags$a(href = "https://www.cdc.gov/coronavirus/2019-nCoV/index.html", "CDC"), "for public health information, the ", tags$a(href = "https://www.nih.gov/health-information/coronavirus", "NIH"), "and ", tags$a(href = "https://www.gisaid.org/", "GISAID"), "for research information, and ", tags$a(href = "https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "JHU"), "for data."),
      tags$br(),
      tags$br(),
      footer = tagList(
        actionButton(inputId = "intro", label = tags$b("See how it works")))
    ))
  })
  # start intro tour
  observeEvent(input$intro,{
    removeModal()
  })
  observeEvent(input$intro,
               introjs(session, options = list("nextLabel" = "Continue",
                                               "prevLabel" = "Previous",
                                               "doneLabel" = "Let's go!",
                                               "showStepNumbers" = "false"))
  )
  # general settings
  options(list(scipen = 99))
  # state selection
  output$states <- renderUI({
    stateslist <- unique(as.character(covid_cases$State))
    pickerInput(
      inputId = "statesinput", label = h5("Select states to include in plot"), 
      choices = stateslist, 
      selected = stateslist,
      multiple = TRUE, 
      options = list(`actions-box` = TRUE)
    )
  })
  # create minimal dataset
  min_covid_case <- reactive({
    select(covid_cases, State, Date, Day, Tests, Cases, DeathRate) %>%
      filter(State %in% input$statesinput)
  })
  # enable inputs if variable is checked
  observeEvent(input$popcheck, {
    if (input$popcheck == FALSE) {
      disable("popinput")
      updateSliderInput(
        session,
        inputId = "popinput",
        label = NULL,
        min = floor(min(covid_cases$Population_hundthou, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Population_hundthou, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Population_hundthou, na.rm = TRUE)), ceiling(max(covid_cases$Population_hundthou, na.rm = TRUE))),
        step = 5
      )
    } else {
      enable("popinput")
      updateSliderInput(
        session,
        inputId = "popinput",
        label = NULL,
        min = floor(min(covid_cases$Population_hundthou, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Population_hundthou, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Population_hundthou, na.rm = TRUE)), ceiling(max(covid_cases$Population_hundthou, na.rm = TRUE))),
        step = 5
      )
    }
  })
  observeEvent(input$agecheck, {
    if (input$agecheck == FALSE) {
      disable("ageinput")
      updateSliderInput(
        session,
        inputId = "ageinput",
        label = NULL,
        min = floor(min(covid_cases$Age, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Age, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Age, na.rm = TRUE)), ceiling(max(covid_cases$Age, na.rm = TRUE))),
        step = 1
      )
    } else {
      enable("ageinput")
      updateSliderInput(
        session,
        inputId = "ageinput",
        label = NULL,
        min = floor(min(covid_cases$Age, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Age, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Age, na.rm = TRUE)), ceiling(max(covid_cases$Age, na.rm = TRUE))),
        step = 1
      )
    }
  })
  observeEvent(input$hscheck, {
    if (input$hscheck == FALSE) {
      disable("hsinput")
      updateSliderInput(
        session,
        inputId = "hsinput",
        label = NULL,
        min = floor(min(covid_cases$HSgrad, na.rm = TRUE)),
        max = ceiling(max(covid_cases$HSgrad, na.rm = TRUE)),
        value = c(floor(min(covid_cases$HSgrad, na.rm = TRUE)), ceiling(max(covid_cases$HSgrad, na.rm = TRUE))),
        step = 1
      )
    } else {
      enable("hsinput")
      updateSliderInput(
        session,
        inputId = "hsinput",
        label = NULL,
        min = floor(min(covid_cases$HSgrad, na.rm = TRUE)),
        max = ceiling(max(covid_cases$HSgrad, na.rm = TRUE)),
        value = c(floor(min(covid_cases$HSgrad, na.rm = TRUE)), ceiling(max(covid_cases$HSgrad, na.rm = TRUE))),
        step = 1
      )
    }
  })
  observeEvent(input$bacheck, {
    if (input$bacheck == FALSE) {
      disable("bainput")
      updateSliderInput(
        session,
        inputId = "bainput",
        label = NULL,
        min = floor(min(covid_cases$BAgrad, na.rm = TRUE)),
        max = ceiling(max(covid_cases$BAgrad, na.rm = TRUE)),
        value = c(floor(min(covid_cases$BAgrad, na.rm = TRUE)), ceiling(max(covid_cases$BAgrad, na.rm = TRUE))),
        step = 1
      )
    } else {
      enable("bainput")
      updateSliderInput(
        session,
        inputId = "bainput",
        label = NULL,
        min = floor(min(covid_cases$BAgrad, na.rm = TRUE)),
        max = ceiling(max(covid_cases$BAgrad, na.rm = TRUE)),
        value = c(floor(min(covid_cases$BAgrad, na.rm = TRUE)), ceiling(max(covid_cases$BAgrad, na.rm = TRUE))),
        step = 1
      )
    }
  })
  observeEvent(input$homecheck, {
    if (input$homecheck == FALSE) {
      disable("homeinput")
      updateSliderInput(
        session,
        inputId = "homeinput",
        label = NULL,
        min = floor(min(covid_cases$OwnsHome, na.rm = TRUE)),
        max = ceiling(max(covid_cases$OwnsHome, na.rm = TRUE)),
        value = c(floor(min(covid_cases$OwnsHome, na.rm = TRUE)), ceiling(max(covid_cases$OwnsHome, na.rm = TRUE))),
        step = 1
      )
    } else {
      enable("homeinput")
      updateSliderInput(
        session,
        inputId = "homeinput",
        label = NULL,
        min = floor(min(covid_cases$OwnsHome, na.rm = TRUE)),
        max = ceiling(max(covid_cases$OwnsHome, na.rm = TRUE)),
        value = c(floor(min(covid_cases$OwnsHome, na.rm = TRUE)), ceiling(max(covid_cases$OwnsHome, na.rm = TRUE))),
        step = 1
      )
    }
  })
  observeEvent(input$homepersonscheck, {
    if (input$homepersonscheck == FALSE) {
      disable("homepersonsinput")
      updateSliderInput(
        session,
        inputId = "homepersonsinput",
        label = NULL,
        min = floor(min(covid_cases$PersonsperHouse, na.rm = TRUE)),
        max = ceiling(max(covid_cases$PersonsperHouse, na.rm = TRUE)),
        value = c(floor(min(covid_cases$PersonsperHouse, na.rm = TRUE)), ceiling(max(covid_cases$PersonsperHouse, na.rm = TRUE))),
        step = 1
      )
    } else {
      enable("homepersonsinput")
      updateSliderInput(
        session,
        inputId = "homepersonsinput",
        label = NULL,
        min = floor(min(covid_cases$PersonsperHouse, na.rm = TRUE)),
        max = ceiling(max(covid_cases$PersonsperHouse, na.rm = TRUE)),
        value = c(floor(min(covid_cases$PersonsperHouse, na.rm = TRUE)), ceiling(max(covid_cases$PersonsperHouse, na.rm = TRUE))),
        step = 1
      )
    }
  })
  observeEvent(input$rentcheck, {
    if (input$rentcheck == FALSE) {
      disable("rentinput")
      updateSliderInput(
        session,
        inputId = "rentinput",
        label = NULL,
        min = floor(min(covid_cases$Rent_medgross, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Rent_medgross, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Rent_medgross, na.rm = TRUE)), ceiling(max(covid_cases$Rent_medgross, na.rm = TRUE))),
        step = 50
      )
    } else {
      enable("rentinput")
      updateSliderInput(
        session,
        inputId = "rentinput",
        label = NULL,
        min = floor(min(covid_cases$Rent_medgross, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Rent_medgross, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Rent_medgross, na.rm = TRUE)), ceiling(max(covid_cases$Rent_medgross, na.rm = TRUE))),
        step = 50
      )
    }
  })
  observeEvent(input$insurancecheck, {
    if (input$insurancecheck == FALSE) {
      disable("insuranceinput")
      updateSliderInput(
        session,
        inputId = "insuranceinput",
        label = NULL,
        min = floor(min(covid_cases$NoHealthInsurance_under65, na.rm = TRUE)),
        max = ceiling(max(covid_cases$NoHealthInsurance_under65, na.rm = TRUE)),
        value = c(floor(min(covid_cases$NoHealthInsurance_under65, na.rm = TRUE)), ceiling(max(covid_cases$NoHealthInsurance_under65, na.rm = TRUE))),
        step = 1
      )
    } else {
      enable("insuranceinput")
      updateSliderInput(
        session,
        inputId = "insuranceinput",
        label = NULL,
        min = floor(min(covid_cases$NoHealthInsurance_under65, na.rm = TRUE)),
        max = ceiling(max(covid_cases$NoHealthInsurance_under65, na.rm = TRUE)),
        value = c(floor(min(covid_cases$NoHealthInsurance_under65, na.rm = TRUE)), ceiling(max(covid_cases$NoHealthInsurance_under65, na.rm = TRUE))),
        step = 1
      )
    }
  })
  observeEvent(input$commutecheck, {
    if (input$commutecheck == FALSE) {
      disable("commuteinput")
      updateSliderInput(
        session,
        inputId = "commuteinput",
        label = NULL,
        min = floor(min(covid_cases$Commute_minutes, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Commute_minutes, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Commute_minutes, na.rm = TRUE)), ceiling(max(covid_cases$BAgrad, na.rm = TRUE))),
        step = 1
      )
    } else {
      enable("commuteinput")
      updateSliderInput(
        session,
        inputId = "commuteinput",
        label = NULL,
        min = floor(min(covid_cases$Commute_minutes, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Commute_minutes, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Commute_minutes, na.rm = TRUE)), ceiling(max(covid_cases$BAgrad, na.rm = TRUE))),
        step = 1
      )
    }
  })
  observeEvent(input$incomecheck, {
    if (input$incomecheck == FALSE) {
      disable("incomeinput")
      updateSliderInput(
        session,
        inputId = "incomeinput",
        label = NULL,
        min = floor(min(covid_cases$Income_pcap, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Income_pcap, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Income_pcap, na.rm = TRUE)), ceiling(max(covid_cases$Income_pcap, na.rm = TRUE))),
        step = 100
      )
    } else {
      enable("incomeinput")
      updateSliderInput(
        session,
        inputId = "incomeinput",
        label = NULL,
        min = floor(min(covid_cases$Income_pcap, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Income_pcap, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Income_pcap, na.rm = TRUE)), ceiling(max(covid_cases$Income_pcap, na.rm = TRUE))),
        step = 100
      )
    }
  })
  observeEvent(input$povertycheck, {
    if (input$povertycheck == FALSE) {
      disable("povertyinput")
      updateSliderInput(
        session,
        inputId = "povertyinput",
        label = NULL,
        min = floor(min(covid_cases$Poverty, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Poverty, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Poverty, na.rm = TRUE)), ceiling(max(covid_cases$Poverty, na.rm = TRUE))),
        step = 1
      )
    } else {
      enable("povertyinput")
      updateSliderInput(
        session,
        inputId = "povertyinput",
        label = NULL,
        min = floor(min(covid_cases$Poverty, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Poverty, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Poverty, na.rm = TRUE)), ceiling(max(covid_cases$Poverty, na.rm = TRUE))),
        step = 1
      )
    }
  })
  observeEvent(input$popdenscheck, {
    if (input$popdenscheck == FALSE) {
      disable("popdensinput")
      updateSliderInput(
        session,
        inputId = "popdensinput",
        label = NULL,
        min = floor(min(covid_cases$Pop_psqMile, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Pop_psqMile, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Pop_psqMile, na.rm = TRUE)), ceiling(max(covid_cases$Pop_psqMile, na.rm = TRUE))),
        step = 5
      )
    } else {
      enable("popdensinput")
      updateSliderInput(
        session,
        inputId = "popdensinput",
        label = NULL,
        min = floor(min(covid_cases$Pop_psqMile, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Pop_psqMile, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Pop_psqMile, na.rm = TRUE)), ceiling(max(covid_cases$Pop_psqMile, na.rm = TRUE))),
        step = 5
      )
    }
  })
   # create selected dataset
  selected_covid_case <- reactive({
    popfilter <- quote(between(Population_hundthou, as.numeric(input$popinput[1]), as.numeric(input$popinput[2])))
    agefilter <- quote(between(Age, as.numeric(input$ageinput[1]), as.numeric(input$ageinput[2])))
    hsfilter <- quote(between(HSgrad, as.numeric(input$hsinput[1]), as.numeric(input$hsinput[2])))
    bafilter <- quote(between(BAgrad, as.numeric(input$bainput[1]), as.numeric(input$bainput[2])))
    homefilter <- quote(between(OwnsHome, as.numeric(input$homeinput[1]), as.numeric(input$homeinput[2])))
    homepersonsfilter <- quote(between(PersonsperHouse, as.numeric(input$homepersonsinput[1]), as.numeric(input$homepersonsinput[2])))
    rentfilter <- quote(between(Rent_medgross, as.numeric(input$rentinput[1]), as.numeric(input$rentinput[2])))
    insurancefilter <- quote(between(NoHealthInsurance_under65, as.numeric(input$insuranceinput[1]), as.numeric(input$insuranceinput[2])))
    commutefilter <- quote(between(Commute_minutes, as.numeric(input$commuteinput[1]), as.numeric(input$commuteinput[2])))
    incomefilter <- quote(between(Income_pcap, as.numeric(input$incomeinput[1]), as.numeric(input$incomeinput[2])))
    povertyfilter <- quote(between(Poverty, as.numeric(input$povertyinput[1]), as.numeric(input$povertyinput[2])))
    popdensfilter <- quote(between(Pop_psqMile, as.numeric(input$popdensinput[1]), as.numeric(input$popdensinput[2])))
    covid_cases %>%
      select(
        State, Date, Day, Tests, Cases, DeathRate,
        if (input$popcheck == FALSE) {"State"} else {"Population_hundthou"},
        if (input$agecheck == FALSE) {"State"} else {"Age"},
        if (input$hscheck == FALSE) {"State"} else {"HSgrad"},
        if (input$bacheck == FALSE) {"State"} else {"BAgrad"},
        if (input$homecheck == FALSE) {"State"} else {"OwnsHome"},
        if (input$homepersonscheck == FALSE) {"State"} else {"PersonsperHouse"},
        if (input$rentcheck == FALSE) {"State"} else {"Rent_medgross"},
        if (input$insurancecheck == FALSE) {"State"} else {"NoHealthInsurance_under65"},
        if (input$commutecheck == FALSE) {"State"} else {"Commute_minutes"},
        if (input$incomecheck == FALSE) {"State"} else {"Income_pcap"},
        if (input$povertycheck == FALSE) {"State"} else {"Poverty"},
        if (input$popdenscheck == FALSE) {"State"} else {"Pop_psqMile"}
      ) %>%
      filter(
        State %in% input$statesinput,
        if (input$popcheck == FALSE) {!is.na(State)} else {!!popfilter},
        if (input$agecheck == FALSE) {!is.na(State)} else {!!agefilter},
        if (input$hscheck == FALSE) {!is.na(State)} else {!!hsfilter},
        if (input$bacheck == FALSE) {!is.na(State)} else {!!bafilter},
        if (input$homecheck == FALSE) {!is.na(State)} else {!!homefilter},
        if (input$homepersonscheck == FALSE) {!is.na(State)} else {!!homepersonsfilter},
        if (input$rentcheck == FALSE) {!is.na(State)} else {!!rentfilter},
        if (input$insurancecheck == FALSE) {!is.na(State)} else {!!insurancefilter},
        if (input$commutecheck == FALSE) {!is.na(State)} else {!!commutefilter},
        if (input$incomecheck == FALSE) {!is.na(State)} else {!!incomefilter},
        if (input$povertycheck == FALSE) {!is.na(State)} else {!!povertyfilter},
        if (input$popdenscheck == FALSE) {!is.na(State)} else {!!popdensfilter}
      )
  })
  # tests graph
  tests_plot <- reactive({
    validate(
      need(input$statesinput != "", "Please select at least 1 state from the dropdown to the left."))
    validate(
      need(try(select(selected_covid_case(), State) != ""), "There are no states matching the selected criteria.\nPlease select fewer variables, adjust the range of those already selected, or add additional states from the dropdown to the left."))
    validate(
      need(try(select(selected_covid_case(), Tests) != ""), "That state has no testing data available."))
    plot <- 
      with_options(list(digits = 1),
      ggplotly(
        ggplot(selected_covid_case()) +
          geom_line(data = min_covid_case(), aes(x = Tests, y = Cases, group = State), color = "#bdc3c7", show.legend = FALSE) +
          geom_line(aes(x = Tests, y = Cases, color = State), show.legend = FALSE) +
          geom_smooth(aes(x = Tests, y = Cases), data = min_covid_case(),
                      method = "loess", se = FALSE, color = "#bdc3c7", size = .5, alpha = .6, linetype = "dotted") +
          geom_ribbon(aes(x = Tests, y = Cases), data = min_covid_case(),
                      stat = "smooth", method = "loess", alpha = .15) +
          geom_smooth(aes(x = Tests, y = Cases),
                      method = "loess", se = FALSE, color = "#3c8dbc", size = .5, alpha = .6, linetype = "dotted") +
          geom_ribbon(aes(x = Tests, y = Cases),
                      stat = "smooth", method = "loess", alpha = .15) +
          scale_x_log10(expand = c(0, 0)) +
          scale_y_log10(expand = c(0, 0)) +
          labs(
            title = "How is the rate of testing related to the confirmed rate of infection?",
            x = "Tests performed per 100,000 people", y = "Confirmed Covid-19 cases per 100,000 people") +
          theme(text = element_text(family = "Georgia"),
                panel.background = element_rect(fill = "#f7f5f0", colour = "#f7f5f0"),
                plot.title = element_text(face = "italic"),
                plot.subtitle = element_text(face = "italic"),
                axis.title = element_text(face = "italic"),
                plot.caption = element_text(face = "italic"),
                panel.grid.major = element_line(colour = "#D5D3CC", size = rel(.5)), 
                panel.grid.major.x = element_blank(),
                panel.grid.minor = element_blank(), 
                axis.ticks = element_blank(),
                axis.text.x = NULL,
                axis.line.x = element_line(colour = "#908f85"),
                plot.margin = unit(c(2, 1, 2, 1), "lines")),
        height = 600
      )
      )
  })
  output$tests_plot <- renderPlotly({
    input$updategraph
    isolate({
      tests_plot()
    })
  })
  output$tests_graph <- renderUI({
    withSpinner(
      plotlyOutput("tests_plot"),
      type = 1,
      color = "#3c8dbc"
    )
  })
  # cases graph
  cases_plot <- reactive({
    validate(
      need(input$statesinput != "", "Please select at least 1 state from the dropdown to the left."))
    validate(
      need(try(select(selected_covid_case(), State) != ""), "There are no states matching the selected criteria.\nPlease select fewer variables, adjust the range of those already selected, or add additional states from the dropdown to the left."))
    plot <- 
      with_options(list(digits = 1),
      ggplotly(
      ggplot(selected_covid_case()) +
      geom_line(data = min_covid_case(), aes(x = Day, y = Cases, group = State), color = "#bdc3c7", show.legend = FALSE) +
      geom_line(aes(x = Day, y = Cases, color = State, group = State), show.legend = FALSE) +
      geom_smooth(aes(x = Day, y = Cases), data = min_covid_case(),
        method = "loess", se = FALSE, color = "#bdc3c7", size = .5, alpha = .6, linetype = "dotted") +
      geom_ribbon(aes(x = Day, y = Cases), data = min_covid_case(),
        stat = "smooth", method = "loess", alpha = .15) +
      geom_smooth(aes(x = Day, y = Cases),
        method = "loess", se = FALSE, color = "#3c8dbc", size = .5, alpha = .6, linetype = "dotted") +
      geom_ribbon(aes(x = Day, y = Cases),
        stat = "smooth", method = "loess", alpha = .15) +
      labs(
        title = "How many people, that we know of, have Covid-19?",
        x = "Days from 50th in-state case", y = "Detected cases per 100,000 people") +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_log10(expand = c(0, 0)) +
      theme(text = element_text(family = "Georgia"),
        panel.background = element_rect(fill = "#f7f5f0", colour = "#f7f5f0"),
        plot.title = element_text(face = "italic"),
        plot.subtitle = element_text(face = "italic"),
        axis.title = element_text(face = "italic"),
        plot.caption = element_text(face = "italic"),
        panel.grid.major = element_line(colour = "#D5D3CC", size = rel(.5)), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = NULL,
        axis.line.x = element_line(colour = "#908f85"),
        plot.margin = unit(c(2, 1, 2, 1), "lines")),
      height = 600,
      tooltip = c("text", "x", "y", "group")
      )
      )
  })
  output$cases_plot <- renderPlotly({
    input$updategraph
    isolate({
      cases_plot()
    })
  })
  output$cases_graph <- renderUI({
      withSpinner(
        plotlyOutput("cases_plot"),
        type = 1,
        color = "#3c8dbc"
      )
  })
  # cfr graph
  case_fatality_plot <- reactive({
    validate(
      need(input$statesinput != "", "Please select at least 1 state from the dropdown to the left."))
    validate(
      need(try(select(selected_covid_case(), State) != ""), "There are no states matching the selected criteria.\nPlease select fewer variables, adjust the range of those already selected, or add additional states from the dropdown to the left."))
    plot <- 
      with_options(list(digits = 1),
      ggplotly(
      ggplot(selected_covid_case()) +
      geom_line(data = min_covid_case(), aes(x = Day, y = DeathRate, group = State), color = "#bdc3c7", show.legend = FALSE) +
      geom_line(aes(x = Day, y = DeathRate, color = State), show.legend = FALSE) +
      geom_smooth(aes(x = Day, y = DeathRate), data = min_covid_case(),
                    method = "loess", se = FALSE, color = "#bdc3c7", size = .5, alpha = .6, linetype = "dotted") +
      geom_ribbon(aes(x = Day, y = DeathRate), data = min_covid_case(),
                    stat = "smooth", method = "loess", alpha = .15) +
      geom_smooth(aes(x = Day, y = DeathRate),
                    method = "loess", se = FALSE, color = "#3c8dbc", size = .5, alpha = .6, linetype = "dotted") +
      geom_ribbon(aes(x = Day, y = DeathRate),
                    stat = "smooth", method = "loess", alpha = .15) +
      labs(
        title = list(text = paste0("Of the people that we know have Covid-19, what percent have died?", "<br>", "<sup>",
                                   "","<sup>")),
        x = "Days from 50th in-state case", y = "Percent of detected cases resulting in a death") +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0), breaks = c(.001, .01, .05, .1, .15), labels = scales::percent) +
      theme(text = element_text(family = "Georgia"),
            panel.background = element_rect(fill = "#f7f5f0", colour = "#f7f5f0"),
            plot.title = element_text(face = "italic"),
            plot.subtitle = element_text(face = "italic"),
            axis.title = element_text(face = "italic"),
            plot.caption = element_text(face = "italic"),
            panel.grid.major = element_line(colour = "#D5D3CC", size = rel(.5)), 
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.ticks = element_blank(),
            axis.text.x = NULL,
            axis.line.x = element_line(colour = "#908f85"),
            plot.margin = unit(c(2, 1, 2, 1), "lines")),
      height = 600
      )
      )
  })
  output$case_fatality_plot <- renderPlotly({
    input$updategraph
    isolate({
      case_fatality_plot()
    })
  })
  output$case_fatality_graph <- renderUI({
    withSpinner(
      plotlyOutput("case_fatality_plot"),
      type = 1,
      color = "#3c8dbc"
    )
  })
  
}

# Shiny app
shinyApp(ui, server)