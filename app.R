#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

# Loading Packages here ----
library(shiny)
library(forecast)
library(leaflet)
library(spdep)
library(RCurl)
library(viridis)
library(jsonlite)
library(pryr)
library(httr)
library(sf)
library(rgeos)
library(tidyr)
library(ggmap)
library(forcats)
library(stringr)
library(RSocrata)
library(ggplot2)
library(magrittr)
library(plyr)
library(RColorBrewer)
library(cartogram)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(shinyjs)
library(dplyr)

wd=getwd()

# Getting NY State POPULATION data and GEOMETRY ----

ny_path=paste0(wd, "/NYS_Civil_Boundaries_SHP/Counties_Shoreline.shp")

nys_pop<- st_read(ny_path)
nys_pop$NAME %<>% paste()
nys_pop$NAME<-str_replace(nys_pop$NAME, "St Lawrence", "St. Lawrence")

# Getting NY State Health Dept COVID data ----

df <- read.socrata(
    "https://health.data.ny.gov/resource/xdss-u53e.json",
    app_token = "CzNn3zCXW3imUL9FuGgmJy5rV",
    email     = "anni3.ly@gmail.com",
    password  = "Rp!676!MsRHNFMy"
)

state_data<- fromJSON("https://covidtracking.com/api/states?state=ny", simplifyDataFrame=TRUE)

lastUpdate=state_data[["lastUpdateEt"]]

# I created this function because, at one point, there were no updates for "inIcuCurrently"
na_value <- function (x) {
    if (class(x)=="NULL") {
        x=paste0("unavailable")
        return(x) 
    } else 
        return(x)
}

state_data[["inIcuCurrently"]]<-na_value(state_data[["inIcuCurrently"]])

summary<- read.csv(text=getURL("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/summary.csv"))

# Joining and formatting final NY state data ---- 

data<- left_join(nys_pop, df, by=c("NAME"="county"))

data$test_date %<>% as.Date()

data$new_positives %<>% as.numeric()
data$cumulative_number_of_positives %<>% as.numeric()
data$total_number_of_tests %<>% as.numeric()
data$cumulative_number_of_tests %<>% as.numeric()

data <- st_simplify(data, preserveTopology = TRUE, dTolerance = 1000)

#Need to log10 transform data to correct overall right skewness 
data$log10_pop2010<- log10(data$POP2010)
data$log10_cumulative_positives<- log10(data$cumulative_number_of_positives)

#Converting NY state sf data to sp
sp_nydata <- as(data, Class = "Spatial")

# Make neighbor list from NY state sp data format
ny_nb <- poly2nb(sp_nydata)

# Get center points of each borough
ny_centers <- coordinates(sp_nydata)

# Getting NYC zip code GEOMETRY ----

path=paste0(wd,"/ZIP_CODE_040114/ZIP_CODE_040114.shp")

nyc.bound<-st_read(path)

nyc.bound <- st_simplify(nyc.bound, preserveTopology = TRUE, dTolerance = 100)
nyc.bound$ZIPCODE<- as.numeric(paste0(nyc.bound$ZIPCODE))

# Getting NYC zip code POPULATION and COVID data ----

nyczip_covid <-read.csv(text=getURL("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/tests-by-zcta.csv"))

nyczip_census <- read.csv("Demographic_Statistics_By_Zip_Code.csv")

# Putting together NYC zip code data ----

nyczip_dat <- left_join(nyc.bound, nyczip_census, by=c("ZIPCODE"="JURISDICTION.NAME"))
nyczip_dat <- left_join(nyczip_dat, nyczip_covid, by=c("ZIPCODE"="MODZCTA"))

nyczip_na <- nyczip_dat[!is.na(nyczip_dat$Positive),]

# Getting NYC borough GEOMETRY ----
b_path=paste0(wd,"/borough_boundaries/geoshape.shp")

nyc.borough<-st_read(b_path)

# Getting NYC borough COVID data from Github repo ----

borough_covid <-read.csv(text=getURL("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/boro.csv"))

colnames(borough_covid)[1]<- "boro_name"
borough_covid <- borough_covid[!(borough_covid$boro_name=="Citywide"),]
borough_covid$boro_name <- as.character(paste0(borough_covid$boro_name))
borough_covid$boro_name[1] <- "Bronx"

# Getting NYC borough POPULATION data ----
library(readxl)
borough_census <- read_excel("borough_census_reformat.xlsx")

# Putting NYC borough data all together ----

borough_dat <- left_join(nyc.borough, borough_covid, by="boro_name")
borough_dat <- left_join(borough_dat, borough_census, by="boro_name")

# For calculated borough SMR ----

r <- sum(borough_dat$COVID_CASE_COUNT) / sum(borough_dat$"Total population") # Compute and print the overall incidence 

borough_dat$covid_EXP <- borough_dat$"Total population" * r # Calculate the expected number for each borough

borough_dat$covid_SMR <- borough_dat$COVID_CASE_COUNT / borough_dat$covid_EXP # Calculate the ratio of Observed to Expected

# Functions and data exploration ----

data_summary <- function(data, varname, groupnames){
    summary_func <- function(x, col){
        c(mean = mean(x[[col]], na.rm=TRUE),
          sd = sd(x[[col]], na.rm=TRUE),
          se = sd(x[[col]], na.rm=TRUE)/sqrt(length(x[[col]])))
    }
    data_sum<-ddply(data, groupnames, .fun=summary_func,
                    varname)
    data_sum <- plyr::rename(data_sum, c(varname="mean"))
    return(data_sum)
}

autoplot_exist<- function(x) {
    y <- ts(x[,-1], start = c(2020, 1), frequency = 365)
    existing_data<- forecast::autoplot(y, facet = F)
    
    return(existing_data)
}

covid_forecast<- function(x) {
    
    vector<- x[nrow(x[3])-1,3] %>% as.numeric()
    diff<-max(x[3]) - vector
    
    y <- ts(x[,-1], start = c(2020, 1), frequency = 365)
    
    fit <- forecast::auto.arima(y[, "cumulative_number_of_positives"], xreg = y[, "cumulative_number_of_tests"], stationary = FALSE)
    
    fc <- forecast(fit, xreg = seq(max(x[3]),max(x[3])+(diff*14), by=diff))
    myplots <- autoplot(fc)
    
    return(myplots)
    
}


residual_check<- function(x) {
    
    y <- ts(x[,-1], start = c(2020, 1), frequency = 365)
    
    fit <- forecast::auto.arima(y[, "cumulative_number_of_positives"], xreg = y[, "cumulative_number_of_tests"], stationary = FALSE)
    checkresiduals(fit)
    
}

# Probability of a binomial exceeding a multiple
binom.exceed <- function(observed, population, expected, e){
    1 - pbinom(e * expected, population, prob = observed / population)
}

# For borough SMR alert ----

# Compute P(rate > 2)
borough_dat$covid_gt_2 <- binom.exceed(
    observed = borough_dat$COVID_CASE_COUNT,
    population = borough_dat$"Total population",
    expected = borough_dat$covid_EXP,
    e = 2)

# Use a 50-color palette that only starts changing at around 0.9
pal <- c(
    rep("#B0D0B0", 40),
    colorRampPalette(c("#B0D0B0", "orange"))(5), 
    colorRampPalette(c("orange", "red"))(5)
)

nycb_sp <- as(borough_dat, Class = "Spatial")

# Calculating SMR for NYC zip code ----
r <- sum(nyczip_na$Positive) / sum(nyczip_na$POPULATION) # Compute and print the overall incidence 

nyczip_na$covid_EXP <- nyczip_na$POPULATION * r # Calculate the expected number for each borough

nyczip_na$covid_SMR <- nyczip_na$Positive / nyczip_na$covid_EXP # Calculate the ratio of Observed to Expected

# Compute P(rate > 2)
nyczip_na$covid_gt_2 <- binom.exceed(
    observed = nyczip_na$Positive,
    population = nyczip_na$POPULATION,
    expected = nyczip_na$covid_EXP,
    e = 2)

nyczip_sp <- as(nyczip_na, Class = "Spatial")

# For forecasting ----
fc_data<- subset(data, NYC == "Y", select = c("test_date", "cumulative_number_of_positives", "cumulative_number_of_tests", "NAME"))
fc_data<- st_set_geometry(fc_data, NULL)
fc_data<-fc_data[order(fc_data$test_date),]

fc_data2<- fc_data %>% group_by(NAME) %>% nest()

#Positive trend plots
existing_plots<-lapply(fc_data2$data, autoplot_exist) 
names(existing_plots)=fc_data2$NAME

#Forecast plots
fc_plots<- lapply(fc_data2$data, covid_forecast)
names(fc_plots)=fc_data2$NAME

#Residuals
residuals<- lapply(fc_data2$data, residual_check)
names(residuals)=fc_data2$NAME

# Training Models ----
#Bronx ARIMA MODEL (0,2,1)
bronx<- subset(fc_data, NAME == "Bronx", select=c("test_date", "cumulative_number_of_positives"))
bronx<-ts(bronx[,-1], start = c(2020, 1), frequency = 365)
train_bronx<- subset(bronx, end = length(bronx)-20)
test_bronx<- subset(fc_data, NAME == "Bronx", select=c("test_date", "cumulative_number_of_positives"))
test_bronx<-ts(test_bronx[,-1], start = c(2020, 1), frequency = 365)

sarima.for(train_bronx, n.ahead = 20, p = 0, d = 2, q = 1) %$% lines(test_bronx)

#Kings ARIMA MODEL (0,2,1)
kings<- subset(fc_data, NAME == "Kings", select=c("test_date", "cumulative_number_of_positives"))
kings<-ts(kings[,-1], start = c(2020, 1), frequency = 365)
train_kings<- subset(kings, end = length(kings)-20)
test_kings<- subset(fc_data, NAME == "Kings", select=c("test_date", "cumulative_number_of_positives"))
test_kings<-ts(test_kings[,-1], start = c(2020, 1), frequency = 365)

sarima.for(train_kings, n.ahead = 20, p = 0, d = 2, q = 1) %$% lines(test_kings)

#New York ARIMA MODEL (0,2,1)
nyc<- subset(fc_data, NAME == "New York", select=c("test_date", "cumulative_number_of_positives"))
nyc<-ts(nyc[,-1], start = c(2020, 1), frequency = 365)
train_nyc<- subset(nyc, end = length(nyc)-20)
test_nyc<- subset(fc_data, NAME == "New York", select=c("test_date", "cumulative_number_of_positives"))
test_nyc<-ts(test_nyc[,-1], start = c(2020, 1), frequency = 365)

sarima.for(train_nyc, n.ahead = 20, p = 0, d = 2, q = 1) %$% lines(test_nyc)

#Queens ARIMA MODEL(1,2,0)
queens<- subset(fc_data, NAME == "Queens", select=c("test_date", "cumulative_number_of_positives"))
queens<-ts(queens[,-1], start = c(2020, 1), frequency = 365)
train_queens<- subset(queens, end = length(queens)-20)
test_queens<- subset(fc_data, NAME == "Queens", select=c("test_date", "cumulative_number_of_positives"))
test_queens<-ts(test_queens[,-1], start = c(2020, 1), frequency = 365)

sarima.for(train_queens, n.ahead = 20, p = 1, d = 2, q = 0) %$% lines(test_queens)

#Richmond Arima (0,2,0)
richmond<- subset(fc_data, NAME == "Richmond", select=c("test_date", "cumulative_number_of_positives"))
richmond<-ts(richmond[,-1], start = c(2020, 1), frequency = 365)
train_richmond<- subset(richmond, end = length(richmond)-20)
test_richmond<- subset(fc_data, NAME == "Richmond", select=c("test_date", "cumulative_number_of_positives"))
test_richmond<-ts(test_richmond[,-1], start = c(2020, 1), frequency = 365)

sarima.for(train_richmond, n.ahead = 20, p = 0, d = 2, q = 0) %$% lines(test_richmond)

# Define UI for application ----

ui = shinyUI({dashboardPage(
    dashboardHeader(title = 'NY COVID-19 Tracker', 
                    tags$li(class = 'dropdown', 
                            style = 'width: 1550px; height: 0px; padding: 0px')
    ), #dashboard header end
    
    dashboardSidebar(
        sidebarMenu(
            id="tabs",
            fluidRow(
                align="center",
                h3("NY COVID-19 Tracker")),
            HTML('<center><img src="plot_zoom.png" width="200" height="100"></center>'),
            menuItem("Summary", tabName="summary", icon=icon("clipboard-list")),
            menuItem("NY State Density Maps", tabName="density_map", icon = icon("map")),
            menuItem("NYC Density Maps", tabName="nyc_density_maps", icon = icon("map-marked-alt")),
            menuItem("Geospatial Analysis", tabName="geospatial_analysis", icon = icon("chart-bar")),
            menuItem("NYC Forecasting", tabName="forecasting", icon = icon("thermometer-half")),
            menuItem("About", icon = icon("th"), tabName = "about"))),
    
    dashboardBody(
        tabItems(
            tabItem(tabName="summary",
                    h2("NY State Summary"),
                    fluidRow(
                        valueBox(state_data[["positive"]], "Positive Cases", icon = icon("user-plus")),
                        valueBox(state_data[["negative"]], "Negative Cases", icon = icon("user-minus"), color="yellow"),
                        valueBox(state_data[["hospitalizedCurrently"]], "Currently Hospitalized", icon = icon("first-aid"), color="red"),
                        valueBox(state_data[["inIcuCurrently"]], "Currently in ICU", icon = icon("procedures"), color="olive"),
                        valueBox(state_data[["recovered"]], "Recovered", icon = icon("star-of-life"), color="purple"),
                        valueBox(state_data[["death"]], "Deaths", color="navy")),
                    h4("Last updated from covidtracking.com:"),
                    state_data[["lastUpdateEt"]],
                    h2("NYC SMR Alert"),
                    fluidRow(box( width = 6, height = 600,
                        h4(tags$b("SMR per borough")),
                        "The SMR (standardized morbidity ratio) is the number of cases per person of a region. SMR is calculated by number of cases divided by the expected, thus an SMR over 1 is when the number of cases exceeds the expected.",
                        br(),br(),"Positive covid-19 cases last updated from NYC Department of Health:",
                        summary[4,2],
                        plotOutput("nyc_smr") %>% withSpinner(color="#0dc5c1")),
                             box(width = 6, height = 600,
                              h4(tags$b("Exceedance probability map by borough")), 
                              "An exceedance probability map is a graphical method of identifying high-risk regions. Thus, this map highlights boroughs in red that have a 95% probability of its current SMR doubling.",
                                br(),br(),br(),br(),plotOutput("alert_smr")%>% withSpinner(color="#0dc5c1")),
                        box(width = 6, height = 600,
                            h4(tags$b("SMR by NYC zip code")),
                            "The SMR (standardized morbidity ratio) is the number of cases per person of a region. SMR is calculated by number of cases divided by the expected, thus an SMR over 1 is when the number of cases exceeds the expected.",
                            br(),br(),"Positive covid-19 cases last updated from NYC Department of Health:",
                            summary[4,2],
                            plotOutput("nyczip_smr") %>% withSpinner(color="#0dc5c1")),
                        box(width = 6, height = 600,
                            h4(tags$b("Exceedance probability map by zip code")),
                            "An exceedance probability map is a graphical method of identifying high-risk regions. Thus, this map highlights zip codes in red that have a 95% probability of its current SMR doubling.",
                            br(),br(),br(),br(),plotOutput("alertzip_smr") %>% withSpinner(color="#0dc5c1")))
            ),
            tabItem(tabName = "density_map",
                    h2("NY State Density Maps"),
                    h4("Last updated from New York State Department of Health:"),
                    data$test_date[1],
                    fluidRow(
                        box(h4(tags$b("NY State Positive Covid-19 Cases")),
                            plotOutput("ny"),
                            sliderInput("test_date", label = "Date Range", min = min(data$test_date), 
                                        max = max(data$test_date), value = min(data$test_date), timeFormat = "%F", animate=TRUE, timezone = NULL),
                            selectInput("y", "Y Variable", 
                                        choices = c("new_positives", "cumulative_number_of_positives"),
                                        multiple = FALSE,
                                        selected = "new_positives")),
                        box(h4(tags$b("NY State Cartogram")),br(),
                            "\"A cartogram is a map in which some thematic mapping variable – such as travel time, population, or GNP – is substituted for land area or distance. The geometry or space of the map is distorted, sometimes extremely, in order to convey the information of this alternate variable.\" - Wikipedia",
                            br(), br(),"In this instance, I have created a population-based cartogram in order to overrepresent counties of larger populations.",
                            br(), br(), "Allow 10 seconds for the cartogram to load with each date adjustment.",
                            plotOutput("ny_cartogram"),
                            sliderInput("test_date2", label = "Date Range", min = min(data$test_date), 
                                        max = max(data$test_date), value = min(data$test_date), timeFormat = "%F", animate=FALSE, timezone = NULL))
                    )
            ),
            tabItem(tabName = "nyc_density_maps",
                    h2("NYC Density Maps"),
                    h4("Last updated from NYC Department of Health:"),
                    summary[4,2],
                    fluidRow(
                        # box(h4(tags$b("Google Maps of NYC")),
                        #     plotOutput("nyc")%>% withSpinner(color="#0dc5c1")),
                        box(h4(tags$b("2020 Population by NYC zip code")),
                            leafletOutput("boroughs")%>% withSpinner(color="#0dc5c1")),
                        box(h4(tags$b("NYC positive cases by borough")),
                            leafletOutput("boroughs2")%>% withSpinner(color="#0dc5c1")),
                        box(h4(tags$b("NYC positive cases by zip code")),
                            leafletOutput("nyczip_leaflet")%>% withSpinner(color="#0dc5c1"))
                    )), 
            tabItem(tabName = "geospatial_analysis",
                    fluidRow(
                    box(width = 6, height = 300,
                        h4("Point Pattern Analysis"),
                        "Point pattern analysis can be used to answer a variety of geospatial questions:",
                        br(),br(),"1) Are points randomly distributed? Are points significantly clustered together?",
                        br(),"2) Is there a bivariate distribution of events that is unique to area?",
                        br(),"3) Are spatial point patterns independent of time? Are points clustering at specific time points?",
                        br(),br(),"No public data available on point-specific positive covid-19 cases.",
                        br()), 
                    box(width = 6, height = 300,
                        h4("Spatial Interpolation Analysis"),
                        "Spatial interpolation analysis is derived from the field of geostatistics, in which points are measurements taken at set locations in order to make predictions about the area. This kind of analysis assumes a gaussian distribution, thus the points must be a continuous variable.",
                        br()),
                    box(width = 6, height = 600,
                        h4("Spatial Autocorrelation: Graph"),
                        "Spatial autocorrelation analysis is used to determine if neighbors have similar characteristics. Neighboring regions can be defined by a shared border or by distance. Here, I am defining neighbors by a shared border, and this is the graphical representation.", 
                        #plotOutput("ny_neighbors"), #commented out because the plotOutput was too slow
                        HTML('<center><img src="plot_zoom.png", width="400"></center>')),
                    box(width = 6, height = 600,
                        h4("Spatial Autocorrelation: Analysis"),
                        "There are different methods of estimating the significance of a spatial relationship of neighbors. Here I am using the Monte-Carlo method of generating 100 simulations of the Moran I value. If the distribution of the 100 simulations is significantly different from the true distribution, then the p-value is less than 0.05. Thus, neighbors share local similarities.",
                        br(),br(),verbatimTextOutput("moran_test1") %>% withSpinner(color="#0dc5c1"))
                    )
                    
            ),
            
            tabItem(tabName = "forecasting",
                    fluidRow(
                        tabBox(
                            title = "Current Testing and Positive Cases in NYC Counties",
                            id = "tabset1", height = "500px",
                            tabPanel("Info", "Here, I am plotting the trend in testing and positive cases in NYC counties.",
                                     br(), br(), "The positive correlation between cumulative tests and positive cases is the reason why I'll be using cumulative tests as a regression variable for the forecast model, in order to accurately predict cumulative positive cases."),
                            tabPanel("Bronx", plotOutput("current_bronx")%>% withSpinner(color="#0dc5c1")),
                            tabPanel("Kings", plotOutput("current_kings")%>% withSpinner(color="#0dc5c1")),
                            tabPanel("New York", plotOutput("current_nyc")%>% withSpinner(color="#0dc5c1")),
                            tabPanel("Queens", plotOutput("current_queens")%>% withSpinner(color="#0dc5c1")),
                            tabPanel("Richmond", plotOutput("current_richmond")%>% withSpinner(color="#0dc5c1"))
                        ),
                        tabBox(
                            title = "14-Day Forecast of Positive Cases",
                            height = "500px",
                            tabPanel("Info", tags$b("Understanding the forecast model:"),
                                     br(),br(), "I am using the best fitted ARIMA model to the data based on parameters that produce the lowest AIC value for each borough.", 
                                     br(),br(),"The section in blue is the predicted future. In this case, I'm forecasting cumulative positive cases 14 days into the future, using cumulative tests as a regression variable.",
                                     br(), "The blue line is the averages of predicted values.",
                                     br(), "The dark blue area denotes plus/minus 1 root-mean-square prediction error (RMSE) bounds, which is approximately equal to a 80% prediction interval.",
                                     br(), "The light blue area denotes plus/minus 2 root mean-square prediction error (RMSE) bounds, which is approximately equal to a 95% prediction interval."),
                            tabPanel("Bronx",plotOutput("fc_bronx")%>% withSpinner(color="#0dc5c1")),
                            tabPanel("Kings", plotOutput("fc_kings")%>% withSpinner(color="#0dc5c1")),
                            tabPanel("New York", plotOutput("fc_nyc")%>% withSpinner(color="#0dc5c1")),
                            tabPanel("Queens", plotOutput("fc_queens")%>% withSpinner(color="#0dc5c1")),
                            tabPanel("Richmond", plotOutput("fc_richmond")%>% withSpinner(color="#0dc5c1"))
                        ),
                        tabBox(
                            title = "Forecast Model on Training Data",
                            height = "500px",
                            tabPanel("Info", tags$b("Understanding the forecast model on training data:"),
                                    br(),br(), "It is an important practice to benchmark the forecast model on training data in order to be reassured of its accuracy of future predictions.",
                                    br(),br(),"The points in black are predicted values from the training data.",
                                    br(),"The points in red are predicted values from the training data that are projected 20 days into the future.",
                                    br(),"The black line is the true value from the testing data.",
                                    br(), "The dark shaded area denotes plus/minus 1 root-mean-square prediction error (RMSE) bounds, which is approximately equal to a 80% prediction interval.",
                                    br(), "The light shaded area denotes plus/minus 2 root mean-square prediction error (RMSE) bounds, which is approximately equal to a 95% prediction interval."),
                            tabPanel("Bronx", HTML('<img src="train_bronx.png">')),
                            tabPanel("Kings", HTML('<img src="train_kings.png">')),
                            tabPanel("New York", HTML('<img src="train_nyc.png">')),
                            tabPanel("Queens", HTML('<img src="train_queens.png">')),
                            tabPanel("Richmond", HTML('<img src="train_richmond.png">'))
                        ),
                        tabBox(
                            title = "Residuals",
                            height = "500px",
                            tabPanel("Info", tags$b("Understanding residuals:"),
                                    br(),br(),"When applying a forecasting method to a time series data,it is always good practice to check the residuals, which are the observation minus the fitted value, thus the prediction error on the training data.",
                                    br(),br(),"The residuals should look like white noise (uncorrelated and have a mean of zero) and have a gaussian distribution.",
                                    br(), "The Ljung-Box Test is one straightforward diagnostic tool of assessing a model's fit to a time series data. A p-value greater than 0.05 suggests the autocorrelations of the residuals are small, thus the model does not show significant lack of fit."),
                            tabPanel("Bronx", HTML('<img src="residuals_bronx.png">'), verbatimTextOutput("residuals_bronx")),
                            tabPanel("Kings",HTML('<img src="residuals_kings.png">'),verbatimTextOutput("residuals_kings")),
                            tabPanel("New York",HTML('<img src="residuals_nyc.png">'), verbatimTextOutput("residuals_nyc")),
                            tabPanel("Queens",HTML('<img src="residuals_queens.png">'),verbatimTextOutput("residuals_queens")),
                            tabPanel("Richmond",HTML('<img src="residuals_richmond.png">'),verbatimTextOutput("residuals_richmond"))
                        )
                    )),
            
            tabItem(tabName = "about",
                    fluidRow(
                        box(width = 12, height = 400,
                            h2("Data Sources"),
                            h4("NY state Covid-19 data are sourced from the New York State Department of Health, which can be found at https://data.ny.gov/ and covidtracking.com"),
                            h4("NYC Covid-19 data are from https://www1.nyc.gov/site/doh/covid/covid-19-data.page"),
                            h4("NY state census data are from http://gis.ny.gov/civil-boundaries/"),
                            h4("NYC census data are from the NYC Deparment of City Planning https://www1.nyc.gov/site/planning/planning-level/nyc-population/census-2010.page"),
                            h4("Polygons of NYC zip codes and boroughs, as well as NYC zip code census data, are obtained from NYC OpenData at https://data.cityofnewyork.us/ and https://www.health.ny.gov/statistics/"),
                            h4("Definition of cartogram from https://en.wikipedia.org/wiki/Cartogram"),
                            br()),
                        box(width = 12, height = 600,
                            h2("About Me"),
                            HTML('<img src="AL_photo.JPG", width="200">'),
                            h4("My name is Annie Ly. I am currently a post-baccalaureate research associate in Dr. Yasmin Hurd's laboratory at Mount Sinai School of Medicine in New York, NY. I will be joining Dr. David Root's laboratory at University of Colorado Boulder as a PhD student in the Behavioral Neuroscience Program this Fall 2020."),
                            br(),h4("R code will be made available on github account anni3ly as soon as possible."),
                            h4("Contact information: anni3.ly@gmail.com "))
                    )
            )
        ),
    ),
    
)})

# Define server logic ----
server <- function(input, output) {

    output$ny <- renderCachedPlot({
        data %>%
            subset(test_date == input$test_date) %>%
            ggplot() +
            geom_sf(mapping = aes_string(fill = input$y),
                    color = "#ffffff", size = 0.05) +
            coord_sf(datum = NA) +
            theme_void() +
            scale_fill_viridis() +
            guides(fill = guide_colorbar(title.position = "top")) +
            theme(legend.position = "bottom")
    }, cacheKeyExpr = { list(input$test_date, input$y) })
    
    output$nyc_smr <- renderPlot({
        spplot(nycb_sp, "covid_SMR")
    })
    
    output$alert_smr <- renderPlot({
        spplot(nycb_sp, "covid_gt_2", col.regions = pal, at = seq(0, 1, len = 50))
    })
    
    output$nyczip_smr <- renderPlot({
        spplot(nyczip_sp, "covid_SMR")
    })
    
    output$alertzip_smr <- renderPlot({
        spplot(nyczip_sp, "covid_gt_2", col.regions = pal, at = seq(0, 1, len = 50))
    })
    
    output$moran_test1 <- renderPrint({
        moran.mc(
            sp_nydata@data$cumulative_number_of_positives,
            nb2listw(ny_nb),
            nsim = 100
        )
    })
    
    output$ny_cartogram <- renderCachedPlot({
        
        data %>%
        subset(test_date == input$test_date2) %>%
            cartogram_cont("POP2010", itermax = 5) %>% 
            st_as_sf() %>%
            st_transform("+proj=longlat +datum=WGS84") %>%
            ggplot() +
            geom_sf(mapping = aes_string(fill = "log10_cumulative_positives"),
                    color = "#ffffff", size = 0.05) +
            coord_sf(datum = NA) +
            theme_void() +
            scale_fill_viridis(name = "log10 of cumulative positive cases") +
            guides(fill = guide_colorbar(title.position = "top")) +
            theme(legend.position = "bottom")
    }, cacheKeyExpr = { input$test_date2 })
    
    # output$nyc <- renderPlot({
    #     nyc <- c(lon = -74.0059, lat = 40.7128)
    #     nyc_map <- get_map(nyc, zoom = 11, scale=1)
    #     ggmap(nyc_map)
    # })
    
    output$boroughs <- renderLeaflet({
        nyc.bound <- st_transform(nyc.bound, "+proj=longlat +datum=WGS84")
        st_crs(nyc.bound)

        col_pal<- colorNumeric("Blues", domain= nyc.bound$POPULATION)

        nyc.bound %>%
            leaflet(options = leafletOptions(dragging = TRUE,
                                             minZoom = 10,
                                             maxZoom = 13)) %>%
            addProviderTiles("CartoDB") %>%
            addPolygons(weight=1,
                        color= ~col_pal(POPULATION),
                        label= ~paste0(PO_NAME, " Population: ", POPULATION),
                        highlight = highlightOptions(weight=3, color="red", bringToFront = TRUE)) %>%  setView(lng = -74.0059, lat = 40.7128, zoom = 10) %>%
            setMaxBounds(lng1 = -74.0059,
                         lat1 = 40.7128,
                         lng2 = -74.0059,
                         lat2 = 40.7128) %>%
            addLegend("topleft", pal = col_pal, values = ~POPULATION,
                      title = "2020 Population",
                      opacity = 1
            )

    })
    
    output$boroughs2 <- renderLeaflet({
        borough_dat <- st_transform(borough_dat, "+proj=longlat +datum=WGS84")

        col_pal2<- colorNumeric("Reds", domain= borough_dat$COVID_CASE_COUNT)

        borough_dat %>%
            leaflet(options = leafletOptions(dragging = TRUE,
                                             minZoom = 10,
                                             maxZoom = 15)) %>%
            addProviderTiles("CartoDB") %>%
            addPolygons(weight=3,
                        color= ~col_pal2(COVID_CASE_COUNT),
                        label= ~paste0(boro_name, " Covid-19 Cases: ", COVID_CASE_COUNT),
                        highlight = highlightOptions(weight=3, color="red", bringToFront = TRUE)) %>% setView(lng = -74.0059, lat = 40.7128, zoom = 10) %>%
            addLegend("topleft", pal = col_pal2, values = ~COVID_CASE_COUNT,
                      title = "Covid Cases by Borough",
                      opacity = 1)
    })

    output$nyczip_leaflet <- renderLeaflet({
        nyczip_na <- st_transform(nyczip_na, "+proj=longlat +datum=WGS84")
        
        col_pal3 <- colorNumeric("Greens", domain= nyczip_na$Positive)
        
        nyczip_na %>%
            leaflet(options = leafletOptions(dragging = TRUE,
                                             minZoom = 10,
                                             maxZoom = 15)) %>%
            addProviderTiles("CartoDB") %>%
            addPolygons(weight=1,
                        color= ~col_pal3(Positive),
                        label= ~paste0(PO_NAME, " Covid-19 Cases: ", Positive),
                        highlight = highlightOptions(weight=3, color="red", bringToFront = TRUE)) %>% setView(lng = -74.0059, lat = 40.7128, zoom = 10) %>%
            addLegend("topleft", pal = col_pal3, values = ~Positive,
                      title = "Covid Cases by Zip Code",
                      opacity = 1)
    })
    
    # # Commented out because plotOutput was too slow
    # output$ny_neighbors <- renderPlot ({
    #     plot(sp_nydata); plot(ny_nb, ny_centers, add = TRUE)
    # })
    
    output$tabset1Selected <- renderText({
        input$tabset1
    })

    output$current_bronx <- renderPlot({
        existing_plots$Bronx + 
            theme(axis.text.x = element_blank(), axis.title.y=element_blank())
    })
    
    output$current_kings <- renderPlot({
        existing_plots$Kings + 
            theme(axis.text.x = element_blank(), axis.title.y=element_blank())
    })
    
    output$current_nyc <- renderPlot({
        existing_plots$"New York" + 
            theme(axis.text.x = element_blank(), axis.title.y=element_blank())
    })
    
    output$current_queens <- renderPlot({
        existing_plots$Queens + 
            theme(axis.text.x = element_blank(), axis.title.y=element_blank())
    })
    
    output$current_richmond <- renderPlot({
        existing_plots$Richmond + 
            theme(axis.text.x = element_blank(), axis.title.y=element_blank())
    })
    
    output$fc_bronx <- renderPlot({
        fc_plots$Bronx + 
            theme(axis.text.x = element_blank()) +
            ylab("cumulative number of positive cases")
    })
    
    output$fc_kings <- renderPlot({
        fc_plots$Kings + 
            theme(axis.text.x = element_blank())+
            ylab("cumulative number of positive cases")
    })
    
    output$fc_nyc <- renderPlot({
        fc_plots$"New York" + 
            theme(axis.text.x = element_blank())+
            ylab("cumulative number of positive cases")
    })
    
    output$fc_queens <- renderPlot({
        fc_plots$Queens + 
            theme(axis.text.x = element_blank())+
            ylab("cumulative number of positive cases")
    })
    
    output$fc_richmond <- renderPlot({
        fc_plots$Richmond + 
            theme(axis.text.x = element_blank())+
            ylab("cumulative number of positive cases")
    })
    
    output$residuals_bronx <- renderPrint({
        residuals$Bronx 
    })
    
    output$residuals_kings <- renderPrint({
        residuals$Kings
    })
    
    output$residuals_nyc <- renderPrint({
        residuals$"New York" 
    })
    
    output$residuals_queens <- renderPrint({
        residuals$Queens
    })
    
    output$residuals_richmond <- renderPrint({
        residuals$Richmond 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
