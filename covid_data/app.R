library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(shinythemes)



df <- read_csv('https://raw.githubusercontent.com/databrew/intro-to-data-science/main/data/covid_data.csv')
pop <- read_csv('https://raw.githubusercontent.com/databrew/intro-to-data-science/main/data/state_pop.csv')

pop_covid <- dplyr::left_join(df, pop) 
pop_covid <- pop_covid %>% mutate(death100 = (daily_deaths / population)*100000, case100 = (daily_cases / population)*100000)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("cosmo"), 
    titlePanel("Covid cases and deaths by state"), 
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "state",
                        label = "Choose a state",
                        choices = unique(df$state),
                        selected = "Washington"),
            # Add an input called “bar_color” that controls the color that fills the bar plot
            # 
            # Add an input called “alpha_value” that controls the transparency of the bars in the chart
            selectInput(inputId = "f_color",
                        label = "Choose the first color",
                        choices = c("red", "blue", "seagreen1"),
                        selected = "blue"),
            selectInput(inputId = "s_color",
                        label = "Choose the second color (if any)",
                        choices = c("orange", "pink", "seagreen1"),
                        selected = "orange"),
            sliderInput(inputId = "alpha_value",
                        label = "Choose transparancy",
                        min = 0,
                        max = 1,
                        value = 0.8)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            # There should now be five charts. Use the fluidRow() and column() argument to put the bar charts side by side. Under the bar charts, put the line plots side by side. Under those, put the final line plot centered in the middle of the screen.
            fluidRow(column(6, plotOutput('cases_month')), column(6, plotOutput('deaths_month'))),
            fluidRow(column(6, plotOutput('cumulative_cases')), column(6, plotOutput('cumulative_deaths'))),
        fluidRow(column(8, align = "center", offset = 2,  plotOutput('per_100k')))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Create a bar chart called “cases_month” that plots a states covid cases by month
    output$cases_month <- renderPlot({
    st <- input$state
    fc <- input$f_color
    trans <- input$alpha_value
    ggplot(data = df %>% mutate(month = floor_date(date, unit = 'month')) %>% filter(state == st) %>% group_by(month) %>% summarize(cases = sum(daily_cases)), aes(month, cases)) + geom_bar(stat = "identity", fill = fc, alpha = trans) + labs(title = paste0("Monthly cases in ", st)) + ggthemes::theme_gdocs()
    })
    # 
    # Create another bar chart called “deaths_month” that plots a states covid deaths by month
    output$deaths_month <- renderPlot({
        st <- input$state
        fc <- input$f_color
        trans <- input$alpha_value
        ggplot(data = df %>% mutate(month = floor_date(date, unit = 'month')) %>% filter(state == st) %>% group_by(month) %>% summarize(deaths = sum(daily_deaths)), aes(month, deaths)) + geom_bar(stat = "identity", fill = fc, alpha = trans) + labs(title = paste0("Monthly deaths in ", st))  + ggthemes::theme_gdocs()
    })
    # 
    # Create a line plot called “cumulative_cases” that plots the cumulative cases over time (hint: use the function cumsum)
    output$cumulative_cases <- renderPlot({
        st <- input$state
        ggplot(data = df %>% mutate(month = floor_date(date, unit = 'month')) %>% filter(state == st) %>% group_by(month) %>% summarize(monthly_cases = sum(daily_cases)) %>% summarize(month, cum_cases = cumsum(monthly_cases)), aes(month, cum_cases))  + geom_line(aes(group=1))  + labs(title = paste0("Cumulative cases in ", st))  + ggthemes::theme_gdocs()
    })
    # 
    # Create a line plot called “cumulative_deaths” that plots the cumulative deaths over time (hint: use the function cumsum)
    output$cumulative_deaths <- renderPlot({
        st <- input$state
        ggplot(data = df %>% mutate(month = floor_date(date, unit = 'month')) %>% filter(state == st) %>% group_by(month) %>% summarize(monthly_deaths = sum(daily_deaths)) %>% summarize(month, cum_deaths = cumsum(monthly_deaths)), aes(month, cum_deaths))  + geom_line(aes(group=1))  + labs(title = paste0("Cumulative deaths in ", st)) + labs(title = paste0("Cumulative deaths in ", st)) + ggthemes::theme_gdocs()
    })
    # 
    output$per_100k <- renderPlot({
        st <- input$state
        fc <- input$f_color
        sc <- input$s_color
        ggplot(data = pop_covid %>% mutate(month = floor_date(date, unit = 'month')) %>% filter(state == st) %>% group_by(month) %>% summarize(monthly_cases_100k = sum(case100), monthly_deaths_100k = sum(death100)), aes(month)) + geom_line(aes(y= monthly_cases_100k), color = fc) + geom_line(aes(y=monthly_deaths_100k), color = sc) + labs(title = paste0("Cases and deaths per 100k in ", st))  + ggthemes::theme_gdocs()
    })
  
}

# Run the application
shinyApp(ui = ui, server = server)
