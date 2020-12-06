# INFO 201
# Final Group Project
# Date: December 9, 2020

library("dplyr")
library("tidyr")

# Load the `ggplot2` library for data manipulation 
library("ggplot2")

library("plotly")
library("shiny")


# =====================================================================
# Read the U.S.Census "Historical Income Tables: Race" data
# =====================================================================
income_data <- read.csv("https://raw.githubusercontent.com/brandonwong3/final-deliverable/master/Census_Summary.csv", stringsAsFactors = FALSE, header=TRUE)

# Function to remove ',' in CSV data for # in the thousands
replaceCommas<-function(x){
  x<-as.numeric(gsub(",", "", x))
}

# =====================================================================
# Wrangle Data from US Census
# =====================================================================

# Clean Data, remove commas from columns with strings of numbers
# Note: in apply() use MARGIN=2 for columns
data_apply <- apply(income_data[ ,c(2:7)], 2, replaceCommas)
data_new <- income_data                                                         # Replicate original data
data_new[ , colnames(data_new) %in% colnames(data_apply)] <- data_apply  # Replace specific columns

# Data for all four Race categories start at 2002; filter from 2002
data_new <- data_new %>% 
  filter(Year >= 2002)


# =====================================================================
# FUNCTIONS
# =====================================================================


# ========================
# Reactive Chart
# Function: Household income by Fifth (Line Chart)
# ggplot geom_ function
# Parameter chooses which fifth of the household income data to graph

plot_chart <- function(by_column) {
  by_column <- ensym(by_column)   # needed to read column name
  
  df <- ggplot(data = data_new) +
    geom_line(mapping = aes(x = Year, y = !!by_column, color = Race)) +
    
    labs(     # Add title and axis labels
      title = paste("Household Income for Each Fifth"),
      x = "Year", # x-axis label
      y = "Income ($)", # y-axis label
      color = "" # legend label for the "color" property
    ) 
   
  df
  ggplotly(df) %>% 
    # Legend overlapped the plotly menus, used manual placement for legend
    add_annotations( text="Race", xref="paper", yref="paper",
                            x=1.02, xanchor="left",
                            y=0.8, yanchor="bottom",  # Same y as legend below
                            legendtitle=TRUE, showarrow=FALSE ) %>%
    layout( legend=list(y=0.8, yanchor="top" ) )
}


# ========================
# Reactive Chart
# Function: Percentage Comparison of Non-White to White Income (Line Chart)
# ggplot geom_ function
# Parameter chooses which fifth of the household income data to graph

plot_income_percent <- function(by_column) {
  by_column <- ensym(by_column) # fixes column name passed as argument
  
  income_long <- data_new %>%   # change data frame to long format
    select(Year, by_column, Race) # select fifth vector
  
  income_dev <- spread(
    income_long, # data frame to spread from
    key = Race, 
    value = by_column  
  )
  income_dev <- mutate(income_dev, # calculate % of non-white to white
                       Asians = Asian / White,
                       Hispanics = Hispanic / White,
                       Whites = 1.0,
                       Blacks = Black / White) 
  
  income_dev <- income_dev %>% # remove orig data, keep only %
    select(Year, Asians, Hispanics, Whites, Blacks)

  income_long <- gather(   # reassemble from long to wide to plot
    income_dev, # data frame to gather from
    key = Race,  
    value = Income, 
    -Year # columns to gather data from, as in dplyr's `select 
  )
  df <- ggplot(data = income_long) +
    geom_line(mapping = aes(x = Year, y = Income, color = Race)) +
    
    labs(
      title = "Income by Race compared to Whites", # plot title 
      x = "Year", # x-axis label
      y = "Income (%)", # y-axis label
      color = "" # legend label deleted
      ) +
    scale_y_continuous(breaks=seq(0.5, 1.5, by = 0.1))
  
  df
  ggplotly(df) %>% 
    # Legend overlapped the plotly menus, used manual placement for legend
    add_annotations( text="Race", xref="paper", yref="paper",
                   x=1.02, xanchor="left",
                   y=0.8, yanchor="bottom",  # Same y as legend below
                   legendtitle=TRUE, showarrow=FALSE ) %>%
    layout( legend=list(y=0.8, yanchor="top" ) )
}


# =====================================================================
# UI code
# =====================================================================

# Define the first page content; uses `tabPanel()` and `sidebarLayout()` 
# layout functions together (as an example)
page_one <- tabPanel(
  "Intro tab", # label for the tab in the navbar 
  titlePanel("Exploring the U.S. Income Gap by Race"), # show with a displayed title
  
  "It should be no surprise that the yearly income of households in the",
  "United States varies by race. Despite the many decades that have passed",
  "since the Civil Rights movement, the income gap between races remains.",
  "This project uses data from the ",
  a("U.S. Census Bureau", href="https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-income-inequality.html"),
  " to explore this gap.",
  "",
  "",
  "",
  
  br(), br(),
  

  p("Income is just one measure of the economical financial racial gap ",
    "in the United Stats. Ultimately, the historical roots of structural ",
    "income inequality has manifested in a racial wealth gap that is ",
    "difficult to overcome. The White middle class prospered and built ",
    "wealth under the Jim Crow laws, the G.I. Bill, taxcuts, segregation, ",
    "and discrimination against Blacks owning wealth. Future generations ",
    "benefited from this accumulated wealth. According to ",
    a("Racial Wealth Gap in the United States", href="https://www.thebalance.com/racial-wealth-gap-in-united-states-4169678"),
    " the racial wealth gap is widening."),
  p(""),
  p("")
  
)


# Define content for the second page 
page_two <- tabPanel(
  "Income tab", # label for the tab in the navbar
  # ...more content would go here... 
  # This content uses a sidebar layout
  sidebarLayout( sidebarPanel(
    
    radioButtons(inputId = "rb_chosen", # key assigned
                 label = "Fifth (quintile) Selection",
                 choices = list("Lowest" = "Lowest", 
                                "Second" = "Second",
                                "Third" = "Third",
                                "Fourth" = "Fourth",
                                "Top 5 Percent" = "Top5")
                 ), # close radio button
  ), # close sidebarLayout
  
  mainPanel(
    h3("Historical Income Inequality"),
    br(),
    plotlyOutput(outputId = "income_plot"),    # panel output
    br(),
    
    br("The income gap for Blacks has not improved despite what people ",
       "may think. The income gap for Hipanics has narrowed somewhat. ",
       "What is most interesting is that the gaps have remained the same ",
       "at every income level. Even within the top five percent of incomes ",
       "for each race, Blacks and Hispanics experience the same amount ",
       "of reduced income as the lower levels of income. "),
    br("In order to close the gap, Blacks and Hispanics must ",
      "move more of their population in to the middle class. ",
      "This means more preparing them for employment in middle class ",
      "jobs and ensuring they receive compensation equal to Whites for ",
      "the same work.")
  ) )
)


# Define content for the third page 
page_three <- tabPanel(
  "Comparison tab", # label for the tab in the navbar

  sidebarLayout( sidebarPanel(
    radioButtons(inputId = "rb_chosen_c", # key assigned
                 label = "Fifth (quintile) Selection",
                 choices = list("Lowest" = "Lowest", 
                                "Second" = "Second",
                                "Third" = "Third",
                                "Fourth" = "Fourth",
                                "Top 5 Percent" = "Top5")
    ), # close radio button
  ), # close sidebarLayout
  
  mainPanel(
    h3("Historical Income Inequality"),
    br(),
    plotlyOutput(outputId = "compare_plot"),    # panel output
    br(),
    
    br("This visual illustrates a comparison of income between Non-Whites ",
       "and Whites. This clearly shows that Blacks and Hipanics have not  ",
       "made significant strides in closing the income gap with Whites. "),
    p(),
    p("Black income has remained flat (no growth) at all income levels."),
    p("Hispanics have made slight improvements in income over time."),
    p("Asians have trended higher and even widened the gap with Whites.")
  ) )
)



# Pass each page to a multi-page layout (`navbarPage`) 
my_ui <- navbarPage(
  "My Application", # application title
  page_one,   # include the first page content
  page_two,   # include the second page content
  page_three   # include the third page content
)


# =====================================================================
# Server code
# =====================================================================

my_server <- function(input, output) {
  # Create and return line chart of Page two
  output$income_plot <- renderPlotly({
    plot_chart(!!input$rb_chosen)
  })

  # Create and return line chart of Page Three
  output$compare_plot <- renderPlotly({
    plot_income_percent(!!input$rb_chosen_c)
  })
}



# =====================================================================
# Start running the application
# =====================================================================

shinyApp(ui = my_ui, server = my_server)
