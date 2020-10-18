# Load packages
library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)

# Settings
setwd(dir = "C:/Users/amoneva/OneDrive - De Haagse Hogeschool/research/trends_app")

# Import data
trends <- read_csv(file = "data/af_df.csv")

# Transform data
trends = as_tibble(
    t(trends), 
    rownames = "row_names"
)
names(trends) <- trends[1, ]
trends <- trends[-1, ]
trends$X1 <- dmy(paste(
    "01-", 
    trends$X1, 
    sep = ""
))

trends <- trends %>% 
    rename(
        date = "X1",
        t_cybercrime = "Total Cybercrime",
        i_cybercrime = "Cybercrime Individuals",
        o_cybercrime = "Cybercrime organisations",
        t_fraud = "Total Fraud",
        i_fraud = "Fraud individuals",
        o_fraud = "Fraud organisations"
    ) %>% 
    mutate(
        t_cybercrime = as.numeric(t_cybercrime),
        i_cybercrime = as.numeric(i_cybercrime),
        o_cybercrime = as.numeric(o_cybercrime),
        t_fraud = as.numeric(t_fraud),
        i_fraud = as.numeric(i_fraud),
        o_fraud = as.numeric(o_fraud)
    ) %>% 
    select(
        date,
        t_cybercrime,
        i_cybercrime,
        o_cybercrime,
        t_fraud,
        i_fraud,
        o_fraud
    )

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    titlePanel(title = "Cybercrime trends during COVID-19"),
    
    sidebarLayout(
        
        position = "left",
        
        sidebarPanel(
            "placeholder text",
            helpText("Create crime trends line charts
                     with data from UK Auction Fraud."),
            
            checkboxGroupInput(
                inputId = "variable",
                label = "Choose a variable to display",
                choices = list(
                    "Total cybercrime",
                    "Cybercrime affecting individuals",
                    "Cybercrime affecting organisations",
                    "Total fraud",
                    "Fraud affecting individuals",
                    "Fraud affecting organisations"
                )
            ),
            
            sliderInput(
                inputId = "date",
                label = "Period of interest",
                min = min(trends$date),
                max = max(trends$date),
                value = c(min(trends$date), max(trends$date)),
                timeFormat = "%b-%y"
            )
        ),
        
        mainPanel(
            # h1(
            #     "placeholder text",
            #     align = "left"
            # ),
            
            textOutput("selected_var"),
            
            textOutput("min_max"),
            
            plotlyOutput("line_chart")
        )
    )
)

# Define server logic
server <- function(
    input, 
    output
) {
    
    output$selected_var <- renderText({ 
        paste(
            "You have selected", 
            input$variable,
            ". "
        )
    })
    
    output$min_max <- renderText({
        paste(
            "You have chosen a period spanning from",
            input$date[1],
            "to",
            input$date[2],
            ". "
        )
    })
    
    output$line_chart <- renderPlotly({
        y_val <- switch(
            input$variable,
            "Total cybercrime" = trends$t_cybercrime,
            "Cybercrime affecting individuals" = trends$i_cybercrime,
            "Cybercrime affecting organisations" = trends$o_cybercrime,
            "Total fraud" = trends$t_fraud,
            "Fraud affecting individuals" = trends$i_fraud,
            "Fraud affecting organisations" = trends$o_fraud
        )
        
        # Plotly version of the plot
        
        plot_ly(
            data = trends,
            x = ~ date,
            y = ~ y_val,
            mode = "lines"
        ) %>% 
            layout(
                shapes = list(
                    type = "line",
                    x0 = dmy("01-03-2020"), 
                    x1 = dmy("01-03-2020"),
                    y0 = 0, 
                    y1 = max(na.omit(y_val)),
                    line = list(
                        dash = "dot",
                        width = 1
                    )
                ),
                annotations = list(
                    x = dmy("01-03-2020"),
                    y = 500,
                    text = "Lockdown",
                    ax = 50
                ),
                xaxis = list(
                    title = "Date",
                    range = c(input$date[1], input$date[2])
                ),
                yaxis = list(
                    title = input$variable,
                    range = c(0, max(y_val))
                )
            )
        
        # Ggplot2 version of the plot
        # 
        # trends %>% 
        #     ggplot(mapping = aes(
        #         x = date,
        #         y = y_val
        #     )) +
        #     geom_line(size = 1) +
        #     geom_point() +
        #     scale_x_date(
        #         date_breaks = "2 months",
        #         date_labels = "%b %y",
        #         limits = c(input$date[1], input$date[2])
        #     ) +
        #     ylim(c(0, max(y_val))) +
        #     labs(
        #         y = input$variable,
        #         x = "Date"
        #     ) +
        #     theme_classic()
    })
}

# Run the application 
shinyApp(
    ui = ui, 
    server = server
)

