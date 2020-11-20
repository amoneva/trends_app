# Load packages
library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)


# Import data
trends <- read_csv(
    file = "af_df_v3.csv",
    col_types = cols(
        i_dating_ui = col_double(),
        i_dating_li = col_double(),
        o_dating_ui = col_double(),
        o_dating_li = col_double(),
        i_ticket_ui = col_double(),
        i_ticket_li = col_double(),
        o_ticket_ui = col_double(),
        o_ticket_li = col_double(),
        i_dtod_trades_ui = col_double(),
        i_dtod_trades_li = col_double(),
        o_dtod_trades_ui = col_double(),
        o_dtod_trades_li = col_double()
    )
)


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    titlePanel(title = "Cybercrime and fraud trends during COVID-19"),
    
    sidebarLayout(
        
        position = "left",
        
        sidebarPanel(
            "Lockdown and social distancing measures imposed on the UK during 
            the COVID pandemic have transformed the landscape of mobility and 
            social interaction. Criminological theory suggests that this can 
            have a dramatic impact on crime, but evidence is usually difficult 
            to obtain. 
            This app displays pairs of data trends that allow for the visual
            comparison between cybercrime and fraud figures as recorded by UK 
            Action Fraud, and routine activity data as recorded by several 
            sources from the UK (i.e., Office for National Statistics, Civil 
            Aviation Authority, UK Cinema Association).
            Note: that two trends appear to be correlated does not necessarily 
            mean that one causes the other.",
            
            helpText("This app is an extension of the article 'Empty streets, 
            busy Internet. A time series analysis of cybercrime and fraud trends
            during COVID-19.' Copyright 2020 The Authors"),
            
            selectInput(
                inputId = "variable_1",
                label = "Select a crime variable to display",
                choices = list(
                    "Total cybercrime",
                    "Cybercrime affecting individuals",
                    "Cybercrime affecting organisations",
                    "Total fraud",
                    "Fraud affecting individuals",
                    "Fraud affecting organisations",
                    "Total online shopping and auction fraud",
                    "Online shopping and auction fraud affecting individuals",
                    "Online shopping and auction fraud affecting organisations",
                    "Total dating fraud",
                    "Dating fraud affecting individuals",
                    "Dating fraud affecting organisations",
                    "Total ticket fraud",
                    "Ticket fraud affecting individuals",
                    "Ticket fraud affecting organisations",
                    "Total door-to-door sales and bogus tradesmen fraud",
                    "Door-to-door sales and bogus tradesmen fraud affecting individuals",
                    "Door-to-door sales and bogus tradesmen fraud affecting organisations"
                )
            ),
            
            selectInput(
                inputId = "variable_2",
                label = "Select a routine activities variable to display",
                choices = list(
                    "Internet retail sales",
                    "Passenger air travellers",
                    "Cinema ticket sales"
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
            
            textOutput("selected_var_date"),
            
            plotlyOutput("line_chart")
        )
    )
)


# Define server logic
server <- function(
    input, 
    output
) {
    
    output$selected_var_date <- renderText({ 
        paste(
            "You have selected", 
            input$variable_1,
            "and ",
            input$variable_2,
            "in a period spanning from ",
            input$date[1],
            "to",
            input$date[2],
            ". "
        )
    })
    
    output$line_chart <- renderPlotly({
        
        y_val_1 <- switch(
            input$variable_1,
            "Total cybercrime" = trends$t_cybercrime,
            "Cybercrime affecting individuals" = trends$i_cybercrime,
            "Cybercrime affecting organisations" = trends$o_cybercrime,
            "Total fraud" = trends$t_fraud,
            "Fraud affecting individuals" = trends$i_fraud,
            "Fraud affecting organisations" = trends$o_fraud,
            "Total online shopping and auction fraud" = trends$t_on_shop_auction,
            "Online shopping and auction fraud affecting individuals" = trends$i_on_shop_auction,
            "Online shopping and auction fraud affecting organisations" = trends$o_on_shop_auction,
            "Total dating fraud" = trends$t_dating,
            "Dating fraud affecting individuals" = trends$i_dating,
            "Dating fraud affecting organisations" = trends$o_dating,
            "Total ticket fraud" = trends$t_ticket,
            "Ticket fraud affecting individuals" = trends$i_ticket,
            "Ticket fraud affecting organisations" = trends$o_ticket,
            "Total door-to-door sales and bogus tradesmen fraud" = trends$t_dtod_trades,
            "Door-to-door sales and bogus tradesmen fraud affecting individuals" = trends$i_dtod_trades,
            "Door-to-door sales and bogus tradesmen fraud affecting organisations" = trends$o_dtod_trades
        )
        
        y_val_2 <- switch(
            input$variable_2,
            "Internet retail sales" = trends$internet_retail_sales,
            "Passenger air travellers" = trends$passenger_air_travellers,
            "Cinema ticket sales" = trends$cinema_ticket_sales
        )
        
        # Plotly version of the plot
        
        plot_ly(data = trends) %>% 
            add_trace(
                x = ~ date,
                y = ~ y_val_1,
                mode = "lines",
                yaxis = "y1",
                # text = input$variable_1,
                name = input$variable_1,
                type = "scatter"
            ) %>% 
            add_trace(
                x = ~ date,
                y = ~ y_val_2,
                mode = "lines",
                yaxis = "y2",
                # text = input$variable_2,
                name = input$variable_2,
                type = "scatter"
            ) %>% 
            layout(
                shapes = list(
                    type = "line",
                    x0 = dmy("01-03-2020"), 
                    x1 = dmy("01-03-2020"),
                    y0 = 0, 
                    y1 = max(na.omit(y_val_1)),
                    line = list(
                        dash = "dot",
                        width = 1
                    )
                ),
                annotations = list(
                    x = dmy("01-03-2020"),
                    y = 500,
                    text = "Lockdown",
                    ax = -50
                ),
                xaxis = list(
                    title = "",
                    range = c(input$date[1], input$date[2])
                ),
                yaxis = list(
                    title = "",
                    range = c(0, max(na.omit(y_val_1))),
                    color = "#1f77b4",
                    zeroline = FALSE
                ),
                yaxis2 = list(
                    title = "",
                    range = c(0, max(na.omit(y_val_2))),
                    color = "#ff7f0e",
                    overlaying = "y", 
                    side = "right",
                    zeroline = FALSE
                ),
                legend = list(
                    x = 0,
                    y = 1.2,
                    orientation = "h"
                )
            )
        
    })
}


# Run the application 
shinyApp(
    ui = ui, 
    server = server
)
