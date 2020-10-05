#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
library(shiny)
library(shinythemes)
library(markdown)
library(DT)

# import dataset
df = read.csv("20200227_EBOV_final10_cumulative.csv")
df <- df[ which(df$sm.date!='9/13/18'),] 
l.virus <- as.list(levels(df$virus))
l.smol <- as.list(levels(df$sm.id))
l.cells <- as.list(levels(df$cells))

# Define virus ui
ui.v <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            width = 3,
            radioButtons(inputId="cells_in", label="Cell Type",
                         choices=l.cells, selected="Huh7"),
            checkboxGroupInput(inputId="virus_in", label="Virus",
                               choices=l.virus, selected="EBOV-HIV Mayinga"),
            checkboxGroupInput(inputId="smol_in", label="Small Molecule",
                               choices=l.smol, selected=l.smol),
        ), 
        mainPanel(
            plotOutput(outputId = "bar"),
            h4("Data updated 3/1/2020")
        ), 
        position = c("left", "right"),
    )
)

# Define virus ui
ui.c <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            width = 3,
            radioButtons(inputId="virus_in2", label="Virus",
                         choices=l.virus, selected="EBOV-HIV Mayinga"),
            checkboxGroupInput(inputId="smol_in2", label="Small Molecule",
                               choices=l.smol, selected=l.smol),
        ), 
        mainPanel(
            plotOutput(outputId = "bar_cells"),
            h4("Data updated 3/1/2020")
        ), 
        position = c("left", "right"),
    )
)

# Define dotplot ui
ui.d <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            width = 3,
            radioButtons(inputId="virus_in3", label="Virus",
                         choices=l.virus, selected="EBOV-HIV Mayinga"),
            radioButtons(inputId="cells_in3", label="Cell Type",
                         choices=l.cells, selected="Huh7"),
        ), 
        mainPanel(
            plotOutput(outputId = "dot"),
            h4("Data updated 3/1/2020"),
            fluidRow(
                column(12,
                       DT::dataTableOutput(outputId = "tbldot")
                )
            )
        ), 
        position = c("left", "right")
    )
)

# Define UI for tabs
shinyUI(
    navbarPage("Small Molecule Project", 
               theme = shinytheme("cerulean"),
               tabPanel("Viruses", verbatimTextOutput("t1v"), ui.v),
               tabPanel("Cell Types", verbatimTextOutput("t2c"), ui.c),
               tabPanel("Raw Data", verbatimTextOutput("t3d"), ui.d)
    )
)

