#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(stringr)
library(DT)

### My plot theme
theme_amc <- theme(
    title = element_text(size = 20, face="bold"), 
    axis.title = element_text(size = 16, face="bold"), 
    axis.text.y = element_text(size=14), 
    axis.text.x = element_text(size=14),
    axis.line = element_line(color="ivory3", size = 0.5),
    axis.ticks = element_line(colour="white"),
    panel.background = element_rect(fill = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "ivory3"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "ivory3"),
    text = element_text(family="Arial")
)

# import dataset
df = read.csv("20200227_EBOV_final10_cumulative.csv")
df <- df[ which(df$sm.date!='9/13/18'),] 
l.virus <- as.list(levels(df$virus))
l.smol <- as.list(levels(df$sm.id))
l.tech <- as.list(levels(df$researcher))
l.cells <- as.list(levels(df$cells))

# prepare stats for bar chart
st.err <- function(x) {sd(x)/sqrt(length(x))}

aggdata <-aggregate(df$relative.gfp, 
                    by=list(df$sm.id,df$sm.conc,df$virus,df$cells), FUN=mean)
aggdata_sd <-aggregate(df$relative.gfp, 
                       by=list(df$sm.id,df$sm.conc,df$virus,df$cells), FUN=sd)
aggdata_sterr <-aggregate(df$relative.gfp, 
                          by=list(df$sm.id,df$sm.conc,df$virus,df$cells), FUN=st.err)
estats = data.frame(aggdata$Group.1, aggdata$Group.2, aggdata$Group.3, aggdata$Group.4, 
                    aggdata$x, aggdata_sd$x, aggdata_sterr$x)
estats <- na.omit(estats)
colnames(estats) <- c('sm', 'conc', 'virus', 'cells', 'mean', 'sd', 'se')
rm(aggdata)
rm(aggdata_sd)
rm(aggdata_sterr)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    # Make subset based on inputs
    df_subset1 <- reactive({
        a <- subset(estats, (estats$virus %in% input$virus_in) & 
                        (estats$cells %in% input$cells_in) &
                        (estats$sm %in% input$smol_in))
        a <- droplevels(a)
        return(a)
    })    
    
    # Bar chart: all viruses on either Huh7 or 293t
    output$bar <- renderPlot({
        ggplot(data=df_subset1(), aes(x=sm, y=mean, fill = virus)) +
            geom_bar(stat="identity", position=position_dodge() )  +
            geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, 
                          position=position_dodge(.9) ) + 
            theme(plot.title = element_text(size = 20, face="bold"), 
                  axis.title = element_text(size = 16, face="bold"),
                  axis.text.y = element_text(size=14), 
                  axis.text.x = element_text(size=14, angle=315, hjust=0.2, vjust=0.5),
                  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) + 
            labs(title="Viral Transduction or Infection \nin the Presence of Small Molecules",
                 x="Small Molecule", y="% GFP(+) or RFP(+), Normalized") +
            theme(legend.position = "bottom", 
                  legend.title=element_text(size=14),
                  legend.text=element_text(size=14),
                  legend.background = element_rect(fill="white", size=0.5,
                                                   linetype="solid", colour ="black")) +
            scale_fill_brewer("Virus", palette="Set2") +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 13))+
            theme_amc
    })
    
    # Make subset based on inputs
    df_subset2 <- reactive({
        b <- subset(estats, (estats$virus %in% input$virus_in2) & 
                        (estats$sm %in% input$smol_in2))
        b <- droplevels(b)
        return(b)
    })  
    
    # Bar chart: select viruses on both Huh7 or 293t
    output$bar_cells <- renderPlot({
        ggplot(data=df_subset2(), aes(x=sm, y=mean, fill = cells)) +
            geom_bar(stat="identity", position=position_dodge() )  +
            geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, 
                          position=position_dodge(.9) ) + 
            theme(plot.title = element_text(size = 20, face="bold"), 
                  axis.title = element_text(size = 16, face="bold"),
                  axis.text.y = element_text(size=14), 
                  axis.text.x = element_text(size=14, angle=315, hjust=0.2, vjust=0.5),
                  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) + 
            labs(title="Viral Transduction or Infection \nin the Presence of Small Molecules",
                 x="Small Molecule", y="% GFP(+) or RFP(+), Normalized") +
            theme(legend.position = "bottom", 
                  legend.title=element_text(size=14),
                  legend.text=element_text(size=14),
                  legend.background = element_rect(fill="white", size=0.5,
                                                   linetype="solid", colour ="black")) +
            scale_fill_brewer("Cell Type", palette="Set2") +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 13))+
            theme_amc
    })
    
    # dot plot & data table
    # Make subset based on inputs
    df_mod = df[, c(1, 2, 3, 4, 5, 6, 7)] # only include relevant columns
    
    df_subset3 <- reactive({
        a <- subset(df_mod, (df_mod$virus %in% input$virus_in3) & 
                        (df_mod$cells %in% input$cells_in3))
        a <- droplevels(a)
        return(a)
    })  
    
    output$dot <- renderPlot({
        ggplot(data=df_subset3(), aes(x=sm.id, y=relative.gfp, group=sm.date)) +
            geom_point(aes(colour = sm.date), size=2) + 
            theme_minimal() +
            labs(title="Viral Transduction or Infection \nin the Presence of Small Molecules",
                 x="Small Molecule", y="% GFP(+) or RFP(+), Normalized",
                 color = 'Date') +
            theme(plot.title = element_text(size = 20, face="bold"),
                  axis.title = element_text(size = 16, face="bold"), 
                  axis.text.y = element_text(size=14), 
                  axis.text.x = element_text(size=14, angle=315, hjust=0.2, vjust=0.5),
                  plot.margin = unit(c(0.2, 1, 0.2, 0.5), "cm"),
                  legend.position = 'right', 
                  legend.title=element_text(size=14, face="bold"),
                  legend.text=element_text(size=14),
                  legend.background = element_rect(fill="white", size=0.5,
                                                   linetype="solid", colour ="black")) +
            geom_hline(yintercept = 100, linetype = 2) 
    })
    
    # output table
    output$tbldot <- DT::renderDataTable({
        DT::datatable(df_subset3(), rownames = FALSE, 
                      colnames = c('Date','SMol ID','SMol Conc', '%Raw Fluor',
                                   '%Norm Fluor','Virus','Cells'))
    },
    )
})
