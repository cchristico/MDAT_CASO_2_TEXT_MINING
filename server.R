#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$words + 1)

        # draw the histogram with the specified number of bins
        
        pal <- colorRampPalette(colors = c("blue", "lightblue"))(input$words)
        
        barplot(d[1:input$words,]$freq, las = 2, names.arg = d[1:input$words,]$word,
                col = pal,
                #c("#E69F00", "#56B4E9", "#009E73")
                main ="Top Palabras mas Frecuentes",
                ylab = "Frecuencia de Palabras")

    })
    
    Dataframe2 <- reactive({
       # text[[1]]$content
        input$doc
    })
    output$textIni <- renderPrint({
        indice <- strtoi(Dataframe2())
        text[[indice]]$content
    })
    
    output$textCl1 <- renderPrint({
        indice <- strtoi(Dataframe2())
        textCln[[indice]]$content
    })

    output$textLem <- renderPrint({
        indice <- strtoi(Dataframe2())
        textLemaSte[[indice]]$content
    })
    
    output$wclo <- renderPlot({

 wordcloud(words = d$word, freq = d$freq, min.freq = input$freq,
                  max.words=input$max, random.order=FALSE, rot.per=0.35,
                  colors=brewer.pal(8, "Dark2"))

        
    })
    
    output$topicNum <- renderPlot({
    FindTopicsNumber_plot(result)
    })
    
    
     output$Topics<-renderPlot({plot_topic_10})
    
    output$docTopic<-renderPlotly({
        
        docsExaple <- strtoi(input$docs)
         topicProportionExamples <- theta[docsExaple,]
         colnames(topicProportionExamples) <- topicNames
         vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(docsExaple)), variable.name = "topic", id.vars = "document")

            gPlot <- ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") +
             geom_bar(stat="identity") +
             theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
             coord_flip() +
             facet_wrap(~ document, ncol = length(docsExaple))

        
    })
    output$tblTopics<-renderDataTable({
        
        docsExaple <- strtoi(input$docs)
        topicProportionExamples <- theta[docsExaple,]
        colnames(topicProportionExamples) <- topicNames
        vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(docsExaple)), variable.name = "topic", id.vars = "document")
        vizDataFrame <- vizDataFrame[order(-vizDataFrame$value),]
        vizDataFrame
        
    })

})
