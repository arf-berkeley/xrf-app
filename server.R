library(pbapply)
library(reshape2)
library(TTR)
library(dplyr)
library(shinyIncubator)
library(data.table)
library(ggtern)
library(ggplot2)
library(shiny)








shinyServer(function(input, output, session) {
    
    
    
 
    
    observeEvent(input$actionprocess, {

        myData <- reactive({
            
            withProgress(message = 'Processing Data', value = 0, {

            inFile <- input$file1
            if (is.null(inFile)) return(NULL)
            temp = inFile$name
            temp <- gsub(".csv", "", temp)
            id.seq <- seq(1, 2048,1)
            
            n <- length(temp)*id.seq

myfiles.x = pblapply(inFile$datapath, read_csv_filename_x)



 myfiles.y = pblapply(inFile$datapath, read_csv_filename_y)

            


            xrf.x <- data.frame(id.seq, myfiles.x)
            colnames(xrf.x) <- c("ID", temp)
            xrf.y <- data.frame(id.seq, myfiles.y)
            colnames(xrf.y) <- c("ID", temp)
           
           
           xrf.x <- data.table(xrf.x)
           xrf.y <- data.table(xrf.y)
           
           
           energy.m <- xrf.x[, list(variable = names(.SD), value = unlist(.SD, use.names = F)), by = ID]
           cps.m <- xrf.y[, list(variable = names(.SD), value = unlist(.SD, use.names = F)), by = ID]
           

            spectra.frame <- data.frame(energy.m$value, cps.m$value, cps.m$variable)
            colnames(spectra.frame) <- c("Energy", "CPS", "Spectrum")
            data <- spectra.frame
            
            
            incProgress(1/n)
            Sys.sleep(0.1)
            })

     data
     
        })


        output$contents <- renderTable({
            
            

           myData()
          
        })
        
        # Return the requested dataset
        datasetInput <- reactive({
            switch(input$element,
            "H.table" = H.table,
            "He.table" = He.table,
            "Li.table" = Li.table,
            "Be.table" = Be.table,
            "B.table" = B.table,
            "C.table" = C.table,
            "N.table" = N.table,
            "O.table" = O.table,
            "F.table" = F.table,
            "Ne.table" = Ne.table,
            "Na.table" = Na.table,
            "Mg.table" = Mg.table,
            "Al.table" = Al.table,
            "Si.table" = Si.table,
            "P.table" = P.table,
            "S.table" = S.table,
            "Cl.table" = Cl.table,
            "Ar.table" = Ar.table,
            "K.table" = K.table,
            "Ca.table" = Ca.table,
            "Sc.table" = Sc.table,
            "Ti.table" = Ti.table,
            "V.table" = V.table,
            "Cr.table" = Cr.table,
            "Mn.table" = Mn.table,
            "Fe.table" = Fe.table,
            "Co.table" = Co.table,
            "Ni.table" = Ni.table,
            "Cu.table" = Cu.table,
            "Zn.table" = Zn.table,
            "Ga.table" = Ga.table,
            "Ge.table" = Ge.table,
            "As.table" = As.table,
            "Se.table" = Se.table,
            "Br.table" = Br.table,
            "Kr.table" = Kr.table,
            "Rb.table" = Rb.table,
            "Sr.table" = Sr.table,
            "Y.table" = Y.table,
            "Zr.table" = Zr.table,
            "Nb.table" = Nb.table,
            "Mo.table" = Mo.table,
            "Tc.table" = Tc.table,
            "Ru.table" = Ru.table,
            "Rh.table" = Rh.table,
            "Pd.table" = Pd.table,
            "Ag.table" = Ag.table,
            "Cd.table" = Cd.table,
            "In.table" = In.table,
            "Sn.table" = Sn.table,
            "Sb.table" = Sb.table,
            "Te.table" = Te.table,
            "I.table" = I.table,
            "Xe.table" = Xe.table,
            "Cs.table" = Cs.table,
            "Ba.table" = Ba.table,
            "La.table" = La.table,
            "Ce.table" = Ce.table,
            "Pr.table" = Pr.table,
            "Nd.table" = Nd.table,
            "Pm.table" = Pm.table,
            "Sm.table" = Sm.table,
            "Eu.table" = Eu.table,
            "Gd.table" = Gd.table,
            "Tb.table" = Tb.table,
            "Dy.table" = Dy.table,
            "Ho.table" = Ho.table,
            "Er.table" = Er.table,
            "Tm.table" = Tm.table,
            "Yb.table" = Yb.table,
            "Lu.table" = Lu.table,
            "Hf.table" = Hf.table,
            "Ta.table" = Ta.table,
            "W.table" = W.table,
            "Re.table" = Re.table,
            "Os.table" = Os.table,
            "Ir.table" = Ir.table,
            "Pt.table" = Pt.table,
            "Au.table" = Au.table,
            "Hg.table" = Hg.table,
            "Tl.table" = Tl.table,
            "Pb.table" = Pb.table,
            "Bi.table" = Bi.table,
            "Po.table" = Po.table,
            "At.table" = At.table,
            "Rn.table" = Rn.table,
            "Fr.table" = Fr.table,
            "Ra.table" = Ra.table,
            "Ac.table" = Ac.table,
            "Th.table" = Th.table,
            "Pa.table" = Pa.table,
            "U.table" = U.table)
        })
        
         observeEvent(input$actionplot, {
        
        # Expression that generates a histogram. The expression is
        # wrapped in a call to renderPlot to indicate that:
        #
        #  1) It is "reactive" and therefore should re-execute automatically
        #     when inputs change
        #  2) Its output type is a plot
        ranges <- reactiveValues(x = NULL, y = NULL)
        
        
        
         plotInput <- reactive({
             


             data <- myData()
             id.seq <- seq(1, 2048,1)
             
             n <- length(data$Energy)
             
             element <- datasetInput()
             intensity.norm <- (element$Intensity/max(element$Intensity))*max(data$CPS)
             intensity.base <- (element$Intensity/max(element$Intensity))
             
             
             
             spectral.plot <- qplot(data$Energy, data$CPS, xlab = "Energy (keV)", ylab = "Counts per Second", geom="line", colour=data$Spectrum) +
             theme_light()+
             theme(legend.position="bottom") +
             geom_segment(aes(x=element$Line, xend=element$Line, y = 0, yend=intensity.norm), colour="grey50", linetype=2)  +
             scale_colour_discrete("Spectrum") +
             coord_cartesian(xlim = ranges$x, ylim = ranges$y)
             
       

         })


        output$distPlot <- renderPlot({

print(plotInput())


        })
        
        # When a double-click happens, check if there's a brush on the plot.
        # If so, zoom to the brush bounds; if not, reset the zoom.
        observeEvent(input$plot1_dblclick, {
            brush <- input$plot1_brush
            if (!is.null(brush)) {
                ranges$x <- c(brush$xmin*mean(data$Energy), brush$xmax*max(data$Energy))
                ranges$y <- c(brush$ymin*mean(data$CPS), brush$ymax*max(data$CPS))
                
            } else {
                ranges$x <- NULL
                ranges$y <- NULL
            }
            
            
            
        })
        
        output$downloadPlot <- downloadHandler(
        filename = function() { paste(input$dataset, '.png', sep='') },
        content = function(file) {
            ggsave(file,plotInput(), width=10, height=7)
        }
        )
        
        
        

        
      
   
    
         })
         
         
 data <- myData()
 
 
 spectra.line.table <- spectra.line.fn(data)
 select.line.table <- datatable(spectra.line.table[, input$show_vars, drop = FALSE])
 select.line.table
 
  
 fordownload <- spectra.line.table[input$show_vars]
 
 tableInput <- reactive({
     spectra.line.table <- spectra.line.fn(data)
     select.line.table <- datatable(spectra.line.table[, input$show_vars, drop = FALSE])
     select.line.table
 })


  output$mytable1 <- renderDataTable({
   
  tableInput()

  })
  

  
  output$downloadData <- downloadHandler(
  filename = function() { paste(input$dataset, '.csv', sep=',') },
  content = function(file
  ) {
      write.csv(tableInput(), file)
  }
  )
  
  
  #####PCA Analysis
  
  plotInput2 <- reactive({
      

  xrf.pca.header <- input$show_vars
  xrf.pca.frame <- spectra.line.table[input$show_vars]
  xrf.pca.n <- length(xrf.pca.frame)
  xrf.smalls <- xrf.pca.frame[2:xrf.pca.n]
  
  xrf.k <- kmeans(xrf.smalls, input$knum, iter.max=1000, nstart=15, algorithm=c("Hartigan-Wong"))
  xrf.pca <- prcomp(xrf.smalls, scale.=FALSE)
  xrf.scores <- as.data.frame(xrf.pca$x)
  xrf.pca.results <- data.frame(spectra.line.table, xrf.k$cluster, xrf.scores)
  
  xrf.pca.header <- input$show_vars
  xrf.pca.frame <- spectra.line.table[input$show_vars]
  xrf.pca.n <- length(xrf.pca.frame)
  xrf.smalls <- xrf.pca.frame[2:xrf.pca.n]
  
  xrf.k <- kmeans(xrf.smalls, input$knum, iter.max=1000, nstart=15, algorithm=c("Hartigan-Wong"))
  xrf.pca <- prcomp(xrf.smalls, scale.=FALSE)
  xrf.scores <- as.data.frame(xrf.pca$x)
  xrf.pca.results <- data.frame(spectra.line.table, xrf.k$cluster, xrf.scores)
  
  
  
  
  regular <- ggplot(data= xrf.pca.results) +
  geom_point(aes(PC1, PC2, colour=xrf.k.cluster), size = input$spotsize) +
  scale_x_continuous("Principle Component 1") +
  scale_y_continuous("Principle Component 2") +
  theme_light() +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.title.x = element_text(size=15)) +
  theme(axis.title.y = element_text(size=15, angle=90)) +
  theme(legend.position="none") +
  theme(plot.title=element_text(size=20)) +
  theme(legend.title=element_text(size=15)) +
  theme(legend.text=element_text(size=15)) +
  guides(colour=guide_legend(title="K-Means")) +
  scale_colour_gradientn(colours=rainbow(7))
  
  ellipse <- ggplot(data= xrf.pca.results)+
  geom_point(aes(PC1, PC2, colour=xrf.k.cluster), size = input$spotsize) +
  scale_x_continuous("Principle Component 1") +
  scale_y_continuous("Principle Component 2") +
  theme_light() +
  stat_ellipse(aes(PC1, PC2, colour=xrf.k.cluster, linetype=as.factor(xrf.k.cluster))) +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.title.x = element_text(size=15)) +
  theme(axis.title.y = element_text(size=15, angle=90)) +
  theme(legend.position="none") +
  theme(plot.title=element_text(size=20)) +
  theme(legend.title=element_text(size=15)) +
  theme(legend.text=element_text(size=15)) +
  guides(colour=guide_legend(title="K-Means")) +
  scale_colour_gradientn(colours=rainbow(7)) +
  guides(linetype=FALSE)
  
  if (input$elipseplot1 == FALSE) {
      regular
  } else if (input$elipseplot1 == TRUE) {
      ellipse
  }


  })
  
  
  output$xrfpcaplot <- renderPlot({
      print(plotInput2())
      
  })
  
  
  output$downloadPlot2 <- downloadHandler(
  filename = function() { paste(input$dataset, '.png', sep='') },
  content = function(file) {
      ggsave(file,plotInput2(), width=10, height=7)
  }
  )
  
  
  
  pcaTableInput <- reactive({
      
      
      xrf.pca.header <- input$show_vars
      xrf.pca.frame <- spectra.line.table[input$show_vars]
      xrf.pca.n <- length(xrf.pca.frame)
      xrf.smalls <- xrf.pca.frame[2:xrf.pca.n]
      
      xrf.k <- kmeans(xrf.smalls, input$knum, iter.max=1000, nstart=15, algorithm=c("Hartigan-Wong"))
      xrf.pca <- prcomp(xrf.smalls, scale.=FALSE)
      xrf.scores <- as.data.frame(xrf.pca$x)
      xrf.pca.results <- data.frame(spectra.line.table, xrf.k$cluster, xrf.scores)
      
      xrf.pca.header <- input$show_vars
      xrf.pca.frame <- spectra.line.table[input$show_vars]
      xrf.pca.n <- length(xrf.pca.frame)
      xrf.smalls <- xrf.pca.frame[2:xrf.pca.n]
      
      xrf.k <- kmeans(xrf.smalls, input$knum, iter.max=1000, nstart=15, algorithm=c("Hartigan-Wong"))
      xrf.pca <- prcomp(xrf.smalls, scale.=FALSE)
      xrf.scores <- as.data.frame(xrf.pca$x)
      xrf.pca.results <- data.frame(xrf.k$cluster, xrf.scores, spectra.line.table[input$show_vars])
      names(xrf.pca.results)[names(xrf.pca.results)=="xrf.k.cluster"] <- "K-means"
      return(xrf.pca.results)
 
      
  })



output$xrfpcatable <- renderTable({
    pcaTableInput()
    
})




output$downloadPcaTable <- downloadHandler(
filename = function() { paste(input$dataset, '.csv', sep=',') },
content = function(file
) {
    write.csv(pcaTableInput(), file)
}
)






plotInput3a <- reactive({


   xrf.pca.header <- input$show_vars
   xrf.pca.frame <- spectra.line.table[input$show_vars]
   xrf.pca.n <- length(xrf.pca.frame)
   xrf.smalls <- xrf.pca.frame[2:xrf.pca.n]
   
   xrf.k <- kmeans(xrf.smalls, input$knum, iter.max=1000, nstart=15, algorithm=c("Hartigan-Wong"))
   
   
   
   unique.spec <- seq(1, length(spectra.line.table$Spectrum), 1)
   null <- rep(1, length(unique.spec))


   
   spectra.line.table.norm <- data.frame(spectra.line.table, null)
   colnames(spectra.line.table.norm) <- c(names(spectra.line.table), "None")
   spectra.line.table.norm

   interval <- unique.spec*as.numeric(input$intervalmm)

   spectra.timeseries.table <- data.frame(interval, spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], xrf.k$cluster)
   colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster")
   
   
  trendy <-  as.vector((if(input$elementnorm=="None") {
       paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " Counts per Second")), sep=",", collapse="")
      } else {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), "/", substr(input$elementnorm, 1, 2))), sep=",", collapse="")
      }))
      
      
      

  




   
   
 

black.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="line", data = spectra.timeseries.table) +
geom_line(colour = "black", lwd=input$linesize) +
theme_light()

smooth.time.series <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="point") +
theme_light() +
stat_smooth()

ramp.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="line", data = spectra.timeseries.table) +
geom_line(aes(colour = Selected), lwd=input$linesize) +
theme_light() +
scale_colour_gradientn(colours=rainbow(7))

area.time.series <- ggplot(spectra.timeseries.table, aes(Interval)) +
theme_classic() +
geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
scale_x_continuous("Length (mm)") +
scale_y_continuous(trendy)


cluster.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="line", data = spectra.timeseries.table) +
geom_line(aes(colour = Cluster), lwd=input$linesize) +
theme_light() +
scale_colour_gradientn(colours=rainbow(7))



if (input$timecolour == "Black") {
    black.time.series
} else if (input$timecolour == "Smooth") {
    smooth.time.series
} else if (input$timecolour == "Selected") {
    ramp.time.series
} else if (input$timecolour == "Cluster") {
    cluster.time.series
} else if (input$timecolour == "Area") {
    area.time.series
}




})


observeEvent(input$timeseriesact1, {
    
    })

  output$timeseriesplot1 <- renderPlot({
      input$timeseriesact1
      isolate(print(plotInput3a()))
      
  })


  output$downloadPlot3a <- downloadHandler(
  filename = function() { paste(input$dataset, '.png', sep='') },
  content = function(file) {
      ggsave(file,plotInput3a(), width=10, height=7)
  }
  )
  


  plotInput3b <- reactive({
      
      
      xrf.pca.header <- input$show_vars
      xrf.pca.frame <- spectra.line.table[input$show_vars]
      xrf.pca.n <- length(xrf.pca.frame)
      xrf.smalls <- xrf.pca.frame[2:xrf.pca.n]
      
      xrf.k <- kmeans(xrf.smalls, input$knum, iter.max=1000, nstart=15, algorithm=c("Hartigan-Wong"))
      
      
      
      unique.spec <- seq(1, length(spectra.line.table$Spectrum), 1)
      null <- rep(1, length(unique.spec))
      
      
      
      spectra.line.table.norm <- data.frame(spectra.line.table, null)
      colnames(spectra.line.table.norm) <- c(names(spectra.line.table), "None")
      spectra.line.table.norm
      
      interval <- unique.spec*as.numeric(input$intervalmm)
      
      spectra.timeseries.table <- data.frame(interval, spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], xrf.k$cluster)
      colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster")
      
      
      trendy <-  as.vector((if(input$elementnorm=="None") {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " Counts per Second")), sep=",", collapse="")
      } else {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), "/", substr(input$elementnorm, 1, 2))), sep=",", collapse="")
      }))
      
      
      
      
      
      
      
      
      
      
      
      
      
      black.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="line", data = spectra.timeseries.table) +
      geom_line(colour = "black", lwd=input$linesize) +
      theme_light()
      
      smooth.time.series <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="point") +
      theme_light() +
      stat_smooth()
      
      ramp.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Selected), lwd=input$linesize) +
      theme_light() +
      scale_colour_gradientn(colours=rainbow(7))
      
      area.time.series <- ggplot(spectra.timeseries.table, aes(Interval)) +
      theme_classic() +
      geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
      scale_x_continuous("Length (mm)") +
      scale_y_continuous(trendy)
      
      
      cluster.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Cluster), lwd=input$linesize) +
      theme_light() +
      scale_colour_gradientn(colours=rainbow(7))
      
      
      
      if (input$timecolour == "Black") {
          black.time.series
      } else if (input$timecolour == "Smooth") {
          smooth.time.series
      } else if (input$timecolour == "Selected") {
          ramp.time.series
      } else if (input$timecolour == "Cluster") {
          cluster.time.series
      } else if (input$timecolour == "Area") {
          area.time.series
      }
      
      
      
      
  })
 
 
 observeEvent(input$timeseriesact2, {


})
  output$timeseriesplot2 <- renderPlot({
      input$timeseriesact2
      isolate(print(plotInput3b()))
      
  })
  
  
  output$downloadPlot3b <- downloadHandler(
  filename = function() { paste(input$dataset, '.png', sep='') },
  content = function(file) {
      ggsave(file,plotInput3b(), width=10, height=7)
  }
  )
  


  plotInput3c <- reactive({
      
      
      xrf.pca.header <- input$show_vars
      xrf.pca.frame <- spectra.line.table[input$show_vars]
      xrf.pca.n <- length(xrf.pca.frame)
      xrf.smalls <- xrf.pca.frame[2:xrf.pca.n]
      
      xrf.k <- kmeans(xrf.smalls, input$knum, iter.max=1000, nstart=15, algorithm=c("Hartigan-Wong"))
      
      
      
      unique.spec <- seq(1, length(spectra.line.table$Spectrum), 1)
      null <- rep(1, length(unique.spec))
      
      
      
      spectra.line.table.norm <- data.frame(spectra.line.table, null)
      colnames(spectra.line.table.norm) <- c(names(spectra.line.table), "None")
      spectra.line.table.norm
      
      interval <- unique.spec*as.numeric(input$intervalmm)
      
      spectra.timeseries.table <- data.frame(interval, spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], xrf.k$cluster)
      colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster")
      
      
      trendy <-  as.vector((if(input$elementnorm=="None") {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " Counts per Second")), sep=",", collapse="")
      } else {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), "/", substr(input$elementnorm, 1, 2))), sep=",", collapse="")
      }))
      
      
      
      
      
      
      
      
      
      
      
      
      
      black.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="line", data = spectra.timeseries.table) +
      geom_line(colour = "black", lwd=input$linesize) +
      theme_light()
      
      smooth.time.series <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="point") +
      theme_light() +
      stat_smooth()
      
      ramp.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Selected), lwd=input$linesize) +
      theme_light() +
      scale_colour_gradientn(colours=rainbow(7))
      
      area.time.series <- ggplot(spectra.timeseries.table, aes(Interval)) +
      theme_classic() +
      geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
      scale_x_continuous("Length (mm)") +
      scale_y_continuous(trendy)
      
      
      cluster.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Cluster), lwd=input$linesize) +
      theme_light() +
      scale_colour_gradientn(colours=rainbow(7))
      
      
      
      if (input$timecolour == "Black") {
          black.time.series
      } else if (input$timecolour == "Smooth") {
          smooth.time.series
      } else if (input$timecolour == "Selected") {
          ramp.time.series
      } else if (input$timecolour == "Cluster") {
          cluster.time.series
      } else if (input$timecolour == "Area") {
          area.time.series
      }
      
      
      
      
  })
  
  observeEvent(input$timeseriesact3, {
 })
  
  output$timeseriesplot3 <- renderPlot({
      input$timeseriesact3

      isolate(print(plotInput3c()))
      
 
  })
  
  output$downloadPlot3c <- downloadHandler(
  filename = function() { paste(input$dataset, '.png', sep='') },
  content = function(file) {
      ggsave(file,plotInput3c(), width=10, height=7)
  }
  )
  

  plotInput3d <- reactive({
      
      
      xrf.pca.header <- input$show_vars
      xrf.pca.frame <- spectra.line.table[input$show_vars]
      xrf.pca.n <- length(xrf.pca.frame)
      xrf.smalls <- xrf.pca.frame[2:xrf.pca.n]
      
      xrf.k <- kmeans(xrf.smalls, input$knum, iter.max=1000, nstart=15, algorithm=c("Hartigan-Wong"))
      
      
      
      unique.spec <- seq(1, length(spectra.line.table$Spectrum), 1)
      null <- rep(1, length(unique.spec))
      
      
      
      spectra.line.table.norm <- data.frame(spectra.line.table, null)
      colnames(spectra.line.table.norm) <- c(names(spectra.line.table), "None")
      spectra.line.table.norm
      
      interval <- unique.spec*as.numeric(input$intervalmm)
      
      spectra.timeseries.table <- data.frame(interval, spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], xrf.k$cluster)
      colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster")
      
      
      trendy <-  as.vector((if(input$elementnorm=="None") {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " Counts per Second")), sep=",", collapse="")
      } else {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), "/", substr(input$elementnorm, 1, 2))), sep=",", collapse="")
      }))
      
      
      
      
      
      
      
      
      
      
      
      
      
      black.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="line", data = spectra.timeseries.table) +
      geom_line(colour = "black", lwd=input$linesize) +
      theme_light()
      
      smooth.time.series <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="point") +
      theme_light() +
      stat_smooth()
      
      ramp.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Selected), lwd=input$linesize) +
      theme_light() +
      scale_colour_gradientn(colours=rainbow(7))
      
      area.time.series <- ggplot(spectra.timeseries.table, aes(Interval)) +
      theme_classic() +
      geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
      scale_x_continuous("Length (mm)") +
      scale_y_continuous(trendy)
      
      
      cluster.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Cluster), lwd=input$linesize) +
      theme_light() +
      scale_colour_gradientn(colours=rainbow(7))
      
      
      
      if (input$timecolour == "Black") {
          black.time.series
      } else if (input$timecolour == "Smooth") {
          smooth.time.series
      } else if (input$timecolour == "Selected") {
          ramp.time.series
      } else if (input$timecolour == "Cluster") {
          cluster.time.series
      } else if (input$timecolour == "Area") {
          area.time.series
      }
      
      
      
      
  })
  
  observeEvent(input$timeseriesact4, {
      
  })



  output$timeseriesplot4 <- renderPlot({
      input$timeseriesact4
      isolate(print(plotInput3d()))
      
  })
  
  output$downloadPlot3d <- downloadHandler(
  filename = function() { paste(input$dataset, '.png', sep='') },
  content = function(file) {
      ggsave(file,plotInput3d(), width=10, height=7)
  }
  )
  
  
      


  plotInput3e <- reactive({
      
      
      xrf.pca.header <- input$show_vars
      xrf.pca.frame <- spectra.line.table[input$show_vars]
      xrf.pca.n <- length(xrf.pca.frame)
      xrf.smalls <- xrf.pca.frame[2:xrf.pca.n]
      
      xrf.k <- kmeans(xrf.smalls, input$knum, iter.max=1000, nstart=15, algorithm=c("Hartigan-Wong"))
      
      
      
      unique.spec <- seq(1, length(spectra.line.table$Spectrum), 1)
      null <- rep(1, length(unique.spec))
      
      
      
      spectra.line.table.norm <- data.frame(spectra.line.table, null)
      colnames(spectra.line.table.norm) <- c(names(spectra.line.table), "None")
      spectra.line.table.norm
      
      interval <- unique.spec*as.numeric(input$intervalmm)
      
      spectra.timeseries.table <- data.frame(interval, spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], xrf.k$cluster)
      colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster")
      
      
      trendy <-  as.vector((if(input$elementnorm=="None") {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " Counts per Second")), sep=",", collapse="")
      } else {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), "/", substr(input$elementnorm, 1, 2))), sep=",", collapse="")
      }))
      
      
      
      
      
      
      
      
      
      
      
      
      
      black.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="line", data = spectra.timeseries.table) +
      geom_line(colour = "black", lwd=input$linesize) +
      theme_light()
      
      smooth.time.series <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="point") +
      theme_light() +
      stat_smooth()
      
      ramp.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Selected), lwd=input$linesize) +
      theme_light() +
      scale_colour_gradientn(colours=rainbow(7))
      
      area.time.series <- ggplot(spectra.timeseries.table, aes(Interval)) +
      theme_classic() +
      geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
      scale_x_continuous("Length (mm)") +
      scale_y_continuous(trendy)
      
      
      cluster.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = "Length (mm)", ylab = trendy, geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Cluster), lwd=input$linesize) +
      theme_light() +
      scale_colour_gradientn(colours=rainbow(7))
      
      
      
      if (input$timecolour == "Black") {
          black.time.series
      } else if (input$timecolour == "Smooth") {
          smooth.time.series
      } else if (input$timecolour == "Selected") {
          ramp.time.series
      } else if (input$timecolour == "Cluster") {
          cluster.time.series
      } else if (input$timecolour == "Area") {
          area.time.series
      }
      
      
      
      
  })
  
  observeEvent(input$timeseriesact5, {


})

  output$timeseriesplot5 <- renderPlot({
      input$timeseriesact5
      
      
      
      isolate(print(plotInput3e()))
      
  })
  
  
  output$downloadPlot3e <- downloadHandler(
  filename = function() { paste(input$dataset, '.png', sep='') },
  content = function(file) {
      ggsave(file,plotInput3e(), width=10, height=7)
  }
  )
   
  
  plotInput4 <- reactive({
      

      xrf.pca.header <- input$show_vars
      xrf.pca.frame <- spectra.line.table[input$show_vars]
      xrf.pca.n <- length(xrf.pca.frame)
      xrf.smalls <- xrf.pca.frame[2:xrf.pca.n]
      
      xrf.k <- kmeans(xrf.smalls, input$knum, iter.max=1000, nstart=15, algorithm=c("Hartigan-Wong"))
      
      first.ratio <-spectra.line.table[input$elementratioa]
      second.ratio <- spectra.line.table[input$elementratiob]
      third.ratio <- spectra.line.table[input$elementratioc]
      fourth.ratio <- spectra.line.table[input$elementratiod]
      
      
      ratio.frame <- data.frame(first.ratio, second.ratio, third.ratio, fourth.ratio, xrf.k$cluster)
      colnames(ratio.frame) <- gsub("[.]", "", c(substr(input$elementratioa, 1, 2), substr(input$elementratiob, 1, 2), substr(input$elementratioc, 1, 2), substr(input$elementratiod, 1, 2), "Cluster"))
      
      ratio.names.x <- c(names(ratio.frame[1]), "/", names(ratio.frame[2]))
      ratio.names.y <- c(names(ratio.frame[3]), "/", names(ratio.frame[4]))
      
      ratio.names.x <- paste(ratio.names.x, sep=",", collapse="")
      ratio.names.y <- paste(ratio.names.y, sep=",", collapse="")
      
      
      
      
      black.ratio.plot <- qplot(ratio.frame[,1]/ratio.frame[,2], ratio.frame[,3]/ratio.frame[,4], xlab = ratio.names.x, ylab = ratio.names.y ) +
      geom_point(lwd=input$spotsize2) +
      theme_light()
      
      cluster.ratio.plot <- qplot(ratio.frame[,1]/ratio.frame[,2], ratio.frame[,3]/ratio.frame[,4], xlab = ratio.names.x, ylab = ratio.names.y ) +
      geom_point(aes(colour=ratio.frame$Cluster), lwd=input$spotsize2) +
      theme_light() +
      scale_colour_gradientn(colours=rainbow(7)) +
      theme(legend.position="none")
      
      cluster.ratio.ellipse.plot <- qplot(ratio.frame[,1]/ratio.frame[,2], ratio.frame[,3]/ratio.frame[,4], xlab = ratio.names.x, ylab = ratio.names.y ) +
      stat_ellipse(aes(ratio.frame[,1]/ratio.frame[,2], ratio.frame[,3]/ratio.frame[,4], colour=ratio.frame$Cluster, linetype=as.factor(ratio.frame$Cluster))) +
      geom_point(aes(colour=ratio.frame$Cluster), lwd=input$spotsize2) +
      theme_light() +
      scale_colour_gradientn(colours=rainbow(7)) +
      theme(legend.position="none")+
      guides(linetype=FALSE)
      
      


      
      
      if (input$ratiocolour == "Black") {
          black.ratio.plot
      } else if (input$ratiocolour == "Cluster" && input$elipseplot2==FALSE) {
          cluster.ratio.plot
      } else if (input$ratiocolour == "Cluster" && input$elipseplot2==TRUE) {
          cluster.ratio.ellipse.plot
      }
  })


   output$elementratiotimeseries <- renderPlot({
       print(plotInput4())


    })
   
   output$downloadPlot4 <- downloadHandler(
   filename = function() { paste(input$dataset, '.png', sep='') },
   content = function(file) {
       ggsave(file,plotInput4(), width=10, height=7)
   }
   )






plotInput5 <- reactive({
    
    xrf.pca.header <- input$show_vars
    xrf.pca.frame <- spectra.line.table[input$show_vars]
    xrf.pca.n <- length(xrf.pca.frame)
    xrf.smalls <- xrf.pca.frame[2:xrf.pca.n]
    
    xrf.k <- kmeans(xrf.smalls, input$knum, iter.max=1000, nstart=15, algorithm=c("Hartigan-Wong"))
    
    first.axis <- spectra.line.table[input$axisa]
    second.axis <- spectra.line.table[input$axisb]
    third.axis <- spectra.line.table[input$axisc]
    
    axis.frame <- data.frame(first.axis, second.axis, third.axis, xrf.k$cluster)
    colnames(axis.frame) <- gsub("[.]", "", c(substr(input$axisa, 1, 2), substr(input$axisb, 1, 2), substr(input$axisc, 1, 2), "Cluster"))
    
    ternaryplot1 <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
    geom_point() +
    theme_light()
    
    ternaryplot2 <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
    geom_density_tern() +
    geom_point() +
    theme_light()
    
    ternaryplotcluster <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
    geom_point(aes_string(colour = colnames(axis.frame)[4])) +
    scale_colour_gradientn(colours=rainbow(7)) +
    theme(legend.position="none") +
    theme_light()
    
    ternaryplotclusterellipse <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
    geom_density_tern() +
    geom_point(aes_string(colour = colnames(axis.frame)[4])) +
    scale_colour_gradientn(colours=rainbow(7)) +
    theme_light()

    if (input$ternarycolour == "Black" && input$terndensityplot==FALSE) {
        ternaryplot1
    } else if (input$ternarycolour == "Black" && input$terndensityplot==TRUE) {
        ternaryplot2
    } else if (input$ternarycolour == "Cluster" && input$terndensityplot==FALSE) {
        ternaryplotcluster
    } else if (input$ternarycolour == "Cluster" && input$terndensityplot==TRUE) {
        ternaryplotclusterellipse
    }


})

output$ternaryplot <- renderPlot({
    
    print(plotInput5())
    
})

output$downloadPlot5 <- downloadHandler(
filename = function() { paste(input$dataset, '.png', sep='') },
content = function(file) {
    ggsave(file,plotInput5(), width=10, height=7)
}
)



})

})










