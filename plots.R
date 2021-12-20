#### PLOT FUNCTIONS FOR THE DIATHOR SHINY PACKAGE ####

#Lolipop Plots

loli.plot <- function(result, ylabel, ylow, yhigh, samplenames, color = "#0073C2"){
  x <- rownames(result)
  data <- result
  data[is.na(data)] = 0

  for(i in 1:ncol(result)) {
    y <- data[,i]

    return(ggplot2::ggplot(data, aes(x=samplenames, y=y)) +
             geom_segment( aes(x=samplenames, xend=samplenames, y=0, yend=y), color="grey") +
             geom_point( color=color, size=4) +
             theme_light() +
             theme(
               panel.grid.major.x = element_blank(),
               panel.border = element_blank(),
               axis.ticks.x = element_blank()
             ) +
             ylim(ylow, yhigh) +
             xlab("") +
             ylab(ylabel)
    )
  }
}

#Percent Plots
percentbarchart.plot <- function(result, title, sampleNames){
  x <- colnames(result) #checks if taxa used colum exists and removes it
  x <- substr(x, nchar(x)-8, nchar(x)) == 'Taxa used' #checks if taxa used colum exists and removes it
  if (tail(x, n=1)==TRUE) {
    result <- result[,-ncol(result)] #and removes it
  }
  sampleCol <- rep(sampleNames, ncol(result)) #gets sample names
  result <- tidyr::gather(result) #uses tidyr to rearrange the dataframe in a single column
  result$sampleCol <- sampleCol #adds another column with the sample names
  colors <- c("#CC1C00", "#5C88DA", "#84BD00", "#FFCD00", "#7C878E", "#E64B35", "#4DBBD5", "#01A087", "#3C5488", "#F39B7F", "#FF410D99", "#6EE2FF99", "#F7C53099", "#95CC5E99", "#D0DFE699", "#F79D1E99", "#748AA699")
  key <- result$key
  value <- result$value
  print(ggplot2::ggplot(result, aes(fill=key, y=value, x=sampleCol)) +
          geom_bar(position="fill", stat="identity") +
          scale_fill_manual(values=colors) +
          xlab("Samples") +
          ylab(title)) #graphs
}

