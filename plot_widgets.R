library(ggplot2)
library(plotly)

scatter_widget <-
  function(data){
    p <- ggplot(
      data = data, aes_string(x ='di', y = 'frontier_stat', text = 'ttwa11nm', colour = 'frontier_stat')
      ) + 
      geom_point(
      ) 
    ggplotly(p)
  }
    

## Example
#https://stackoverflow.com/questions/5106782/use-of-ggplot-within-another-function-in-r
scatter_widget(ttwa)

