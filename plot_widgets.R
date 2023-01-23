library(ggplot2)
library(plotly)


scatter_widget <-
  function(data){
    p <- ggplot(
      data = data, aes_string(x = 'IMD_rank', y = 'frontier_rank', text = 'NAME')
      ) + 
      geom_point(
        aes(colour = frontier_rank)
      ) 
    ggplotly(p)
  }
    

## Example
#https://stackoverflow.com/questions/5106782/use-of-ggplot-within-another-function-in-r
#scatter_widget(la)

