library(ggplot2)
library(plotly)


scatter_widget <-
  function(data, x, y, id_text){
    p <- ggplot(
      data = data, aes_string(x = x, y = y, text = id_text)
      ) + 
      geom_point(
        aes_string(colour = y)
      ) 
    ggplotly(p)
  }
    

## Example
#https://stackoverflow.com/questions/5106782/use-of-ggplot-within-another-function-in-r
#scatter_widget(la)

