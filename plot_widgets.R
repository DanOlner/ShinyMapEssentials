library(ggplot2)
library(plotly)


density_widget <-
  function(data, xVar){
    p <- ggplot(data = data, aes_string(x = xVar)) + geom_density()
    ggplotly(p)
  }
    

## Example

#https://stackoverflow.com/questions/5106782/use-of-ggplot-within-another-function-in-r
# density_widget(la, 'lowest2015')

