message('library paths:\n', paste('... ', .libPaths(), sep='', collapse='\n'))
shiny::runApp('./shiny/', launch.browser=TRUE)