shinyServer(
  function(input, output, session) {

    source('adaboost_methods.R')
    source('plot_fns.R')

    library(scales)
    set.seed(1)

    data <- simulate_data(800, 200)
    ab <- adaboost_initialise(data, n_it =100)
    ab <- adaboost_one_it(ab)

    autoInvalidate <- reactiveTimer(10, session)

    v <- reactiveValues(val = 'reset') # initially, don't show plots

    classifier <- reactive({ # update plots 
      autoInvalidate()
      if (ab$it < ab$n_it) {
        ab <<- adaboost_one_it(ab)
      }
      return(1)
    })
   
    # button click events
    observeEvent(input$run, { 
      v$val <- 'run'
    })

    observeEvent(input$stop, {
      v$val <- 'stop'
    })

    observeEvent(input$reset, {
      v$val <- 'reset'
    })

    output$example <- renderPlot({
      if (v$val == 'run') {
        val <- classifier()
      } else if (v$val == 'reset') {
        ab <<- adaboost_initialise(data, n_it=100)
        return(NULL)
      }

      par(mfrow=c(2,2), oma=c(1,1,1,1), mar=c(3,3,2,2), mgp=c(2,1,0))
      plot_best_classifier(ab)
      plot_errors(ab)
      plot_weak_classifier(ab)
      plot_point_weights(ab)

    }, width=600, height=600)
})


