library(shiny)
library(bslib)
library(igraph)

prepare_graph <- function() {
  cat("Loading data...\n")
  dfGraph <- read.csv2("https://bergplace.org/share/out.radoslaw_email_email", 
                       skip=2, sep=" ", header=FALSE)[, 1:2]
  names(dfGraph) <- c("from", "to")
  
  # Create directed graph
  g <- graph.data.frame(dfGraph, directed=TRUE)
  g <- simplify(g)
  
  cat("Nodes:", vcount(g), "Edges:", ecount(g), "\n")
  
  cat("Calculating edge weights...\n")
  
  edge_counts <- as.data.frame(table(dfGraph$from, dfGraph$to))
  names(edge_counts) <- c("from", "to", "count")
  edge_counts <- edge_counts[edge_counts$count > 0, ]
  
  total_sent <- aggregate(count ~ from, data=edge_counts, sum)
  names(total_sent) <- c("from", "total")
  
  # Calculate weights
  edge_counts <- merge(edge_counts, total_sent, by="from")
  edge_counts$weight <- edge_counts$count / edge_counts$total
  
  edge_list <- as_edgelist(g, names=TRUE)
  edge_keys_graph <- paste(edge_list[,1], edge_list[,2], sep="_")
  edge_keys_counts <- paste(edge_counts$from, edge_counts$to, sep="_")
  
  weight_lookup <- setNames(edge_counts$weight, edge_keys_counts)
  E(g)$weight <- weight_lookup[edge_keys_graph]
  E(g)$weight[is.na(E(g)$weight)] <- 0.001
  
  cat("Pre-caching neighbor structure...\n")
  n <- vcount(g)
  neighbor_cache <- vector("list", n)
  weight_cache <- vector("list", n)
  
  all_edges <- as_edgelist(g, names=FALSE)
  all_weights <- E(g)$weight
  
  for (i in 1:n) {
    outgoing_indices <- which(all_edges[, 1] == i)
    
    if (length(outgoing_indices) > 0) {
      neighbor_cache[[i]] <- all_edges[outgoing_indices, 2]
      weight_cache[[i]] <- all_weights[outgoing_indices]
    } else {
      neighbor_cache[[i]] <- integer(0)
      weight_cache[[i]] <- numeric(0)
    }
  }
  
  g$neighbor_cache <- neighbor_cache
  g$weight_cache <- weight_cache
  
  cat("Graph preparation complete!\n")
  return(g)
}


simulate_cascade <- function(g, initial_nodes, weight_multiplier=1.0, max_iterations=50) {
  n <- vcount(g)
  
  activated <- rep(FALSE, n)
  activated[initial_nodes] <- TRUE
  
  activation_counts <- c(length(initial_nodes))
  newly_activated <- initial_nodes
  
  tried <- new.env(hash=TRUE, size=n*10)

  neighbor_cache <- g$neighbor_cache
  weight_cache <- g$weight_cache
  
  iteration <- 1
  
  while(length(newly_activated) > 0 && iteration <= max_iterations) {
    next_activated <- integer(0)
    
    for(node in newly_activated) {

      neighs <- neighbor_cache[[node]]
      weights <- weight_cache[[node]]
      
      if(length(neighs) == 0) next
      
      for(idx in seq_along(neighs)) {
        neighbor_id <- neighs[idx]
        
        if(activated[neighbor_id]) next
        
        key <- paste0(node, "_", neighbor_id)
        if(exists(key, envir=tried, inherits=FALSE)) next
        
        assign(key, TRUE, envir=tried)
        
        prob <- weights[idx] * weight_multiplier
        prob <- min(prob, 1.0)
        
        if(runif(1) <= prob) {
          activated[neighbor_id] <- TRUE
          next_activated <- c(next_activated, neighbor_id)
        }
      }
    }
    
    if(length(next_activated) > 0) {
      activation_counts <- c(activation_counts, length(next_activated))
      newly_activated <- next_activated[!duplicated(next_activated)]
    } else {
      newly_activated <- integer(0)
    }
    
    iteration <- iteration + 1
  }
  
  return(activation_counts)
}


select_initial_nodes <- function(g, strategy, percentage=0.05) {
  n_nodes <- ceiling(vcount(g) * percentage)
  
  if(strategy == "outdegree") {
    degrees <- degree(g, mode="out")
    return(order(degrees, decreasing=TRUE)[1:n_nodes])
    
  } else if(strategy == "betweenness") {
    btw <- betweenness(g, directed=TRUE)
    return(order(btw, decreasing=TRUE)[1:n_nodes])
    
  } else if(strategy == "closeness") {
    clo <- closeness(g, mode="out")
    clo[is.infinite(clo)] <- 0  # Handle isolated nodes
    return(order(clo, decreasing=TRUE)[1:n_nodes])
    
  } else if(strategy == "random") {
    return(sample(1:vcount(g), n_nodes))
    
  } else if(strategy == "pagerank") {
    pr <- page.rank(g, directed=TRUE)$vector
    return(order(pr, decreasing=TRUE)[1:n_nodes])
  }
}


run_experiment <- function(g, strategy, weight_multiplier=1.0, max_iterations=50, n_runs=100) {
  all_results <- matrix(0, nrow=n_runs, ncol=max_iterations+1)
  
  for(run in 1:n_runs) {
    initial_nodes <- select_initial_nodes(g, strategy)
    counts <- simulate_cascade(g, initial_nodes, weight_multiplier, max_iterations)
    
    len <- min(length(counts), max_iterations+1)
    all_results[run, 1:len] <- counts[1:len]
  }
  
  avg_counts <- colMeans(all_results)
  
  return(avg_counts)
}


ui <- page_sidebar(
  title = "Information Diffusion - Independent Cascade Model",
  
  sidebar = sidebar(
    sliderInput(
      inputId = "weight_mult",
      label = "Activation probability multiplier:",
      min = 10,
      max = 200,
      value = 100,
      step = 10,
      post = "%"
    ),
    
    sliderInput(
      inputId = "max_iter",
      label = "Maximum iterations:",
      min = 1,
      max = 50,
      value = 10,
      step = 1
    ),
    
    actionButton(
      inputId = "run_sim",
      label = "Run Simulation",
      class = "btn-primary",
      width = "100%"
    ),
    
    HTML("<hr>"),
    HTML("<h5>Strategies:</h5>"),
    HTML("<ul>
      <li><b>Out-degree:</b> Most outgoing connections</li>
      <li><b>Betweenness:</b> Bridge nodes</li>
      <li><b>Closeness:</b> Central nodes</li>
      <li><b>Random:</b> Baseline</li>
      <li><b>PageRank:</b> Importance score</li>
    </ul>"),
    HTML("<p><small>167 nodes, 5783 edges<br>
         Initial: 5% (9 nodes)<br>
         Runs per strategy: 100 (averaged)</small></p>")
  ),
  
  plotOutput(outputId = "diffusionPlot", height = "600px")
)

# ==============================================================================
# PART 6: SHINY APP SERVER
# ==============================================================================

server <- function(input, output) {
  
  g <- prepare_graph()
  
  cat("Ready for simulations!\n")
  
  simulation_results <- eventReactive(input$run_sim, {
    weight_multiplier <- input$weight_mult / 100
    max_iterations <- input$max_iter
    
    withProgress(message = 'Running simulations...', value = 0, {
      strategies <- c("outdegree", "betweenness", "closeness", "random", "pagerank")
      results <- list()
      
      start_time <- Sys.time()
      
      for(i in 1:length(strategies)) {
        incProgress(1/5, detail = paste("Strategy:", strategies[i]))
        results[[i]] <- run_experiment(g, strategies[i], weight_multiplier, 
                                       max_iterations, n_runs=100)
      }
      
      elapsed <- difftime(Sys.time(), start_time, units="secs")
      cat("Simulation completed in", round(elapsed, 2), "seconds\n")
      
      return(list(
        results = results,
        weight_mult = weight_multiplier,
        max_iter = max_iterations,
        elapsed = elapsed
      ))
    })
  })
  
  output$diffusionPlot <- renderPlot({
    sim_data <- simulation_results()
    
    results <- sim_data$results
    weight_multiplier <- sim_data$weight_mult
    max_iterations <- sim_data$max_iter
    
    strategy_names <- c("Out-degree", "Betweenness", "Closeness", "Random", "PageRank")
    colors <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")
    
    plot(0:max_iterations, results[[1]][1:(max_iterations+1)], 
         type="l", col=colors[1], lwd=2,
         xlab="Iteration", ylab="Newly activated nodes",
         main=paste0("Information Diffusion (weight × ", weight_multiplier, 
                     ") - ", round(sim_data$elapsed, 1), "s"),
         ylim=c(0, max(sapply(results, function(x) max(x[1:(max_iterations+1)])))))
    
    for(i in 2:5) {
      lines(0:max_iterations, results[[i]][1:(max_iterations+1)], 
            col=colors[i], lwd=2)
    }
    
    legend("topright", legend=strategy_names, col=colors, lwd=2, cex=0.9)
    grid()
  })
}

shinyApp(ui = ui, server = server)