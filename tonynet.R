tonynet = function (NetMatrix, normalize = NULL, n = NULL, degree = NULL, 
          Title = "Plot", type = "auto", label = TRUE, labelsize = 1, 
          label.cex = FALSE, label.color = FALSE, label.n = NULL, halo = FALSE, 
          cluster = "walktrap", community.repulsion = 0.1, vos.path = NULL, 
          size = 3, size.cex = FALSE, curved = FALSE, noloops = TRUE, 
          remove.multiple = TRUE, remove.isolates = FALSE, weighted = NULL, 
          edgesize = 1, edges.min = 0, alpha = 0.5, verbose = TRUE) 
{
  S <- NULL
  colnames(NetMatrix) <- rownames(NetMatrix) <- tolower(colnames(NetMatrix))
  bsk.S <- TRUE
  l <- NA
  net_groups <- list()
  if (!is.null(normalize)) {
    S <- normalizeSimilarity(NetMatrix, type = normalize)
    bsk.S <- graph.adjacency(S, mode = "undirected", weighted = T)
  }
  if (isTRUE(size)) {
    size <- 20
    size.cex <- T
  }
  if (alpha < 0 & alpha > 1) {
    alpha <- 0.5
  }
  bsk.network <- graph.adjacency(NetMatrix, mode = "undirected", 
                                 weighted = weighted)
  V(bsk.network)$name <- colnames(NetMatrix)
  deg <- degree(bsk.network, mode = "all")
  deg.dist <- data.frame(node = V(bsk.network)$name, degree = deg) %>% 
    arrange(desc(.data$degree)) %>% mutate(degree = .data$degree/max(.data$degree))
  V(bsk.network)$deg <- deg
  if (isTRUE(size.cex)) {
    V(bsk.network)$size <- (deg/max(deg)) * size
  }
  else {
    V(bsk.network)$size <- rep(size, length(V(bsk.network)))
  }
  if (isTRUE(label.cex)) {
    lsize <- log(1 + (deg/max(deg))) * labelsize
    lsize[lsize < 0.5] <- 0.5
    V(bsk.network)$label.cex <- lsize
  }
  else {
    V(bsk.network)$label.cex <- labelsize
  }
  if (!is.null(degree)) {
    Deg <- deg - diag(NetMatrix)
    Vind <- Deg < degree
    if (sum(!Vind) == 0) {
      cat("\ndegree argument is to high!\n\n")
      return()
    }
    bsk.network <- delete.vertices(bsk.network, which(Vind))
    if (!isTRUE(bsk.S)) {
      bsk.S <- delete.vertices(bsk.S, which(Vind))
    }
  }
  else if (!is.null(n)) {
    if (n > dim(NetMatrix)[1]) {
      n <- dim(NetMatrix)[1]
    }
    nodes <- names(sort(deg, decreasing = TRUE)[1:n])
    bsk.network <- delete.vertices(bsk.network, which(!(V(bsk.network)$name %in% 
                                                          nodes)))
    if (!isTRUE(bsk.S)) {
      bsk.S <- delete.vertices(bsk.S, which(!(V(bsk.S)$name %in% 
                                                nodes)))
    }
  }
  if (edges.min > 1) {
    remove.multiple = FALSE
  }
  bsk.network <- simplify(bsk.network, remove.multiple = remove.multiple, 
                          remove.loops = noloops)
  if (!isTRUE(bsk.S)) {
    bsk.S <- simplify(bsk.S, remove.multiple = remove.multiple, 
                      remove.loops = noloops)
  }
  bsk.save <- bsk.network
  V(bsk.save)$id <- V(bsk.save)$name
  E(bsk.network)$num <- E(bsk.save)$num <- count_multiple(bsk.network, 
                                                          eids = E(bsk.network))
  if (is.null(weighted)) {
    E(bsk.save)$weight <- E(bsk.save)$num
  }
  if (!is.null(weighted)) {
    E(bsk.network)$width <- (E(bsk.network)$weight + min(E(bsk.network)$weight))/max(E(bsk.network)$weight + 
                                                                                       min(E(bsk.network)$weight)) * edgesize
  }
  else {
    if (isTRUE(remove.multiple)) {
      E(bsk.network)$width <- edgesize
    }
    else {
      edges <- E(bsk.network)$num
      E(bsk.network)$width <- edges/max(edges) * edgesize
    }
  }
  bsk.network <- delete.edges(bsk.network, which(E(bsk.network)$num < 
                                                   edges.min))
  if (!isTRUE(bsk.S)) {
    bsk.S <- delete.edges(bsk.S, which(E(bsk.network)$num < 
                                         edges.min))
  }
  if (isTRUE(remove.isolates)) {
    bsk.network <- delete.isolates(bsk.network, mode = "all")
    if (!isTRUE(bsk.S)) {
      bsk.S <- delete.vertices(bsk.S, which(V(bsk.S)$name %in% 
                                              setdiff(V(bsk.S)$name, V(bsk.network)$name)))
    }
  }
  cl <- clusteringNetwork(bsk.network, cluster)
  bsk.network <- cl$bsk.network
  if (!isTRUE(bsk.S)) {
    V(bsk.S)$color <- V(bsk.network)$color
    V(bsk.S)$community <- V(bsk.network)$community
  }
  net_groups <- cl$net_groups
  if (!isTRUE(bsk.S)) {
    layout_results <- switchLayout(bsk.S, type, community.repulsion)
    bsk.S <- layout_results$bsk.S
  }
  else {
    layout_results <- switchLayout(bsk.network, type, community.repulsion)
    bsk.network <- layout_results$bsk.network
  }
  l <- layout_results$l
  LABEL = ""
  if (isTRUE(label)) {
    LABEL <- V(bsk.network)$name
    if (!is.null(label.n)) {
      q <- 1 - (label.n/length(V(bsk.network)$deg))
      if (q <= 0) {
        V(bsk.network)$labelsize <- 10
      }
      else {
        if (q > 1) {
          q <- 1
        }
        q <- quantile(V(bsk.network)$deg, q)
        LABEL[V(bsk.network)$deg < q] <- ""
        V(bsk.network)$labelsize <- 10
        V(bsk.network)$labelsize[V(bsk.network)$deg < 
                                   q] <- 0
      }
    }
  }
  if (isTRUE(label.color)) {
    lab.color <- V(bsk.network)$color
  }
  else {
    lab.color <- "black"
  }
  igraph::graph_attr(bsk.network, "alpha") <- alpha
  igraph::graph_attr(bsk.network, "ylim") <- c(-1, 1)
  igraph::graph_attr(bsk.network, "xlim") <- c(-1, 1)
  igraph::graph_attr(bsk.network, "rescale") <- TRUE
  igraph::graph_attr(bsk.network, "asp") <- 0
  igraph::graph_attr(bsk.network, "layout") <- l
  igraph::graph_attr(bsk.network, "main") <- Title
  E(bsk.network)$curved = curved
  V(bsk.network)$label.dist = 0.7
  V(bsk.network)$frame.color = adjustcolor("black", alpha)
  V(bsk.network)$color <- adjustcolor(V(bsk.network)$color, 
                                      alpha)
  V(bsk.network)$label.color <- adjustcolor("black", min(c(1, 
                                                           alpha + 0.1)))
  V(bsk.network)$label.font = 2
  V(bsk.network)$label = LABEL
  if (isTRUE(halo) & cluster != "none") {
    if (isTRUE(verbose)) {
      plot(net_groups, bsk.network)
    }
  }
  else {
    E(bsk.network)$color = adjustcolor(E(bsk.network)$color, 
                                       alpha/2)
    if (isTRUE(verbose)) {
      plot(bsk.network)
    }
  }
  if (cluster != "none") {
    cluster_res <- data.frame(net_groups$names, net_groups$membership, 
                              as.numeric(betweenness(bsk.network, directed = F, 
                                                     normalized = F)), suppressWarnings(as.numeric(closeness(bsk.network))), 
                              as.numeric(page.rank(bsk.network)$vector))
    names(cluster_res) <- c("vertex", "cluster", "btw_centrality", 
                            "clos_centrality", "pagerank_centrality")
    cluster_res <- cluster_res[order(cluster_res$cluster), 
    ]
  }
  else {
    cluster_res <- NA
  }
  params <- list(normalize = normalize, n = n, degree = degree, 
                 Title = Title, type = type, label = label, labelsize = labelsize, 
                 label.cex = label.cex, label.color = label.color, label.n = label.n, 
                 halo = halo, cluster = cluster, community.repulsion = community.repulsion, 
                 vos.path = vos.path, size = size, size.cex = size.cex, 
                 curved = curved, noloops = noloops, remove.multiple = remove.multiple, 
                 remove.isolates = remove.isolates, weighted = weighted, 
                 edgesize = edgesize, edges.min = edges.min, alpha = alpha, 
                 verbose = verbose)
  params <- data.frame(params = names(unlist(params)), values = unlist(params), 
                       row.names = NULL)
  net <- list(graph = bsk.network, graph_pajek = bsk.save, 
              cluster_obj = net_groups, cluster_res = cluster_res, 
              community_obj = cl$net_groups, layout = l, S = S, nodeDegree = deg.dist, 
              params = params)
  return(net)
}