#' This function is ued to find the greatest common devisier
#'
#' @param H1 first value
#' @param H2 second value
#'
#' @return its return the greatest common devisier
#' @export
#'
#' @examples
#' euclidean(123612,13892347912)
#' euclidean(100,1000)

euclidean = function(H1,H2){
  if(H2 == 0 ){
    stop("can not divide by zero")
  }
  while(H2 != 0){
    temp = H2
    H2 = H1 %% H2
    H1 = temp
  }
  return(H1)
}


#' Dijkstra's algorithm used for finding the shortest paths between nodes in a graph.
#'
#' @param graph graph
#' @param init_node starting node
#'
#' @return it return the shortest node
#' @export
#'
#' @examples
#' wiki_graph <-
#' data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'            v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'            w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#' dijkstra(wiki_graph, 1)
#' dijkstra(wiki_graph, 3)



dijkstra <- function(graph, init_node) {

  # validate first
  stopifnot(is.numeric(init_node) && init_node < length(unique(c(graph$v1, graph$v2))))

  node_names= unique(c(graph$v1, graph$v2))
  #check initial node is not available in node_names
  #print(node_names)
  if(!(init_node %in% node_names)){
    stop("init node is not in give graph nodes")
  }

  #Create graph
  h_matrix = data.matrix(graph)
  distance = rep(Inf, length(node_names))
  names(distance) = node_names

  check_visited = c(rep(FALSE,length(node_names))) #to check if a node was visited
  names(check_visited) = node_names

  distance[as.character(init_node)] = 0  #distance from init_node to itself=0

  while(length(h_matrix) > 0){
    #Get the node still in graph with the low distance
    nodes = unique(as.vector(h_matrix[,1:2]))
    leat_dist_node = names(which.min(distance[as.character(nodes)]))
    #Get relevant edges
    relat_to_least_dist_node = h_matrix[which(h_matrix[,1:2]==leat_dist_node, arr.ind = T)[,1],]

    #remove least_dist_node from matrix
    h_matrix = h_matrix[-which(h_matrix[,1:2]==leat_dist_node, arr.ind = T)[,1],]

    #Get adjacent nodes
    u_nodes = unique(as.vector(relat_to_least_dist_node[,1:2]))

    for(adjacent in u_nodes){
      weight = relat_to_least_dist_node[which(relat_to_least_dist_node[,1:2] == adjacent, arr.ind = T)[,1],3][1]
      alt = distance[as.character(leat_dist_node)] + weight
      if(alt < distance[as.character(adjacent)]){
        distance[as.character(adjacent)] = alt
        check_visited[as.character(adjacent)] = leat_dist_node
      }
    }
  }
  distance <- unname(distance)
  return(distance)
}

wiki_graph <- data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
                         v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
                         w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
