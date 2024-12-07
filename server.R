#server.R

# Serveur (backend principal)
server <- function(input, output, session) {
  # Réactif pour stocker les données du graphe
  graph_data <- reactiveValues(
    nodes = data.frame(id = character(), label = character(), color = character(), compartment = character(), stringsAsFactors = FALSE),
    edges = data.frame(from = character(), to = character(), label = character(), stringsAsFactors = FALSE)
  )
  
  # Réactif pour suivre si le graphe est généré
  graph_generated <- reactiveVal(FALSE)
  
  # Gestion du clic sur le bouton "Add Node"
  observeEvent(input$add_node, {
    showModal(modalDialog(
      title = "Ajouter un nœud",
      textInput("node_id", "ID du nœud :", value = ""),
      textInput("node_label", "Nom du nœud :", value = ""),
      selectInput("node_shape", "Forme du nœud :", 
                  choices = c("circle", "diamond", "square"), 
                  selected = "circle"),
      conditionalPanel(
        condition = "input.node_shape != 'square'",  # Montrer ce champ uniquement si la forme n'est pas "square"
        selectInput("node_compartment", "Compartiment :", 
                    choices = c(names(compartment_colors)), 
                    selected = "cytosol")
      ),
      footer = tagList(
        actionButton("confirm_add_node", "Confirm"),
        modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$confirm_add_node, {
    req(input$node_id, input$node_label, input$node_shape)
    
    # Vérifier si l'ID existe déjà
    if (input$node_id %in% graph_data$nodes$id) {
      showNotification("L'ID du nœud existe déjà !", type = "error")
      return()
    }
    
    # Définir les propriétés spécifiques pour les nœuds
    if (input$node_shape == "square") {
      # Forcer les propriétés pour les carrés (réactions)
      node_compartment <- "reaction"
      node_size <- 20
      node_color_background <- "red"
      border_color <- "black"
    } else {
      # Propriétés par défaut pour les autres formes
      node_compartment <- input$node_compartment
      node_size <- 50
      node_color_background <- "lightgrey"
      border_color <- if (node_compartment %in% names(compartment_colors)) {
        compartment_colors[[node_compartment]]
      } else {
        "black"  # Couleur par défaut si le compartiment n'est pas trouvé
      }
    }
    
    # Créer le nœud avec toutes les propriétés nécessaires
    new_node <- data.frame(
      id = input$node_id,
      label = input$node_label,
      shape = input$node_shape,
      size = node_size,
      font.size = 20,
      color.background = node_color_background,
      color.border = border_color,
      compartment = node_compartment,
      stringsAsFactors = FALSE
    )
    
    # Synchronisation des colonnes entre les nœuds existants et le nouveau
    all_columns <- union(names(graph_data$nodes), names(new_node))
    for (col in setdiff(all_columns, names(graph_data$nodes))) {
      graph_data$nodes[[col]] <- NA
    }
    for (col in setdiff(all_columns, names(new_node))) {
      new_node[[col]] <- NA
    }
    
    # Ajouter le nœud aux données existantes
    graph_data$nodes <- rbind(graph_data$nodes, new_node)
    
    # Appliquer la fonction de calcul des tailles
    graph_data$nodes <- calculate_node_size(graph_data$nodes)
    
    # Mettre à jour les sélecteurs
    updateSelectInput(session, "edge_from", choices = setNames(graph_data$nodes$id, graph_data$nodes$label))
    updateSelectInput(session, "edge_to", choices = setNames(graph_data$nodes$id, graph_data$nodes$label))
    updateSelectInput(session, "delete_edge_from", choices = setNames(graph_data$nodes$id, graph_data$nodes$label))
    updateSelectInput(session, "delete_edge_to", choices = setNames(graph_data$nodes$id, graph_data$nodes$label))
    
    # Notification de succès
    showNotification("Node successfully added!", type = "message")
    removeModal()
  })
  
  observeEvent(input$delete_node, {
    req(input$network_selected)  # Vérifie qu'un nœud est sélectionné dans le graphe
    
    # Récupérer l'ID du nœud sélectionné
    selected_node <- input$network_selected
    
    # Supprimer le nœud sélectionné et ses arêtes associées
    graph_data$nodes <- graph_data$nodes[graph_data$nodes$id != selected_node, ]
    graph_data$edges <- graph_data$edges[!(graph_data$edges$from == selected_node | graph_data$edges$to == selected_node), ]
    
    # Recalculer les tailles après suppression
    graph_data$nodes <- calculate_node_size(graph_data$nodes)
    
    # Mettre à jour les sélecteurs
    updateSelectInput(session, "edge_from", choices = setNames(graph_data$nodes$id, graph_data$nodes$label))
    updateSelectInput(session, "edge_to", choices = setNames(graph_data$nodes$id, graph_data$nodes$label))
    updateSelectInput(session, "delete_edge_from", choices = setNames(graph_data$nodes$id, graph_data$nodes$label))
    updateSelectInput(session, "delete_edge_to", choices = setNames(graph_data$nodes$id, graph_data$nodes$label))
    
    # Notification de succès
    showNotification("Node successfully deleted!", type = "message")
  })
  
  # Modale pour ajouter une arête
  observeEvent(input$add_edge_button, {
    showModal(modalDialog(
      title = "Add Edge",
      selectInput("edge_from", "From Node:", 
                  choices = setNames(graph_data$nodes$id, graph_data$nodes$label)),
      selectInput("edge_to", "To Node:", 
                  choices = setNames(graph_data$nodes$id, graph_data$nodes$label)),
      textInput("edge_label", "Edge Label (Optional):"),
      selectInput("arrow_type", "Arrow Type:",
                  choices = list("Basic (Black)" = "basic",
                                 "Inhibition (Red)" = "inhibition",
                                 "Activation (Dashed Green)" = "activation")),
      footer = tagList(
        actionButton("confirm_add_edge", "Confirm"),
        modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$confirm_add_edge, {
    req(input$edge_from, input$edge_to, input$arrow_type)
    
    # Check if nodes exist
    if (!(input$edge_from %in% graph_data$nodes$id) || !(input$edge_to %in% graph_data$nodes$id)) {
      showNotification("Specified nodes do not exist.", type = "error")
      return()
    }
    
    # Determine edge properties based on arrow type
    edge_color <- "black"
    edge_dashes <- FALSE
    
    if (input$arrow_type == "inhibition") {
      edge_color <- "red"
    } else if (input$arrow_type == "activation") {
      edge_color <- "green"
      edge_dashes <- TRUE
    }
    
    # Create the new edge
    new_edge <- data.frame(
      from = input$edge_from,
      to = input$edge_to,
      arrows = "to", # Arrow points to the target node
      color = edge_color, # Set color based on selection
      dashes = edge_dashes, # Dashed line for activation
      width = 1, # Default edge width
      label = input$edge_label, # Optional label
      stringsAsFactors = FALSE
    )
    
    # Synchronize columns with existing edges
    all_columns <- union(names(graph_data$edges), names(new_edge))
    for (col in setdiff(all_columns, names(graph_data$edges))) {
      graph_data$edges[[col]] <- NA
    }
    for (col in setdiff(all_columns, names(new_edge))) {
      new_edge[[col]] <- NA
    }
    
    # Add the new edge to the data
    graph_data$edges <- rbind(graph_data$edges, new_edge)
    
    # Success notification
    showNotification("Edge successfully added!", type = "message")
    removeModal()
  })
    
  
  observeEvent(input$delete_edge, {
    showModal(modalDialog(
      title = "Delete Edge",
      # From Node with search feature
      selectizeInput("delete_edge_from", "From Node:", 
                     choices = setNames(graph_data$nodes$id, graph_data$nodes$label),
                     selected = NULL, 
                     options = list(placeholder = "Select or search a node")),
      # To Node with search feature
      selectizeInput("delete_edge_to", "To Node:", 
                     choices = setNames(graph_data$nodes$id, graph_data$nodes$label),
                     selected = NULL, 
                     options = list(placeholder = "Select or search a node")),
      # Footer with Confirm and Cancel buttons
      footer = tagList(
        modalButton("Cancel"), # Cancel button
        actionButton("confirm_delete_edge", "Delete") # Confirmation button
      )
    ))
  })
  
  observeEvent(input$confirm_delete_edge, {
    req(input$delete_edge_from, input$delete_edge_to) # Ensure required inputs are provided
    
    # Check if any matching edges exist
    edges_to_delete <- graph_data$edges[
      graph_data$edges$from == input$delete_edge_from &
        graph_data$edges$to == input$delete_edge_to, 
    ]
    
    if (nrow(edges_to_delete) == 0) {
      showNotification("No matching edge found to delete.", type = "error")
      return()
    }
    
    # Remove the matching edge(s)
    graph_data$edges <- graph_data$edges[!(
      graph_data$edges$from == input$delete_edge_from &
        graph_data$edges$to == input$delete_edge_to
    ), ]
    
    # Notify the user of successful deletion
    showNotification("Edge successfully deleted!", type = "message")
    
    # Close the modal
    removeModal()
  })
  
  # Charger un fichier SBML
  observeEvent(input$generate_graph, {
    req(input$sbml_file)
    
    sbml_data <- load_sbml_data(input$sbml_file$datapath)
    graph_data$nodes <- sbml_data$nodes
    graph_data$edges <- sbml_data$edges
    
    # Recalculer les tailles après génération
    graph_data$nodes <- calculate_node_size(graph_data$nodes)
    
    # Mettre à jour les sélecteurs
    updateSelectInput(session, "edge_from", choices = setNames(graph_data$nodes$id, graph_data$nodes$label))
    updateSelectInput(session, "edge_to", choices = setNames(graph_data$nodes$id, graph_data$nodes$label))
    updateSelectInput(session, "delete_edge_from", choices = setNames(graph_data$nodes$id, graph_data$nodes$label))
    updateSelectInput(session, "delete_edge_to", choices = setNames(graph_data$nodes$id, graph_data$nodes$label))
  })
  
  # Légende dynamique dans la sidebar
  output$sidebar_legend <- renderUI({
    if (nrow(graph_data$nodes) > 0) {
      # Obtenir les compartiments uniques avec leurs couleurs
      unique_compartments <- unique(graph_data$nodes[, c("compartment", "color.border"), drop = FALSE])
      unique_compartments <- unique_compartments[complete.cases(unique_compartments), ]
      
      div(
        h4("Legend"),
        tags$ul(
          lapply(1:nrow(unique_compartments), function(i) {
            tags$li(
              style = "list-style-type: none; margin-bottom: 5px; display: flex; align-items: center;",
              div(
                style = paste0(
                  "width: 20px; height: 20px; border: 1px solid ", unique_compartments$color.border[i], 
                  "; background-color: transparent; border-radius: 50%; margin-right: 10px;"  # Bordures arrondies pour un rond
                )
              ),
              span(unique_compartments$compartment[i], style = "color: black; font-size: 14px;")
            )
          })
        )
      )
    } else {
      div(h4("No legend to display"))
    }
  })
  
  # Télécharger le fichier SBML
  output$download_sbml <- downloadHandler(
    filename = function() {
      input$save_filename
    },
    content = function(file) {
      save_sbml(graph_data$nodes, graph_data$edges, file)
    }
  )
  
  # Résumé du graphe
  output$graph_summary <- renderText({
    num_nodes <- nrow(graph_data$nodes)
    num_edges <- nrow(graph_data$edges)
    paste("Nodes:", num_nodes, "| Edges:", num_edges)
  })
  
  # Tableaux interactifs
  output$nodes_table <- renderDT({
    datatable(graph_data$nodes, options = list(pageLength = 10))
  })
  
  output$edges_table <- renderDT({
    datatable(graph_data$edges, options = list(pageLength = 10))
  })
  
  output$network <- renderVisNetwork({
    req(graph_data$nodes, graph_data$edges)
    
    # Recalculate node sizes before rendering
    graph_data$nodes <- calculate_node_size(graph_data$nodes)
    
    # Retrieve the selected layout
    layout_choice <- input$layout_choice
    
    vis_net <- visNetwork(graph_data$nodes, graph_data$edges) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visEdges(arrows = "to") %>%
      visInteraction(zoomView = TRUE, dragView = TRUE, multiselect = TRUE) %>%
      visNodes(
        font = list(size = 20, vadjust = -30), # Position labels outside nodes
        labelHighlightBold = FALSE, # Disable bold highlight on hover
        scaling = list(label = list(enabled = FALSE)) # Disable automatic scaling
      )
    
    # Apply layout based on user choice
    if (layout_choice == "forceAtlas2Based") {
      vis_net <- vis_net %>%
        visPhysics(
          solver = "forceAtlas2Based",
          stabilization = TRUE,
          forceAtlas2Based = list(
            gravitationalConstant = -100,
            centralGravity = 0.01,
            springLength = 150,
            springConstant = 0.08,
            avoidOverlap = 0.2,
            damping = 0.4
          )
        )
    } else if (layout_choice == "barnesHut") {
      vis_net <- vis_net %>%
        visPhysics(
          solver = "barnesHut",
          stabilization = TRUE,
          barnesHut = list(
            gravitationalConstant = -20000,
            centralGravity = 0.3,
            springLength = 200,
            springConstant = 0.05,
            damping = 0.09
          )
        )
    } else if (layout_choice == "hierarchical") {
      vis_net <- vis_net %>%
        visHierarchicalLayout(
          direction = "UD",  # Up-Down direction
          levelSeparation = 150,
          nodeSpacing = 150,
          treeSpacing = 200,
          blockShifting = TRUE,
          edgeMinimization = TRUE,
          parentCentralization = TRUE
        )
    } else if (layout_choice == "circular") {
      vis_net <- vis_net %>%
        visIgraphLayout(layout = "layout_in_circle")
    }
    
    vis_net
  })
}