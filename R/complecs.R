#' Create a COMPLECS graph
#'
#' COMPLECS was developed to help make sense of complex systems. It reads data from a
#' number of worksheets in a spreadsheet and generates a diagram according to those
#' specifications. Originally, COMPLECS was developed to visualise a problem during
#' the needs assessment phase of intervention development.
#'
#' COMPLECS is a recursive acronym for COMPLECS Organises Multiple Players & Linked
#' Environments using Connected Specifications.
#'
#' @param input Either a link to a Google Sheet, or a path to an Excel file.
#' @param title The title of the COMPLECS graph.
#' @param layout The layout to use; has to be one of the `DiagrammeR` layout
#' types (`dot`, `neato`, `circo` and `twopi`).
#' @param outputFile A character vector where each element is one path (including
#' filename) to write the graph to.
#' @param outputWidth,outputHeight If not `NULL`, a way to override the width and
#' height when calling `complecs` to generate a COMPLECS overview.
#' @param width,height If not `NULL`, a way to override the width and
#' height when calling `print` to print a COMPLECS overview.
#' @param returnSvgOnly Whether to only return the SVG in a character vector.
#' @param maxLabelLength The number of characters where to wrap the labels.
#' @param x The object to print (i.e. a result of a call to `complecs`).
#' @param ... Any additional arguments for the [print()] method are passed
#' to [DiagrammeR::render_graph()].
#'
#' @return A `complecs` object that includes the graph and the graph in SVG in
#' `output$graph` and `output$graphSvg`.
#' @export
#'
#' @examples complecs(paste0("https://docs.google.com/spreadsheets/d/",
#'          "1WMO15xroy4a0RfpuZ8GhT-NfDoxwS34w9PrWp8rGjjk"));
complecs <- function(input,
                     title = "COMPLECS overview",
                     layout = "neato",
                     outputFile = NULL,
                     outputWidth=NULL,
                     outputHeight=NULL,
                     returnSvgOnly = FALSE,
                     maxLabelLength=20) {

  entitySheet <- opts$get("complecs_entitySheet");
  connectionsSheet <- opts$get("complecs_connectionsSheet");
  entityTypesSheet <- opts$get("complecs_entityTypesSheet");
  connectionTypesSheet <- opts$get("complecs_connectionTypesSheet");
  entityCols <- opts$get("complecs_entityCols");
  connectionsCols <- opts$get("complecs_connectionsCols");
  entityTypesCols <- opts$get("complecs_entityTypesCols");
  connectionTypesCols <- opts$get("complecs_connectionTypesCols");

  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());

  if (file.exists(input)) {
    ### Read the file

  } else if ((length(input) == 1) && (grepl('^http.?://', input))) {
    ### Read the google sheets workbook
    registeredWS <-
      googlesheets::gs_url(input,
                           verbose=FALSE);

    ### Read the list of worksheets
    worksheetNames <-
      googlesheets::gs_ws_ls(registeredWS);

    worksheetData <-
      lapply(worksheetNames,
             function(i) {
               googlesheets::gs_read(registeredWS, ws=i,
                                     verbose=FALSE);
             });
    names(worksheetData) <- worksheetNames;

  } else {
    stop("As `input`, provide either the path to a file or the URL of a google spreadsheet.");
  }

  res$intermediate$worksheetData <-
    worksheetData;

  ### Check for presence of required worksheets
  if (!all(c(entitySheet,
             connectionsSheet,
             entityTypesSheet,
             connectionTypesSheet) %in% names(worksheetData))) {
    stop("Not all required worksheets exist in the COMPLEX ",
         "specification! The currently configured worksheet ",
         "names are ", vecTxtQ(c(entitySheet,
                                 connectionsSheet,
                                 entityTypesSheet,
                                 connectionTypesSheet)), ", ",
         "and the worksheets in the spreadsheet you specified ",
         "are ", vecTxtQ(worksheetNames), ".");
  }

  entities <- as.data.frame(worksheetData[[entitySheet]]);
  connections <- as.data.frame(worksheetData[[connectionsSheet]]);
  entityTypes <- as.data.frame(worksheetData[[entityTypesSheet]]);
  connectionTypes <- as.data.frame(worksheetData[[connectionTypesSheet]]);

  ### Merge columns from type dataframes into regular dataframes
  mergedEntities <-
    merge(x = entities,
          y = entityTypes,
          by.x = entityCols['entity_type_id'],
          by.y = entityTypesCols['entity_type_id']);
  mergedConnections <-
    merge(x = connections,
          y = connectionTypes,
          by.x = connectionsCols['connection_type_id'],
          by.y = connectionTypesCols['connection_type_id']);

  ### Convert merged dataframes into lists
  entitiesAsList <-
    c(list(n = nrow(entities),
           type = mergedEntities[, entityCols['entity_type_id']],
           label = mergedEntities[, entityCols['entity_label']]),
      as.list(mergedEntities[, setdiff(names(mergedEntities),
                                       c(entityCols['entity_type_id'],
                                         entityCols['entity_label']))]));

  # ### Create vectors with entity type and connection type attributes
  # entityType_stroke <- stats::setNames(entityTypes[, entityTypesCols['entity_type_stroke']],
  #                                      nm = entityTypes[, entityTypesCols['entity_type_id']]);
  # entityType_fill <- stats::setNames(entityTypes[, entityTypesCols['entity_type_fill']],
  #                                    nm = entityTypes[, entityTypesCols['entity_type_id']]);
  # entityType_shape <- stats::setNames(entityTypes[, entityTypesCols['entity_type_shape']],
  #                                     nm = entityTypes[, entityTypesCols['entity_type_id']]);
  # entityType_style <- stats::setNames(entityTypes[, entityTypesCols['entity_type_style']],
  #                                     nm = entityTypes[, entityTypesCols['entity_type_id']]);
  # entityType_text <- stats::setNames(entityTypes[, entityTypesCols['entity_type_text']],
  #                                    nm = entityTypes[, entityTypesCols['entity_type_id']]);
  # connectionType_stroke <-
  #   stats::setNames(connectionTypes[, connectionTypesCols['connection_type_stroke']],
  #                   nm = connectionTypes[, connectionTypesCols['connection_type_id']]);
  # connectionType_style <-
  #   stats::setNames(connectionTypes[, connectionTypesCols['connection_type_style']],
  #                   nm = connectionTypes[, connectionTypesCols['connection_type_id']]);
  # connectionType_dir <-
  #   stats::setNames(connectionTypes[, connectionTypesCols['connection_type_dir']],
  #                   nm = connectionTypes[, connectionTypesCols['connection_type_id']]);

  nodes_df <-
    do.call(DiagrammeR::create_node_df,
            entitiesAsList);

  # nodes_df <-
  #   DiagrammeR::create_node_df(nrow(entities),
  #                              type=entities[, entityCols['entity_type_id']],
  #                              label=entities[, entityCols['entity_label']],
  #                              entity_id=entities[, entityCols['entity_id']],
  #                              color = entityType_stroke[entities[, entityCols['entity_type_id']]],
  #                              fillcolor = entityType_fill[entities[, entityCols['entity_type_id']]],
  #                              fontcolor = entityType_text[entities[, entityCols['entity_type_id']]],
  #                              shape = entityType_shape[entities[, entityCols['entity_type_id']]],
  #                              style = entityType_style[entities[, entityCols['entity_type_id']]]);

  ### Create a vector to conveniently convert entity_ids to the node_df ids
  node_ids <- stats::setNames(nodes_df$id,
                              nm=nodes_df$entity_id);

  connectionsAsList <-
    c(list(from = node_ids[mergedConnections[, "from_entity_id"]],
           to = node_ids[mergedConnections[, "to_entity_id"]],
           rel = mergedConnections[, connectionsCols["connection_type_id"]]),
      as.list(mergedConnections[, setdiff(names(mergedConnections),
                                          c(connectionsCols["from_entity_id"],
                                            connectionsCols["to_entity_id"],
                                            connectionsCols["connection_type_id"]))]));

  edges_df <-
    do.call(DiagrammeR::create_edge_df,
            connectionsAsList);

  ### Wrap labels in node_df
  nodes_df$label <-
    unlist(lapply(nodes_df$label,
                  function(lbl) {
                    paste0(strwrap(lbl, maxLabelLength), collapse="\n");
                  }));

  # edges_df <-
  #   DiagrammeR::create_edge_df(from = node_ids[connections[, "from_entity_id"]],
  #                              to = node_ids[connections[, "to_entity_id"]],
  #                              rel = connections[, "connection_type_id"],
  #                              color = connectionType_stroke[connections[, connectionsCols['connection_type_id']]],
  #                              style = connectionType_style[connections[, connectionsCols['connection_type_id']]],
  #                              dir = connectionType_dir[connections[, connectionsCols['connection_type_id']]]);

  res$intermediate$nodes_df <-
    nodes_df;
  res$intermediate$edges_df <-
    edges_df;

  ### Combine node and edge dataframes into a graph
  graph <-
    DiagrammeR::create_graph(nodes_df = nodes_df,
                             edges_df = edges_df,
                             graph_name = title);

  graph <-
    apply_graph_theme(graph,
                      c("layout", layout, "graph"),
                      c("outputorder", "nodesfirst", "graph"),
                      c("fixedsize", "false", "node"));

  ### From DiagrammeR::export_graph
  dot_code <- DiagrammeR::generate_dot(graph);
  graphSvg <-
    DiagrammeRsvg::export_svg(DiagrammeR::grViz(dot_code));
  graphSvg <-
    sub(".*\n<svg ", "<svg ", graphSvg);
  graphSvg <- gsub('<svg width=\"[0-9]+pt\" height=\"[0-9]+pt\"\n viewBox=',
                   '<svg viewBox=',
                   graphSvg);

  if (!is.null(outputFile)) {
    for (currentFile in outputFile) {
      if (!dir.exists(dirname(currentFile))) {
        warning("Directory specified to save output file to, '",
                dirname(currentFile), "', does not exist! Have not written output file.");
      } else {
        DiagrammeR::export_graph(graph,
                                 file_name = currentFile,
                                 file_type = tools::file_ext(currentFile),
                                 width = outputWidth,
                                 height = outputHeight,
                                 title = DiagrammeR::get_graph_name(graph));
      }
    }
  }

  res$output$graph <- graph;
  res$output$graphsSvg <- graphSvg;

  class(res) <- "complecs";

  if (returnSvgOnly) {
    return(graphSvg);
  } else {
    return(res);
  }

}

#' @rdname complecs
#' @method print complecs
#' @export
print.complecs <- function(x,
                           width=x$input$width,
                           height=x$input$height,
                           title = DiagrammeR::get_graph_name(x$output$graph),
                           ...) {
  print(DiagrammeR::render_graph(x$output$graph,
                                 title=title,
                                 width=width,
                                 height=height,
                                 ...));
}

