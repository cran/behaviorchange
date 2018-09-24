#' Acyclic Behavior Change Diagram
#'
#' This function generates an acyclic behavior change diagram (ABCD)
#' from a specification in a google sheet or .csv file. An ABCD is
#' a logic model that illustrates the assumptions underlying a
#' behavior change intervention. Specifically, the ABCD shows the
#' assumed causal and structural assumptions, thereby showing what is
#' assumed to cause what (e.g. which elements of the intervention are
#' assumed to influence which aspects of the target population's
#' psychology?) and what is assumed to consist of what (e.g. which
#' determinants are assumed to contain which specific aspects of the
#' target population's psychology?).
#'
#' Specifically, a full ABCD is a model that shows the following
#' elements:
#'
#' - **Behavior Change Principles (BCPs)**: The specific
#'     psychological principles engaged to influence the relevant
#'     sub-determinants, usually selected using the determinants
#'     to which the sub-determinants 'belong'. These are also
#'     known as methods of behavior change in the Intervention
#'     Mapping framework, or behavior change techniques, BCTs,
#'     in the Behavior Change Wheel approach. For a list of 99
#'     BCPs, see Kok et al. (2016).
#' - **Conditions for effectiveness**: The conditions
#'     that need to be met for a Behavior Change Principle (BCP) to
#'     be effective. These conditions depend on the specific
#'     underlying Evolutionary Learning Processes (ELPs) that the
#'     BCP engages (Crutzen & Peters, 2018). If the conditions for
#'     effectiveness (called *parameters* for effectiveness in the
#'     Intervention Mapping framework) are not met, the method will
#'     likely not be effective, or at least, not achieve its
#'     maximum effectiveness.
#' - **Applications**: Since BCP's describe aspects of
#'     human psychology in general, they are necessarily formulated
#'     on a generic level. Therefore, using them in an intervention
#'     requires translating them to the specific target population,
#'     culture, available means, and context. The result of this
#'     translation is the application of the BCP. Multiple BCPs can
#'     be combined into one application; and one BCP can be applied in
#'     multiple applications (see Kok, 2014).
#' - **Sub-determinants**: Behavior change interventions
#'     engage specific aspects of the human psychology (ideally, they
#'     specifically, target those aspects found most important in
#'     predicting the target behavior, as can be established with
#'     \code{\link[behaviorchange]{CIBER}} plots. These aspects are
#'     called sub-determinants (the Intervention Mapping framework
#'     references *Change Objectives*, which are sub-determinants
#'     formulated according to specific guidelines). In some
#'     theoretical traditions, sub-determinants are called *beliefs*.
#' - **Determinants**: The overarching psychological constructs that
#'     are defined as clusters of specific aspects of the human
#'     psychology that explain humans' behavior (and are targeted
#'     by behavior change interventions). Psychological theories
#'     contain specific definitions of such determinants, and make
#'     statements about how they relate to each other and to human
#'     behavior. There are also theories (and exists empirical
#'     evidence) on how these determinants can be changed (i.e. BCPs),
#'     so althought the sub-determinants are what is targeted in an
#'     intervention, the selection of feasible BCPs requires knowing
#'     to which determinants those sub-determinants belong.
#' - **Performance objectives**: The specific sub-behaviors that often
#'     underlie (or make up) the ultimate target behavior. These are
#'     distinguished from the overarching target behavior because
#'     the relevant determinants of these sub-behaviors can be
#'     different: for example, the reasons why people do or do not
#'     *buy* condoms can be very different from the reasons why they
#'     do or do not *carry* condoms or why they do or do not
#'     *negotiate* condom use with a sexual partner.
#' - **Behavior**: The ultimate target behavior of the intervention,
#'     usually an umbrella that implicitly contains multiple
#'     performance objectives.
#'
#' For details, see Peters et al. (2019).
#'
#' @param specs The specifications: either a google sheets URL,
#'   the path to a local file, a character vector with both,
#'   or a matrix or data frame
#' @param specCols The order of the columns. This character vector
#'   specified the order of the elements of an ABCD. In the default
#'   order, from left to right, these are (see below for definitions
#'    and more details):
#'   - `bcps` = **Behavior Change Principles (BCPs)**;
#'   - `cnds` = **Conditions for effectiveness**;
#'   - `apps` = **Applications**;
#'   - `sdts` = **Sub-determinants**;
#'   - `dets` = **Determinants**;
#'   - `pobs` = **Performance Objectives**;
#'   - `behs` = **Behaviors**;
#' @param omitColOrder If not all seven columns are available in the
#'   specifications, this character vector specifies which columns
#'   are assumed to be omitted.
#' @param localBackup Whether to write the specifications
#'   to a local backup
#' @param title The title of the diagram
#' @param outputFile If specified, the ABCD is written to
#'   this file.
#' @param outputWidth,outputHeight If an `outputFile` is
#'   specified, these determine its width and height (in pixels)
#' @param includeColNames Whether to include the column names
#'   as titles/legend for the entities in each 'column'
#'   of the ABCD.
#' @param maxLabelLength At which width to word wrap the
#'   labels.
#' @param silent Whether to suppress (`TRUE`) or show
#'   (`FALSE`) more detailed information.
#' @param regExReplacements A list of pairs of regular
#'   expressions that will be applied to the specifications
#'   before generating the ABCD. This can be used to sanitize
#'   problematic characters (e.g. ', " and \).
#'
#' @return A list consisting of an `input`, `intermediate`, and
#'   `output` list, where the ABCD is stored in the `output` list
#'   as `graph`.
#' @author
#'   Gjalt-Jorn Peters, \email{gjalt-jorn@@a-bc.eu}, with
#'   contributions from Matti Heino.
#' @references
#'   Crutzen, R., & Peters, G.-J. Y. (2018). Evolutionary
#'   learning processes as the foundation for behaviour change.
#'   Health Psychology Review, 12(1), 43–57.
#'   https://doi.org/10.1080/17437199.2017.1362569
#'
#'   Kok, G. (2014). A practical guide to effective behavior
#'   change: How to apply theory- and evidence-based behavior
#'   change methods in an intervention. European Health Psychologist,
#'   16(5), 156–170. https://doi.org/10.31234/osf.io/r78wh
#'
#'   Kok, G., Gottlieb, N. H., Peters, G.-J. Y., Mullen,
#'   P. D., Parcel, G. S., Ruiter, R. A. C., … Bartholomew,
#'   L. K. (2016). A taxonomy of behavior change methods:
#'   an Intervention Mapping approach. Health Psychology
#'   Review, 10(3), 297–312.
#'   https://doi.org/10.1080/17437199.2015.1077155
#'
#'   Peters, G.-J. Y., et al. (2019) The core of
#'   behavior change: introducing the Acyclic Behavior Change
#'   Diagram to report and analyze interventions.
#' @examples ### Partial acyclic behavior change diagram of only
#' ### one performance objective (sub-behavior)
#' behaviorchange::abcd(abcd_specs_single_po);
#'
#' ### Full acyclic behavior change diagram
#' behaviorchange::abcd(abcd_specs_full);
#'
#' @export

abcd <- function(specs,
                 specCols = c('bcps', 'cnds', 'apps', 'sdts', 'dets', 'pobs', 'behs'),
                 omitColOrder = c('cnds', 'behs', 'bcps', 'apps'),
                 localBackup = NULL,
                 title = "Acyclic Behavior Change Diagram\n\n",
                 outputFile = NULL,
                 outputWidth=3000,
                 outputHeight=1500,
                 includeColNames = TRUE,
                 maxLabelLength = 30,
                 silent = FALSE,
                 regExReplacements = list(c("\\\"", "`"),
                                          c("\\'", "`"),
                                          c("\\\\", "/"))) {

  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());

  loadedDatasheet <- FALSE;

  if (is.character(specs) && length(specs) == 2) {
    whichIsURL <- grepl('^http.?://', specs);
    if (!(TRUE %in% whichIsURL && FALSE %in% whichIsURL)) {
      stop("If argument 'specs' has two values, one should be a Google Sheets URL and one should be a filename!");
    }
    gs_url <- specs[whichIsURL];
    file <- specs[!whichIsURL];
  } else if (is.character(specs) && length(specs) == 1) {
    if (grepl('^http.?://', specs)) {
      gs_url <- specs[grepl('^http.?://', specs)];
    } else {
      file <- specs;
    }
  } else if (is.matrix(specs) || is.data.frame(specs)) {
    datasheet <- specs;
    loadedDatasheet <- TRUE;
  } else {
    stop("Argument 'specs' was not a character value or vector, a matrix, or a data frame!");
  }

  ### Import sheets, if sheets identifier (gs_url) was provided
  if (!loadedDatasheet && !is.null(gs_url)) {
    tryCatch({
      gsObject <- googlesheets::gs_url(gs_url);
      datasheet <- googlesheets::gs_read(gsObject);
      loadedDatasheet <- TRUE;
      if (!silent) {
        cat("Successfully read the data from Google sheets.\n");
      }
    },
    error = function(e) {
      if (!silent) {
        cat("You specified a google sheet, but I have problems",
            "accessing it - trying to access a local file.\n");
      }
    });
  }

  ### If the sheets identifier was not provided, or loading it failed,
  ### load from a local file
  if (!loadedDatasheet) {

    ### Check whether the files exist
    if (!is.null(file)) {
      if (!file.exists(file)) {
        stop("You specified a filename for 'file' ('",
             file, "'), but it does not exist.");
      }
    } else {
      stop("Did not manage to load the specifications!");
    }

    datasheet <- utils::read.csv(file, stringsAsFactors = FALSE);

    if (!silent) {
      cat("Succesfully read the ABCD specifications from local files.\n");
    }

  }

  ### Write local backup, if need be
  if (!is.null(localBackup)) {
    utils::write.csv(datasheet,
                     row.names=FALSE,
                     localBackup);
    if (!silent) {
      cat("Stored local backup to '", localBackup, "'.\n", sep="");
    }
  }

  ### Save original names for use later on (since
  ### conversion to a regular data frame will replace
  ### e.g. spaces etc)
  originalColNames <- names(datasheet);

  res$intermediate$datasheet <-
    datasheet <-
    as.data.frame(lapply(datasheet,
                         function(column) {
                           for (i in seq_along(regExReplacements)) {
                             column <- gsub(regExReplacements[[i]][1],
                                            regExReplacements[[i]][2],
                                            column);
                            }
                            return(column);
                          }),
                  stringsAsFactors=FALSE);

  ### Get number of columns with data
  useCols <- ncol(datasheet);

  ### Check for problematic numbers of columns
  if (useCols > length(specCols)) {
    warning("The specification contains ", useCols,
            "columns, but I can use at most  ",
            (length(specCols) - length(omitColOrder)),
            "columns: ingnoring the right-most columns.");
    useCols <- length(specCols);
  } else if (useCols < (length(specCols) - length(omitColOrder))) {
    stop("The specification contains ", useCols,
         "columns, but I need at least ",
         (length(specCols) - length(omitColOrder)),
         "columns.");
  }

  ### If this is not a complete ABCD specification,
  ### omit the required number of columns
  usedCols <- setdiff(specCols, utils::head(omitColOrder,
                                            length(specCols) - useCols));

  ### Process column names; first get them from the list of
  ### original column names, omitting the column for conditions
  colNames <- originalColNames[which(usedCols %in% setdiff(specCols, 'cnds'))];
  col_ids <- seq_along(colNames);
  names(col_ids) <- colNames;
  colNames <- sapply(colNames, function(x)
    return(sapply(x, function(xx)
      return(paste0(strwrap(xx, maxLabelLength), collapse="\n")))));

  ### Extract the columns from the datasheet
  cols <- list();
  cols <- lapply(usedCols,
                 function(colname) {
                   return(unique(datasheet[, which(usedCols == colname)]));
                 });





  # if (ncol(datasheet) < 5) {
  #   stop("The loaded data sheet does not have at least five columns.");
  # } else if (ncol(datasheet) == 5) {
  #   includeBehavior <- FALSE;
  #   useCols <- 5;
  # } else if (ncol(datasheet) == 6) {
  #   includeBehavior <- TRUE;
  #   useCols <- 6;
  # } else if (ncol(datasheet) > 6) {
  #   includeBehavior <- TRUE;
  #   useCols <- 6;
  # }


  ### Extract entities of each type and set their IDs
  bcps <- unique(datasheet[, which(usedCols == 'bcps')]);
  bcp_ids <- 1:length(bcps) + max(col_ids);
  names(bcp_ids) <- bcps;
  bcps <- sapply(bcps, function(x)
    return(sapply(x, function(xx)
      return(paste0(strwrap(xx, maxLabelLength), collapse="\n")))));

  if ('cnds' %in% usedCols) {
    cnds <- unique(datasheet[, which(usedCols=='cnds')]);
    cnds <- sapply(cnds, function(x)
      return(sapply(x, function(xx)
        return(paste0(strwrap(xx, maxLabelLength), collapse="\n")))));
  }

  apps <- unique(datasheet[, which(usedCols == 'apps')]);
  app_ids <- 1:length(apps) + max(bcp_ids);
  names(app_ids) <- apps;
  apps <- sapply(apps, function(x)
    return(sapply(x, function(xx)
      return(paste0(strwrap(xx, maxLabelLength), collapse="\n")))));

  sdts <- unique(datasheet[, which(usedCols == 'sdts')]);
  sdt_ids <- 1:length(sdts) + max(app_ids);
  names(sdt_ids) <- sdts;
  sdts <- sapply(sdts, function(x)
    return(sapply(x, function(xx)
      return(paste0(strwrap(xx, maxLabelLength), collapse="\n")))));

  dets <- unique(datasheet[, c(which(usedCols == 'dets'),
                               which(usedCols == 'pobs'))])[, 1];
  det_ids <- 1:length(dets) + max(sdt_ids);
  ### For the names, attach the performance objectives
  names(det_ids) <- apply(unique(datasheet[, c(which(usedCols == 'dets'),
                                               which(usedCols == 'pobs'))]),
                          1, paste, collapse="_");
  dets <- sapply(dets, function(x)
    return(sapply(x, function(xx)
      return(paste0(strwrap(xx, maxLabelLength), collapse="\n")))));

  pobs <- unique(datasheet[, which(usedCols == 'pobs')]);
  pob_ids <- 1:length(pobs) + max(det_ids);
  names(pob_ids) <- pobs;
  pobs <- sapply(pobs, function(x)
    return(sapply(x, function(xx)
      return(paste0(strwrap(xx, maxLabelLength), collapse="\n")))));

  if ('behs' %in% usedCols) {
    behs <- unique(datasheet[, which(usedCols == 'behs')]);
    beh_ids <- 1:length(behs) + max(pob_ids);
    names(beh_ids) <- behs;
    behs <- sapply(behs, function(x)
      return(sapply(x, function(xx)
        return(paste0(strwrap(xx, maxLabelLength), collapse="\n")))));
  }

  ### Combine into one list for vectorized processing
  entity_labels <- list(bcps = bcps,
                        apps = apps,
                        sdts = sdts,
                        dets = dets,
                        pobs = pobs);
  if ('behs' %in% usedCols) {
    entity_labels <- c(entity_labels,
                       list(behs = behs));
  }

  entity_ids <- list(bcps = bcp_ids,
                     apps = app_ids,
                     sdts = sdt_ids,
                     dets = det_ids,
                     pobs = pob_ids);
  if ('behs' %in% usedCols) {
    entity_ids <- c(entity_ids,
                    list(behs = beh_ids));
  }

  node_names <- c('bcps', 'apps', 'sdts', 'dets', 'pobs');
  edge_names <- c('bcp_app_edges',
                  'app_sdt_edges',
                  'sdt_det_edges',
                  'det_pob_edges');
  if ('behs' %in% usedCols) {
    node_names <- c(node_names,
                    'behs');
    edge_names <- c(edge_names,
                    'pob_beh_edges');
  }

  ### Create a datasheet with IDs instead of labels
  datasheet_ids <- data.frame(bcps = bcp_ids[datasheet[, which(usedCols == 'bcps')]],
                              apps = app_ids[datasheet[, which(usedCols == 'apps')]],
                              sdts = sdt_ids[datasheet[, which(usedCols == 'sdts')]],
                              dets = det_ids[apply(datasheet[,
                                                             c(which(usedCols == 'dets'),
                                                               which(usedCols == 'pobs'))],
                                                   1, paste, collapse="_")],
                              pobs = pob_ids[datasheet[, which(usedCols == 'pobs')]]);

  if ('behs' %in% usedCols) {
    datasheet_ids[, 'behs'] <- beh_ids[datasheet[, which(usedCols == 'behs')]];
  }

  bcp_app_edges <- unique(datasheet_ids[, 1:2]);
  app_sdt_edges <- unique(datasheet_ids[, 2:3]);
  sdt_det_edges <- unique(datasheet_ids[, 3:4]);
  det_pob_edges <- unique(datasheet_ids[, 4:5]);

  if ('behs' %in% usedCols) {
    pob_beh_edges <- unique(datasheet_ids[, 5:6]);
  }

  ######################################################################
  ### Start on the node data frames
  ######################################################################

  nodeAttributes <- list(bcps = list(shape = 'box',
                                     color = "#000000",
                                     fillcolor = "#FFFFFF",
                                     style="rounded,filled"),
                         apps = list(shape = 'box',
                                     color = "#000000",
                                     fillcolor = "#FFFFFF",
                                     style="filled"),
                         sdts = list(shape = 'box',
                                     color = "#EEEEEE",
                                     fillcolor = "#EEEEEE",
                                     style="filled"),
                         dets = list(shape = 'ellipse',
                                     color = "#000000",
                                     fillcolor = "#FFFFFF",
                                     style = "filled"),
                         pobs = list(shape = 'box',
                                     color = "#000000",
                                     fillcolor = "#FFFFFF",
                                     style="rounded,filled"));

  if ('behs' %in% usedCols) {
    nodeAttributes <-
      c(nodeAttributes,
        list(behs = list(shape = 'box',
                         color = "#000000",
                         fillcolor = "#FFFFFF",
                         style="rounded,filled")));
  }

  node_dfs <-
    lapply(node_names,
           function(i) {
             return(DiagrammeR::create_node_df(n=length(entity_ids[[i]]),
                                               label=entity_labels[[i]],
                                               type=i,
                                               style=nodeAttributes[[i]]$style,
                                               color=nodeAttributes[[i]]$color,
                                               fillcolor=nodeAttributes[[i]]$fillcolor,
                                               fontcolor="#000000",
                                               fixedsize=FALSE,
                                               shape=nodeAttributes[[i]]$shape));
           });

  if (includeColNames) {
    colNames_node_df <-
      list(DiagrammeR::create_node_df(n=length(col_ids),
                                      label=colNames,
                                      type="colName",
                                      style="filled",
                                      color="#FFFFFF",
                                      fillcolor="#FFFFFF",
                                      fontcolor="#000000",
                                      fixedsize=FALSE,
                                      shape="plaintext"));
    final_nodeDf <- do.call(DiagrammeR::combine_ndfs,
                            c(colNames_node_df,
                              node_dfs));
  } else {
    final_nodeDf <- do.call(DiagrammeR::combine_ndfs,
                            node_dfs);
  }

  ######################################################################
  ### Start on the edge data frames
  ######################################################################

  edges_from <- c(bcp_app_edges[, 1],
                  app_sdt_edges[, 1],
                  sdt_det_edges[, 1],
                  det_pob_edges[, 1]);
  edges_to <- c(bcp_app_edges[, 2],
                app_sdt_edges[, 2],
                sdt_det_edges[, 2],
                det_pob_edges[, 2]);

  edges_from <- list(bcp_app_edges = bcp_app_edges[, 1],
                     app_sdt_edges = app_sdt_edges[, 1],
                     sdt_det_edges = sdt_det_edges[, 1],
                     det_pob_edges = det_pob_edges[, 1]);
  edges_to <- list(bcp_app_edges = bcp_app_edges[, 2],
                   app_sdt_edges = app_sdt_edges[, 2],
                   sdt_det_edges = sdt_det_edges[, 2],
                   det_pob_edges = det_pob_edges[, 2]);

  if ('behs' %in% usedCols) {
    edges_from <- c(edges_from,
                    pob_beh_edges[, 1]);
    edges_to <- c(edges_to,
                  pob_beh_edges[, 2]);
    edges_from <- c(edges_from,
                    list(pob_beh_edges = pob_beh_edges[, 1]));
    edges_to <- c(edges_to,
                  list(pob_beh_edges = pob_beh_edges[, 2]));
  }

  ### Set edge attributes so they can be different
  ### for the edges from and to different types of nodes
  edgeAttributes <-
    list(bcp_app_edges = list(arrowhead = 'icurve',
                              label=letters[seq_along(edges_to$bcp_app_edges)],
                              tooltip="The parameters for use have to be explained separately",
                              color = "#000000"),
         app_sdt_edges = list(arrowhead = 'normal',
                              label="",
                              tooltip="Influence",
                              color = "#000000"),
         sdt_det_edges = list(arrowhead = 'dot',
                              label="",
                              tooltip="Is a part of",
                              color = "#000000"),
         det_pob_edges = list(arrowhead = 'normal',
                              label="",
                              tooltip="Influences",
                              color = "#000000"));

  if ('cnds' %in% usedCols) {
    edgeAttributes$bcp_app_edges$label <- cnds;
    edgeAttributes$bcp_app_edges$tooltip <- cnds;
  }

  ### If the ABCD includes both PO's and behavior, add
  ### the edges to the behavior
  if ('behs' %in% usedCols) {
    edgeAttributes <-
      c(edgeAttributes,
        list(pob_beh_edges = list(arrowhead = 'dot',
                                  label="",
                                  tooltip="Is a part of",
                                  color = "#000000")));
  }

  edge_dfs <-
    lapply(edge_names,
           function(i) {
             return(DiagrammeR::create_edge_df(from=edges_from[[i]],
                                               to=edges_to[[i]],
                                               label=edgeAttributes[[i]]$label,
                                               tooltip=edgeAttributes[[i]]$tooltip,
                                               arrowhead=edgeAttributes[[i]]$arrowhead,
                                               color=edgeAttributes[[i]]$color));
           });

  if (includeColNames) {
    colNames_edge_df <-
      list(DiagrammeR::create_edge_df(from=1:(length(col_ids)-1),
                                      to=2:length(col_ids),
                                      color = "#FFFFFF"));
    final_edgeDf <- do.call(DiagrammeR::combine_edfs,
                            c(colNames_edge_df,
                              edge_dfs));
  } else {
    final_edgeDf <- do.call(DiagrammeR::combine_edfs,
                            edge_dfs);
  }

  ######################################################################
  ### Create final graph, set attributes, and return the result
  ######################################################################

  res$output$graph <-
    DiagrammeR::create_graph(nodes_df = final_nodeDf,
                             edges_df = final_edgeDf,
                             graph_name = title);

  res$output$graph <-
    DiagrammeR::add_global_graph_attrs(res$output$graph,
                                       "layout",
                                       "dot", "graph");
  res$output$graph <-
    DiagrammeR::add_global_graph_attrs(res$output$graph,
                                       "rankdir",
                                       "LR", "graph");
  res$output$graph <-
    DiagrammeR::add_global_graph_attrs(res$output$graph,
                                       "outputorder",
                                       "nodesfirst", "graph");

  if (!is.null(outputFile)) {
    for (currentFile in outputFile) {
      DiagrammeR::export_graph(res$output$graph,
                               file_name = currentFile,
                               file_type = tools::file_ext(currentFile),
                               width=outputWidth,
                               height=outputHeight,
                               title = DiagrammeR::get_graph_name(res$output$graph));
    }
  }

  class(res) <- "abcdiagram";

  return(res);

}

#' @export
print.abcdiagram <- function(x, ...) {
  print(DiagrammeR::render_graph(x$output$graph,
                                 width=x$input$width,
                                 height=x$input$height,
                                 title = DiagrammeR::get_graph_name(x$output$graph)),
        ...);
}

#' Two simple example datasets for ABCD's
#'
#' This are two (nested) datasets illustrating the logic model of change for
#' a simple condom use intervention in a way that can be visualised using
#' the [abcd] function. The full dataset is `abcd_specs_full`, and a subset
#' that only contains the information about one sub-behavior (performance
#' objective) is available as `abcd_specs_single_po`. The variables in the full
#' dataset are:
#'
#' * `Behavior Change Principles`: The behavior change principles (BCPs), also known as methods for behavior change or 'behavior change techniques' (BCTs), that describe the psychological principles that are assumed to realise the change in the (sub-)determinants.
#' * `Applications`: The applications of these BCPs. Where the BCPs describe theoretical principles, the applications are more or less tangible intervention elements.
#' * `Sub-determinants\\n(e.g. beliefs; can be formulated as Change Objectives)`: The specific aspects of teh target population's psychology that are targeted by the BCPs (e.g. beliefs, or in Intervention Mapping vocabulary, Change Objectives).
#' * `Determinants`: The determinants, psychological constructs, that the targeted sub-determinants are a part of, and that together predict the Performance Objectives (sub-behaviors).
#' * `Performance Objectives`: Explicitly defined sub-behaviors at a level of specificity that distinguishes them from other sub-behaviors, and that together form the target behavior.
#' * `Target Behavior`: The ultimate target behavior, usually defined at a relatively general level.
#'
#' @docType data
#' @aliases abcd_specs_full abcd_specs_single_po
#' @keywords data
#' @name abcd_specs_examples
#' @usage data(abcd_specs_full)
#' @usage data(abcd_specs_single_po)
#' @format For `abcd_specs_full`, a data frame with 6 variables and 7 rows;
#' for `abcd_specs_single_po`, a data frame with 5 variables and 4 rows.
c("abcd_specs_single_po", "abcd_specs_full");

# abcd_specs_single_po <-
#   data.frame(c("Social comparison",
#                "Persuasive communication",
#                "Modeling",
#                "Guided practice"),
#              c("A chart showing percentage of people using condoms.",
#                "Quotations from people expressing approval.",
#                "A role model illustrates bringing up condoms in different settings.",
#                "In a mini-game, target population individuals practice negotiation."),
#              c("Most people use condoms.",
#                "Most people approve of me suggesting to use condoms.",
#                "It is easy for me to bring up condoms.",
#                "If my partner is not enthusiastic, I know I can persuade them."),
#              c("Perceived Norms",
#                "Perceived Norms",
#                "Self-efficacy",
#                "Self-efficacy"),
#              c("Negotiate condom use",
#                "Negotiate condom use",
#                "Negotiate condom use",
#                "Negotiate condom use"));
#
# abcd_specs_full <-
#   data.frame(c("Persuasive communication",
#                "Persuasive communication",
#                "Persuasive communication",
#                "Social comparison",
#                "Persuasive communication",
#                "Modeling",
#                "Guided practice"),
#              c("An infographic explains how condoms work.",
#                "An infographic explains how condoms work.",
#                "Quotations from people expressing approval.",
#                "A chart showing percentage of people using condoms.",
#                "Quotations from people expressing approval.",
#                "A role model illustrates bringing up condoms in different settings.",
#                "In a mini-game, target population individuals practice negotiation."),
#              c("Condoms help prevent HIV.",
#                "Condoms help prevent pregnancy.",
#                "Most people think buying condoms is normal.",
#                "Most people use condoms.",
#                "Most people approve of me suggesting to use condoms.",
#                "It is easy for me to bring up condoms.",
#                "If my partner is not enthusiastic, I know I can persuade them."),
#              c("Attitude",
#                "Attitude",
#                "Perceived Norms",
#                "Perceived Norms",
#                "Perceived Norms",
#                "Self-efficacy",
#                "Self-efficacy"),
#              c("Buy condoms",
#                "Buy condoms",
#                "Buy condoms",
#                "Negotiate condom use",
#                "Negotiate condom use",
#                "Negotiate condom use",
#                "Negotiate condom use"),
#              c("Condom use",
#                "Condom use",
#                "Condom use",
#                "Condom use",
#                "Condom use",
#                "Condom use",
#                "Condom use"),
#              stringsAsFactors = FALSE);
#
# names(abcd_specs_full) <-
#   c('Behavior Change Principles',
#     'Applications',
#     'Sub-determinants\n(e.g. beliefs; can be formulated as Change Objectives)',
#     'Determinants',
#     'Performance Objectives',
#     'Target Behavior');
# names(abcd_specs_single_po) <-
#   names(abcd_specs_full)[1:5];
#
# devtools::use_data(abcd_specs_full, abcd_specs_single_po);

# abcd_complete <- abcd(specs=c("https://docs.google.com/spreadsheets/d/1U1j-VoiK3WmfveJ7VpUMY_H9WNXDh85a8jKbM67AQSI/edit?usp=sharing",
#                               "C:/Sync/Data/statistics/R/library/tmp/abcd-complete.csv"),
#                       localBackup="C:/Sync/Data/statistics/R/library/tmp/abcd-complete.csv",
#                       outputFile=c("C:/Sync/Data/statistics/R/library/tmp/abcd-complete.svg",
#                                    "C:/Sync/Data/statistics/R/library/tmp/abcd-complete.png"));
#
# abcd_full <- abcd(specs=c("https://docs.google.com/spreadsheets/d/13VE1_1Oa38CidDDbiuIw7ZP8DIJ2qzs_i8wlJ63YuMI",
#                           "C:/Sync/Data/statistics/R/library/tmp/abcd-full.csv"),
#                   localBackup="C:/Sync/Data/statistics/R/library/tmp/abcd-full.csv",
#                   outputFile=c("C:/Sync/Data/statistics/R/library/tmp/abcd-full.svg",
#                                "C:/Sync/Data/statistics/R/library/tmp/abcd-full.png"));
#
# abcd_single_po <- abcd(specs=c("https://docs.google.com/spreadsheets/d/1ib4CJlWUYcShwwue8kXq2519tTefK6T-orLarhuk0q0",
#                                "C:/Sync/Data/statistics/R/library/tmp/abcd-single-po.csv"),
#                        localBackup="C:/Sync/Data/statistics/R/library/tmp/abcd-single-po.csv",
#                        outputFile=c("C:/Sync/Data/statistics/R/library/tmp/abcd-single-po.svg",
#                                     "C:/Sync/Data/statistics/R/library/tmp/abcd-single-po.png"));
