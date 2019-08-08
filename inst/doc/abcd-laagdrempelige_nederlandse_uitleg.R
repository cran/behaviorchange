## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
);
require('magrittr');

## ----raa, echo=FALSE, fig.width=6, fig.height=3.5, fig.cap="Figuur 1: De reasoned action approach."----

raaGraph <-
  DiagrammeR::create_graph() %>%
  DiagrammeR::add_node(label="Gedrag") %>%
  DiagrammeR::add_node(label="Intentie", to=1) %>%
  DiagrammeR::add_node(label="Attitude", to=2) %>%
  DiagrammeR::add_node(label="Waargenomen norm", to=2) %>%
  DiagrammeR::add_node(label="Waargenomen gedragscontrole", to=2) %>%
  behaviorchange::apply_graph_theme(c("layout", "dot", "graph"),
                                    c("rankdir", "LR", "graph"),
                                    c("outputorder", "nodesfirst", "graph"),
                                    c("fixedsize", "false", "node"),
                                    c("shape", "box", "node"),
                                    c("style", "rounded,filled", "node"),
                                    c("color", "#000000", "node"),
                                    c("color", "#000000", "edge"),
                                    c("dir", "forward", "edge"),
                                    c("fillcolor", "#FFFFFF", "node"));

DiagrammeR::render_graph(raaGraph);


## ----abcd-specificatie, echo=FALSE---------------------------------------
abcd_specs_dutch_xtc <- behaviorchange::abcd_specs_dutch_xtc;

names(abcd_specs_dutch_xtc) <-
  c("Gedrags-veranderings-principes",
    "Voorwaarden voor effectiviteit",
    "Toepassingen",
    "Sub-determinanten",
    "Determinanten",
    "Sub-gedragingen",
    "Doelgedrag");

knitr::kable(abcd_specs_dutch_xtc);

## ----abcd-diagram, echo=FALSE, fig.width=14, fig.height=7, eval=FALSE----
#  abcd_specs_dutch_xtc_graph <-
#    behaviorchange::abcd(behaviorchange::abcd_specs_dutch_xtc);
#  print(abcd_specs_dutch_xtc_graph);
#  DiagrammeR::export_graph(abcd_specs_dutch_xtc_graph$output$graph,
#                           here::here('vignettes', 'abcd_specs_dutch_xtc.png'),
#                           title="Acyclic Behavior Change Diagram",
#                           width=3000,
#                           height=1000);

