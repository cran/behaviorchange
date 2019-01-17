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
knitr::kable(behaviorchange::abcd_specs_dutch_xtc,
             format="html",
             table.attr="style='font-size:85%;'");

## ----abcd-diagram, echo=FALSE, fig.width=14, fig.height=7----------------
print(behaviorchange::abcd(behaviorchange::abcd_specs_dutch_xtc));

