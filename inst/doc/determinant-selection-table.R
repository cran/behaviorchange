## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = ""
)

## -----------------------------------------------------------------------------
dat <- get(data("BBC_pp15.1", package="behaviorchange"));

## -----------------------------------------------------------------------------
subdets <-
  grep(
    "highDose_AttBeliefs_",
    names(dat),
    value=TRUE
  );

## ---- eval=TRUE, echo=FALSE---------------------------------------------------

oldKableViewOption <- getOption("kableExtra_view_html", NULL);
options(kableExtra_view_html = FALSE);

if (!exists('headingLevel') || !is.numeric(headingLevel) || (length(headingLevel) != 1)) {
  headingLevel <- 0;
}


## ---- results='asis', eval=TRUE-----------------------------------------------
ufs::heading("Determinant Selection Table",
             headingLevel=headingLevel);

## ---- echo=echoPartial, results='asis', eval=TRUE-----------------------------

if (requireNamespace("kableExtra", quietly = TRUE)) {
  print(
    prettyDeterminantSelectionTable(
      x,
      digits=digits
    )
  );
} else {
  knitr::kable(x,
               digits=digits,
               row.names=FALSE);
}


## ----echo=FALSE---------------------------------------------------------------
if (!is.null(oldKableViewOption)) {
  options(kableExtra_view_html = oldKableViewOption);
}

## -----------------------------------------------------------------------------
behaviorchange::determinant_selection_table(
  data=dat,
  determinants = subdets,
  target = 'highDose_intention'
);

## ---- eval=TRUE, echo=FALSE---------------------------------------------------

oldKableViewOption <- getOption("kableExtra_view_html", NULL);
options(kableExtra_view_html = FALSE);

if (!exists('headingLevel') || !is.numeric(headingLevel) || (length(headingLevel) != 1)) {
  headingLevel <- 0;
}


## ---- results='asis', eval=TRUE-----------------------------------------------
ufs::heading("Determinant Selection Table",
             headingLevel=headingLevel);

## ---- echo=echoPartial, results='asis', eval=TRUE-----------------------------

if (requireNamespace("kableExtra", quietly = TRUE)) {
  print(
    prettyDeterminantSelectionTable(
      x,
      digits=digits
    )
  );
} else {
  knitr::kable(x,
               digits=digits,
               row.names=FALSE);
}


## ----echo=FALSE---------------------------------------------------------------
if (!is.null(oldKableViewOption)) {
  options(kableExtra_view_html = oldKableViewOption);
}

## -----------------------------------------------------------------------------
behaviorchange::determinant_selection_table(
  data=dat,
  determinants = subdets,
  target = 'highDose_intention',
  sortBy=6
);

