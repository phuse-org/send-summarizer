#AddUIDep function used in tox summary app

addUIDep <- function(x) {
  jqueryUIDep <- htmltools::htmlDependency("jqueryui", "1.10.4",
                                           c(href = "shared/jqueryui/1.10.4"),
                                           script = "jquery-ui.min.js",
                                           stylesheet = "jquery-ui.min.css"
  )
  htmltools::attachDependencies(x, c(htmltools::htmlDependencies(x), list(jqueryUIDep)))
}