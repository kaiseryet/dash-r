rm(list = ls())

library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(mice)


app = Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

colors = list(background = '#000000', text = '#7ECF23')

# add a page title and subtitle
title_main = htmlH1('Regression and Variable Selection Models (dev.)',
                    style = list(textAlign = 'center', 
                                 color = colors$text))

subtitle_main = htmlH2('for Data Analysis and Visualization', 
                       style = list(textAlign = 'center', 
                                    color = colors$text))


# file upload button
upload_button_style = list('width' = '100%', 
                           'height' = '60px', 
                           'lineHeight' = '60px', 
                           'borderWidth' = '1px', 
                           'borderStyle' = 'dashed', 
                           'borderRadius' = '5px', 
                           'textAlign' = 'center', 
                           'margin' = '10px')

upload_data = dccUpload(id = "upload_data", 
                               children = htmlDiv(list('Drag and Drop or ', htmlA('Select csv Files'))), 
                               disabled = FALSE, 
                               max_size = 5000, 
                               min_size = 0, 
                               style = upload_button_style, 
                               multiple=FALSE)


# build the model, yielding parameter estimation

# check for outcome and build the model, with three possible models: logistic, poisson, gaussian
fit = function(dat, outcome) {
  if (length(levels(factor(dat$outcome))) == 2) { # check if logistic model
    dat$outcome = factor(dat$outcome, ordered = TRUE)
    fit_temp = glm(outcome~., family = binomial(link = "logit"), data = dat)
    tab_temp = summary(fit_temp)$coefficients
    est_temp = tab_temp[,1]
    se_temp = tab_temp[,2]
    lower_temp = qnorm(.025, est_temp, se_temp)
    upper_temp = qnorm(.975, est_temp, se_temp)
    pval_temp = tab_temp[,4]
    est_oddsr = exp(est_temp)
    oddsr_lower = exp(lower_temp)
    oddsr_upper = exp(upper_temp)
    tab = data.frame(est_oddsr, oddsr_lower, oddsr_upper, pval_temp)
    names(tab) = c("Estimated Odds Ratio", "95% Lower Bound of Estimation", "95% Upper bound of Estimation", "p-value")
    tab_temp = knitr::kable(tab_temp, digits = 3, caption = paste(c("Odds Estimation of ", levels(dat$outcome)[2]), sep = ""))
    tab = dccMarkdown(children = tab_temp)
    tab = htmlDiv(list(htmlH3(paste(c("Odds Estimation of ", levels(dat$outcome)[2]), sep = "")), htmlHr(), tab), id = "est_tab")
  } else {
    if (is.integer(dat$outcome) == TRUE & all(dat$outcome > -1)) { # check if poisson model for count data
      fit_temp = glm(outcome~., family = poisson(link = "log"), data = dat)
      tab_temp = summary(fit_temp)$coefficients
      est_temp = tab_temp[,1]
      se_temp = tab_temp[,2]
      lower_temp = qnorm(.025, est_temp, se_temp)
      upper_temp = qnorm(.975, est_temp, se_temp)
      pval_temp = tab_temp[,4]
      est_rr = exp(est_temp)
      rr_lower = exp(lower_temp)
      rr_upper = exp(upper_temp)
      tab = data.frame(est_rr, rr_lower, rr_upper, pval_temp)
      names(tab) = c("Estimated Risk Ratio", "95% Lower Bound of Risk Ratio", "95% Upper bound of Risk Ratio", "p-value")
      tab_temp = knitr::kable(tab_temp, digits = 3, caption = paste(c("Risk Ratio of 1 Unit Increase in ", outcome), sep = ""))
      tab = dccMarkdown(children = tab_temp)
      tab = htmlDiv(list(htmlH3(paste(c("Risk Ratio of 1 Unit Increase in ", outcome), sep = "")), htmlHr(), tab), id = "est_tab")
    } else { # only linear model left here
      fit_temp = lm(outcome~., data = dat)
      tab_temp = summary(fit_temp)$coefficients
      est_temp = tab_temp[,1]
      se_temp = tab_temp[,2]
      lower_temp = qnorm(.025, est_temp, se_temp)
      upper_temp = qnorm(.975, est_temp, se_temp)
      pval_temp = tab_temp[,4]
      tab = data.frame(est_temp, lower_temp, upper_temp, pval_temp)
      names(tab) = c("Estimated Coefficients", "95% Lower Bound of Estimation", "95% Upper bound of Estimation", "p-value")
      tab_temp = knitr::kable(tab_temp, digits = 3, caption = paste(c("Linear Model Estimation with ", outcome, " as Response"), sep = ""))
      tab = dccMarkdown(children = tab_temp)
      tab = htmlDiv(list(htmlH3(paste(c("Linear Model Estimation with ", outcome, " as Response"), sep = "")), htmlHr(), tab), id = "est_tab")
    }
  }
  return(tab) # return an html object to include 
}


# check for missing data, and choose to impute or omit missing data
fit_missing = function(dat, outcome, missing_action) {
  dat = read.csv(dat, header = TRUE)
  if (any(is.na(dat)) == TRUE) {
    if (missing_action == "Impute missing data, if any") {
      temp = mice(dat, m = min(c(25, max(colSums(is.na(dat)))/dim(dat)[1])), maxit = 5) # based on White's paper, but not to make too many multiple imputations
      dat = complete(temp, action = min(c(25, max(colSums(is.na(dat)))/dim(dat)[1])))
      tab_output = fit(dat = dat, outcome = outcome)
    } else {
      dat = na.omit(dat)
      tab_output = fit(dat = dat, outcome = outcome)
    }
  } else {
    tab_output = fit(dat = dat, outcome = outcome)
  }
  return(tab_output)
}


# initialize an output table
table_output = dccMarkdown(id = "table_output")


## feature selection with random effects (for future development)
### glmmlasso


### my method here


## what to see? model or diagnostic plot?


# design the dropdown for outcome_spec and missing_action
outcome_spec = dccInput(id = "outcome_spec",
                        value = "Specify the outcome variable",
                        type = "text")

missing_action = dccRadioItems(id = "missing_action", 
                               options = list(list(label = "Impute missing data, if any", value = "Impute missing data, if any"),
                                              list(label = "Omit all observations containing missing data", value = "Omit all observations containing missing data")),
                               value = "Impute missing data, if any",
                               labelStyle = list(display = "inline-block"))


# bring together all the components 
app$layout(htmlDiv(list(title_main, 
                        subtitle_main, 
                        htmlHr(), 
                        outcome_spec,
                        missing_action,
                        upload_data,
                        htmlHr(),
                        htmlDiv(id = "table_output")),
                   style = list(backgroundColor = colors$background)))

# callbacks mutation
app$callback(output = list(id = 'table_output', property='children'), # output mutates the existing object to show the mapping value of params
             params = list(input(id = "upload_data", property = "content"), input(id='outcome_spec', property='text'), input(id='missing_action', property='text')), # params is the input here; it mutates the existing object to bear value
             func = fit_missing) # mapping params to output

# run server 
app$run_server()


