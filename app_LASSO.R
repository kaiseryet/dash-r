library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(glmnet)
library(ncpen)
library(knitr)
library(pgmm)
data("wine")


Gringnolino = as.numeric(wine$Type==2)
Gringnolino = factor(Gringnolino)
wine = cbind(Gringnolino, wine[,-1])
fit1 = glmnet(as.matrix(wine[,-1]), Gringnolino, family = "binomial")
tab1 = as.matrix(coef(fit1))
tab1_names = round(log(fit1$lambda),3)
tab1 = knitr::kable(tab1, col.names = tab1_names, digits = 3, caption = "LASSO fit for wine dataset with different log lambda values")


fit2 = ncpen(as.numeric(Gringnolino)-1, as.matrix(wine[,-1]), family = "binomial", penalty = "scad", lambda = .01)
tab2 = knitr::kable(coef(fit2))


app <- Dash$new()

markdown_text <- 

#app$layout(
#  htmlDiv(
#    list(
#      dccMarkdown(children=tab1)
#    )
#  )
#)

app$layout(
  htmlDiv(
    list(
      htmlH4(children="LASSO fit for wine dataset with different log lambda values"),
      dccMarkdown(children=tab1)
    )
  )
)

app$run_server(showcase = TRUE)




