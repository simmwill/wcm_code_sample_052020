#' Estimates predefined logistic model with customizable outcome
#'
#' Function that allows iterative specification of CHD outcome in multivariable logistic model testing exposure interaction.
#'   Specified data frame must contain all necessary variables for fitted models; see below for more details.
#'
#' @param outcome 1 of 6 congenital heart defects. Must be quoted: `"lvoto"`, `"rvoto_noebstein"`, `"conotruncal"`,
#'   `"septal"`, `"asd2"`, `"vsdpm"`.
#' @param df Data frame to be used for fitted models. Since this is an illustrative example, the data frame is needs to be
#'   prespecified to a greater extent than would be ideal in a more iterative setting. The data frame in this instance must
#'   contain indicator variables for all six CHD outcomes in this study (see `outcome` argument), both relevant exposure variables,
#'   and all relevant model covariates (see below for specific variables).
#'
#' @return Output from multivariable logistic model (called with `glm(..., family = "binomial")`) estimating log-ods of
#'   `outcome` definedabove. RHS model terms include `pm25mean`, `heat_days_90`, their cross-product, and predetermined
#'   covariates (`mean_dp`, `age_cat`, `race_cat`, `educ_cat`).
#'
#' @examples
#' interaction_model(study_data_mod, "lvoto")
#' interaction_model(study_data_mod, "asd2")

interaction_model = function(df, outcome) {

  glm(
    data = df,
    formula =
      reformulate(
        "pm25mean + heat_days_90 + pm25mean*heat_days_90 + mean_dp + age_cat + race_cat + educ_cat",  # RHS variables
        paste(outcome)  # LHS variable: one of ("lvoto", "rvoto", "conotruncal", "septal", "asd2", "vsdpm")
      ),
    family = "binomial"
  )

}
