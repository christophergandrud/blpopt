#' Bootstrapped predictions of the CATE-BLP
#'
#' @param obj `cateblp` class object created by `cate_blp`
#' @param iterations integer number of bootstrap iterations
#' @param output character of either `"totals"` or `"raw.list"`. If `"totals"`
#' then the total predicted treatment effect for each boot strap iteration is found
#' using both complete treatment (all units treated) or treatment only when the
#' predicted effect is greater than `tau.treatment.baseline`.
#' @param tau.treatment.baseline numeric. Treatment is only made for units when the
#' predicted treatment effect is > `tau.treatment.baseline`.
#'
#' @importFrom stats formula predict.lm
#' @importFrom dplyr bind_rows %>% group_by summarize
#'
#' @export

cate_blp_bootstrap <- function(obj,
                               iterations = 100,
                               output = "totals",
                               tau.treatment.baseline = 0)
{
    if (!inherits(obj, "cateblp")) stop("obj must be created by cate_blp",
                                        call. = FALSE)

    predictions <- predicted_optimal <- NULL

    m <- obj$fitted_model
    m_formula <- formula(m)
    original_data <- m$model
    original_nrow <- nrow(original_data)

    out_list <- vector(mode = "list", length = iterations)

    show_progress <- function(i_) {
        intervaln <- floor(iterations * 0.1)
        if (floor(i_/intervaln) == i_/intervaln) {
            cat(paste("[", i_, "/", iterations, "]\r"))
        }
    }

    for (i in 1:iterations) {
        show_progress(i)
        samp <- original_data[sample(original_nrow, size = original_nrow, replace = TRUE), ]
        m_samp <- predict.lm(lm(m_formula, data = samp))

        X_samp <- data.frame(samp[, -1])
        names(X_samp) <- names(samp)[-1]
        out_list[[i]] <- data.frame(sample = paste0("sample_", i),
                                    X_samp, predictions = m_samp)
    }
    if (output == "raw.list") {
        return(out_list)
    } else if (output == "totals") {

        df <- dplyr::bind_rows(out_list, .id = "sample")
        df$predicted_optimal <- ifelse(df$prediction < tau.treatment.baseline,
                                       0,
                                       df$prediction)
        out_df <- df %>% group_by(sample) %>%
            summarize(predicted_totals = sum(predictions),
                      predicted_blp_optimal_totals = sum(predicted_optimal))
        return(out_df)
    }
}
