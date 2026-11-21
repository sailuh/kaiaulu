# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Train a sentiment model using PySenti
#'
#' @description
#' Trains a transformer-based sentiment model (e.g., BERT, XLNet, RoBERTa,
#' ALBERT) by passing a labeled reply data.table to the external PySenti
#' Python script. The script splits the data into train/validation/test sets,
#' trains the model, evaluates it on the test set, and writes the metrics to a
#' JSON file, which this function reads back.
#'
#' Use \code{\link{pysenti_metrics_summary}}, \code{\link{pysenti_per_class_metrics}},
#' \code{\link{pysenti_training_history}} and \code{\link{pysenti_confusion_matrix}}
#' to turn the returned `metrics` into tables.
#'
#' @param conda_path Path to conda if pysenti is setup in a conda env (optional)
#' @param pysenti_conda_env_name pysenti conda env name if pysenti is setup in a conda env (required only if conda path specified)
#' @param pysenti_path Path to the PySenti training Python script.
#' @param train_data_path Path to the model train .csv with at least `text` and `polarity` columns.
#' @param model_save_path Path where the trained model will be stored.
#' @param model_name Model architecture name; `"bert-base-cased"`, `"xlnet-base-cased", etc.
#' @param metrics_save_path Path of the JSON metrics file written by PySenti. Defaults to
#' `model_save_path` without its extension, followed by `_metrics.json`.
#' @return A list with `log`, a character vector of the Python process stdout/stderr, and
#' `metrics`, a list parsed from the metrics JSON (`model_name`, `split_sizes`, `history`, `test`).
#' @export
pysenti_train_model <- function(conda_path = "",
                                pysenti_conda_env_name = "",
                                pysenti_path,
                                train_data_path,
                                model_save_path,
                                model_name,
                                metrics_save_path = paste0(sub("\\.[^./]*$", "", model_save_path),
                                                           "_metrics.json")) {

  script_args <- c(
    pysenti_path,
    "--input", train_data_path,
    "--model_name", model_name,
    "--output", model_save_path,
    "--metrics_output", metrics_save_path
  )

  # Remove stale metrics so a failed run cannot be mistaken for a successful one
  if (file.exists(metrics_save_path)) file.remove(metrics_save_path)

  if(conda_path != ""){
    args <- c("run", "-n", pysenti_conda_env_name, "python", script_args)
    res <- system2(conda_path, args = args, stdout = TRUE, stderr = TRUE)
  }
  else{
    res <- system2("python", args = script_args, stdout = TRUE, stderr = TRUE)
  }

  status <- attr(res, "status")
  if ((!is.null(status) && status != 0) || !file.exists(metrics_save_path)) {
    stop("PySenti training failed. Last lines of output:\n",
         paste(utils::tail(res, 30), collapse = "\n"))
  }

  metrics <- jsonlite::fromJSON(metrics_save_path)
  list(log = res, metrics = metrics)
}

#' Summarize PySenti test-set metrics
#'
#' @param metrics The `metrics` element returned by \code{\link{pysenti_train_model}}.
#' @return A one-row data.table with weighted `accuracy`, `precision`, `recall` and `f1`
#' on the test set, plus the train/validation/test split sizes.
#' @export
pysenti_metrics_summary <- function(metrics) {
  test <- metrics$test
  data.table::data.table(
    accuracy = test$accuracy,
    precision = test$precision,
    recall = test$recall,
    f1 = test$f1,
    n_train = metrics$split_sizes$train,
    n_validation = metrics$split_sizes$validation,
    n_test = metrics$split_sizes$test
  )
}

#' Per-class PySenti test-set metrics
#'
#' @param metrics The `metrics` element returned by \code{\link{pysenti_train_model}}.
#' @return A data.table with one row per sentiment label and columns
#' `label`, `precision`, `recall`, `f1` and `support`.
#' @export
pysenti_per_class_metrics <- function(metrics) {
  data.table::as.data.table(metrics$test$per_class)
}

#' PySenti per-epoch training history
#'
#' @param metrics The `metrics` element returned by \code{\link{pysenti_train_model}}.
#' @return A data.table with one row per epoch and columns `epoch`, `train_loss`,
#' `train_accuracy`, `val_accuracy` and `val_f1`.
#' @export
pysenti_training_history <- function(metrics) {
  data.table::as.data.table(metrics$history)
}

#' PySenti test-set confusion matrix
#'
#' @param metrics The `metrics` element returned by \code{\link{pysenti_train_model}}.
#' @return A long data.table with columns `true`, `predicted` and `n`, one row per
#' cell of the confusion matrix. `true` and `predicted` are factors in label order
#' (neutral, positive, negative), ready for plotting with e.g. ggplot2::geom_tile().
#' @export
pysenti_confusion_matrix <- function(metrics) {
  labels <- metrics$test$labels
  # jsonlite::fromJSON simplifies the nested list to a matrix: rows = true, cols = predicted.
  # as.integer() flattens column-major, so `true` varies fastest.
  cm <- as.matrix(metrics$test$confusion_matrix)
  data.table::data.table(
    true = factor(rep(labels, times = length(labels)), levels = labels),
    predicted = factor(rep(labels, each = length(labels)), levels = labels),
    n = as.integer(cm)
  )
}

#' Predict sentiment using a trained PySenti model
#'
#' @description
#' Applies a trained PySenti model to a reply data.table, writes predictions
#' to a CSV file, and returns them as a data.table.
#'
#' @param conda_path Path to conda if pysenti is setup in a conda env (optional)
#' @param pysenti_conda_env_name pysenti conda env name if pysenti is setup in a conda env (required only if conda path specified)
#' @param pysenti_path Path to the PySenti prediction Python script.
#' @param reply_dt A data.table containing a `text` column.
#' @param model_save_path Path to the trained model folder.
#' @param prediction_path Directory where the prediction CSV will be written.
#' @param model_name Model architecture name; `"bert-base-cased"`, `"xlnet-base-cased", etc.
#' @return A data.table containing predicted sentiment labels.
#' @export
pysenti_predict <- function(conda_path = "",
                            pysenti_conda_env_name = "",
                            pysenti_path,
                            reply_dt,
                            model_save_path,
                            prediction_save_path,
                            model_name) {

  data.table::fwrite(reply_dt, prediction_save_path)


  if(conda_path != ""){
    args <- c(
      "run",
      "-n",
      pysenti_conda_env_name,
      "python",
      pysenti_path,
      "--input", prediction_save_path,
      "--model_name", model_name,
      "--model_path", model_save_path,
      "--output", prediction_save_path
    )
    res <- system2(conda_path, args = args, stdout = TRUE, stderr = TRUE)
  }
  else{
    args <- c(
      pysenti_path,
      "--input", prediction_save_path,
      "--model_name", model_name,
      "--model_path", model_save_path,
      "--output", prediction_save_path
    )
    res <- system2("python", args = args, stdout = TRUE, stderr = TRUE)
  }
  # stringi::stri_c(c("conda",args), collapse = " ")

  data.table::fread(prediction_save_path)
}
