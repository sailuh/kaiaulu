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
#' Python script.
#'
#' @param pysenti_path Path to the PySenti Python script.
#' @param reply_dt A data.table with at least `text` and `polarity` columns.
#' @param model_save_path Directory where the trained model will be stored.
#' @param model Model architecture name; one of `"bert"`, `"xlnet"`, `"roberta"`, or `"albert"`.
#' @return Character vector containing stdout/stderr from the Python process.
#' @export
pysenti_train_model <- function(pysenti_path,
                                reply_dt,
                                model_save_path,
                                model) {

  tmp_train <- tempfile(fileext = ".csv")
  data.table::fwrite(reply_dt, tmp_train)

  if (!file.exists(pysenti_path)) {
    stop("Python script not found at: ", pysenti_path)
  }

 model_save_path <- file.path(sub("/$", "", model_save_path), paste0(model, "_model"))

  args <- c(
    pysenti_path,
    "--mode", "train",
    "--input", tmp_train,
    "--model", model,
    "--output", model_save_path
  )

  res <- system2("python", args = args, stdout = TRUE, stderr = TRUE)

  # Extract and print the line that starts with "MODEL_SAVED_AT:"
  final_line <- grep("^MODEL_SAVED_AT:", res, value = TRUE)
  sub("^MODEL_SAVED_AT:\\s*", "", final_line)
}

#' Predict sentiment using a trained PySenti model
#'
#' @description
#' Applies a trained PySenti model to a reply data.table, writes predictions
#' to a CSV file, and returns them as a data.table.
#'
#' @param pysenti_path Path to the PySenti Python script.
#' @param reply_dt A data.table containing a `text` column.
#' @param model_save_path Path to the trained model folder.
#' @param prediction_path Directory where the prediction CSV will be written.
#' @param model Model architecture name; one of `"bert"`, `"xlnet"`, `"roberta"`, or `"albert"`.
#' @return A data.table containing predicted sentiment labels.
#' @export
pysenti_predict <- function(pysenti_path,
                            reply_dt,
                            model_save_path,
                            prediction_path,
                            model) {

  tmp_input <- tempfile(fileext = ".csv")
  data.table::fwrite(reply_dt, tmp_input)

  model_save_path <- file.path(sub("/$", "", model_save_path), paste0(model, "_model"))
  # Timestamp to make the filename unique
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_file <- file.path(sub("/$", "", prediction_path), paste0(model, "_prediction_", timestamp, ".csv"))

  args <- c(
    pysenti_path,
    "--mode", "predict",
    "--input", tmp_input,
    "--model", model,
    "--model_path", model_save_path,  
    "--output", output_file
  )

  res <- system2("python", args = args, stdout = TRUE, stderr = TRUE)

  cat(grep("^Using model file:", res, value = TRUE), "\n")
  cat(grep("^PREDICTION_SAVED_AT:", res, value = TRUE), "\n")
  
  data.table::fread(output_file)
}


