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
#' @param model Model architecture name; `"bert-base-cased"`, `"xlnet-base-cased", etc.
#' @return Character vector containing stdout/stderr from the Python process.
#' @export
pysenti_train_model <- function(pysenti_path,
                                reply_dt,
                                model_save_path,
                                model_name) {

  tmp_train <- tempfile(fileext = ".csv")
  data.table::fwrite(reply_dt, tmp_train)

  if (!file.exists(pysenti_path)) {
    stop("Python script not found at: ", pysenti_path)
  }

  model_save_path <- paste0(model_save_path, model_name)

  args <- c(
    pysenti_path,
    "--mode", "train",
    "--input", tmp_train,
    "--output", model_save_path,
    "--model_name", model_name
  )

  res <- system2("python", args = args, stdout = TRUE, stderr = TRUE)

  # Extract line that starts with "MODEL_SAVED_AT:"
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
#' @param model Model architecture name; `"bert-base-cased"`, `"xlnet-base-cased", etc.
#' @param timestamp Character string representing the current time, formatted as "YYYYMMDD_HHMMSS", used to append to the prediction file name for uniqueness.
#' @return A data.table containing predicted sentiment labels.
#' @export
pysenti_predict <- function(pysenti_path,
                            reply_dt,
                            model_save_path,
                            prediction_save_path,
                            model_name,
                            timestamp) {

  tmp_predict <- tempfile(fileext = ".csv")
  data.table::fwrite(reply_dt, tmp_predict)

  model_save_path <- paste0(model_save_path, model_name)

  output_file <- paste0(
    prediction_save_path,
    model_name, "_",
    timestamp, ".csv"
  )

  args <- c(
    pysenti_path,
    "--mode", "predict",
    "--input", tmp_predict,
    "--model_name", model_name,
    "--model_path", model_save_path,
    "--output", output_file
  )

  res <- system2("python", args = args, stdout = TRUE, stderr = TRUE)

  # Capture the full line starting with "PREDICTION_SAVED_AT:"
  full_line <- grep("^PREDICTION_SAVED_AT:", res, value = TRUE)

  print(full_line)

  data.table::fread(output_file)
}


