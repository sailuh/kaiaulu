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

#' @param conda_path Path to conda if pysenti is setup in a conda env (optional)
#' @param pysenti_conda_env_name pysenti conda env name if pysenti is setup in a conda env (required only if conda path specified)
#' @param pysenti_path Path to the PySenti Python script.
#' @param train_data_path Path to the model train .csv with at least `text` and `polarity` columns.
#' @param model_save_path Directory where the trained model will be stored.
#' @param model_name Model architecture name; `"bert-base-cased"`, `"xlnet-base-cased", etc.
#' @return Character vector containing stdout/stderr from the Python process.
#' @export
pysenti_train_model <- function(conda_path = "",
                                pysenti_conda_env_name = "",
                                pysenti_path,
                                train_data_path,
                                model_save_path,
                                model_name) {

  if(conda_path != ""){
    args <- c(
      "run",
      "-n",
      pysenti_conda_env_name,
      "python",
      pysenti_path,
      "--mode", "train",
      "--input", train_data_path,
      "--model_name", model_name,
      "--output", model_save_path
    )
    res <- system2(conda_path, args = args, stdout = TRUE, stderr = TRUE)
  }
  else{
    args <- c(
      "python",
      pysenti_path,
      "--mode", "train",
      "--input", train_data_path,
      "--model_name", model_name,
      "--output", model_save_path
    )
    res <- system2(conda_path, args = args, stdout = TRUE, stderr = TRUE)
  }
  # stringi::stri_c(c("conda",args), collapse = " ")


}

#' Predict sentiment using a trained PySenti model
#'
#' @description
#' Applies a trained PySenti model to a reply data.table, writes predictions
#' to a CSV file, and returns them as a data.table.
#'
#' @param conda_path Path to conda if pysenti is setup in a conda env (optional)
#' @param pysenti_conda_env_name pysenti conda env name if pysenti is setup in a conda env (required only if conda path specified)
#' @param pysenti_path Path to the PySenti Python script.
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
      "--mode", "predict",
      "--input", prediction_save_path,
      "--model_name", model_name,
      "--model_path", model_save_path,
      "--output", prediction_save_path
    )
    res <- system2(conda_path, args = args, stdout = TRUE, stderr = TRUE)
  }
  else{
    args <- c(
      "python",
      pysenti_path,
      "--mode", "predict",
      "--input", prediction_save_path,
      "--model_name", model_name,
      "--model_path", model_save_path,
      "--output", prediction_save_path
    )
    res <- system2(conda_path, args = args, stdout = TRUE, stderr = TRUE)
  }
  # stringi::stri_c(c("conda",args), collapse = " ")

  data.table::fread(prediction_save_path)
}


