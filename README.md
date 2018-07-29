# linear-model-retail-store
linear model of in-store sales at a clothing retailer

For a full discussion, see: http://www.rosshartshorn.net/stuffrossthinksabout/a_python_programmer_uses_R/

This is the (scaled) daily in-store sales at a small clothing retailer, as a csv.  The lm (linear model) function of R is
used to make a model (or several) for predicting future sales.

To reproduce, at the command line, type the following:
mkdir plots
mkdir dataframes
mkdir models
Rscript 1_clean_data.R
Rscript 2_create_model.R
Rscript 3_make_plots.R
Rscript 4_predict_future_results.R
