# load example data in example_df
source("generate_example_data.R")

for(i in unique(example_df$officer)){

# run markdown file
rmarkdown::render('report.Rmd',  
                  output_file =  paste0(str_replace(i, ", ", "_"), "_portfolio_report_", Sys.Date(), ".pdf"),  
                  output_dir = "reports/")
}
