# Very simple script to just list what's in the data file

# Load the prepared data
load("prepared_data.rdata")

# List all objects
cat("Objects in prepared_data.rdata:\n")
print(ls())

# Just list data frames and their properties
cat("\nData frames in the file:\n")
for(obj_name in ls()) {
  tryCatch({
    obj <- get(obj_name)
    if(is.data.frame(obj)) {
      cat("\nData frame:", obj_name, "\n")
      cat("Dimensions:", dim(obj), "\n")
      cat("Column names:", paste(colnames(obj), collapse=", "), "\n")
    }
  }, error = function(e) {
    cat("Error examining", obj_name, ":", conditionMessage(e), "\n")
  })
}