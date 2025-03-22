library(recommenderlab)
ibcf_model <- readRDS("ibcf_model.rds")

# Check basic class info
print(class(ibcf_model))

# See model structure at top level
str(ibcf_model, max.level = 1)

# Check if there's a similarity matrix
if(!is.null(getModel(ibcf_model)$sim)) {
  sim_dims <- dim(getModel(ibcf_model)$sim)
  print(paste("Similarity matrix dimensions:", sim_dims[1], "x", sim_dims[2]))
}

# Try to access model slots if it's an S4 object
if(isS4(ibcf_model)) {
  print("S4 object slots:")
  print(slotNames(ibcf_model))
  
  if("model" %in% slotNames(ibcf_model)) {
    print("Model slot contents:")
    print(names(ibcf_model@model))
  }
}

# Check for item or user mapping
if(exists("itemLabels", where = getModel(ibcf_model))) {
  print("First 5 item labels:")
  print(head(getModel(ibcf_model)$itemLabels, 5))
}

# Try a different path to similarity data
if(!is.null(ibcf_model@model$sim)) {
  print("Similarity matrix dimensions via @model$sim:")
  print(dim(ibcf_model@model$sim))
}