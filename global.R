library(tools)
library(Seurat)
library(dplyr)
library(shinyjs)
library(shiny)
library(DT)
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2)
library(shinybusy)
library(glue)
library(markdown)
library(ggthemes)
library(shinyalert)

load_seurat_obj <- function(path){
  errors <- c()
  #check file extension
  if (!tolower(tools::file_ext(path)) == 'rds'){
    errors <- c(errors, "Invalid .rds file.")
    return(errors)
  }
  # read in file
  tryCatch(
    {
      obj<- readRDS(path)
    },
    error = function(e){
      errors<- c(errors, "Invalid .rds file.")
      return(errors)
    }
  )
  #validate obj is a Seurat object
  if (!inherits(obj, "Seurat")){
    errors <- c(errors, "File is not a seurat object.")
    return(errors)
  }
  return(obj)
}

create_metadata_UMAP <- function(obj, col){
  if (col %in% c("nCount_RNA", "nFeature_RNA", "percent.mt")){
    col_df <- data.frame(obj@reductions$umap@cell.embeddings, data = obj@meta.data[,col])
    umap <- ggplot(data = col_df) +
      geom_point(mapping = aes(UMAP_1, UMAP_2, color = log10(data)), size = 0.01) +
      scale_colour_gradientn(colours = rainbow(7))
  } else if (col %in% colnames(obj@meta.data)) {
    umap <- DimPlot(obj, pt.size = .1, label = F, label.size = 4, group.by = col, reduction = "umap")
  } else {
    umap <- ggplot() +
      theme_void() +
      geom_text(aes(x = 0.5, y = 0.5, label = "col doesn't exist"), size = 20, color = "gray73", fontface = "bold") +
      theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
  }
  return(umap)
}

create_feature_plot <- function(obj, gene) {
  if (gene %in% rownames(obj)) {
    FP <- FeaturePlot(obj, features = gene, pt.size = 0.001, combine = FALSE)
  } else {
    FP <- ggplot() + 
      theme_void() + 
      geom_text(aes(x = 0.5, y = 0.5, label = "Gene doesn't exist"), size = 20, color = "gray73", fontface = "bold") +
      theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
  }
  return(FP)
}