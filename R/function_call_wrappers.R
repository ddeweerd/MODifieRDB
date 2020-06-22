#' MCODE function wrapper
#' @inheritParams MODifieR::mcode
#' @param input_name Name of the input object in the database
#' @param ppi_name Name of the PPI network in the database
#' @param to_db Save result in database, default is TRUE
#' @param folder If not saved in database, folder where to save the module as .rds
#' @param module_name Unique name for the module object when stored in database
#' @param con Database connection obtained with \code{\link{connect_to_db}} function
#'
#' @details
#' Retrieves a MODifieR input object from database by input_name and runs \code{\link[MODifieR]{mcode}} method.
#' @export
mcode_db <- function(input_name, ppi_name, to_db = TRUE, folder = NULL,
                             hierarchy = 1, vwp = 0.5, haircut = F, fluff = F,
                             fdt = 0.8, loops = T, deg_cutoff = 0.05,
                             module_cutoff = 3.5, module_name, con){

  validate_inference_db(module_name, con)

  MODifieR_input <- MODifieR_input_from_db(input_name = input_name, con)

  cur_ppi <- ppi_network_from_db(ppi_name = ppi_name, con)

  module <- MODifieR::mcode(MODifieR_input = MODifieR_input,
                            ppi_network = cur_ppi,
                            hierarchy = hierarchy,
                            vwp = vwp,
                            haircut = haircut,
                            fluff = fluff,
                            fdt = fdt,
                            loops = loops,
                            deg_cutoff = deg_cutoff,
                            module_cutoff = module_cutoff,
                            dataset_name = input_name)


  module$settings$ppi_network <- ppi_name
  if (to_db){
    MODifieR_object_to_db(module, object_name = module_name, con = con)
  }else{
    saveRDS(object = module, file = paste0(folder, "/", module_name, ".rds"))
  }
}
#' Clique DB function wrapper
#'
#' @inheritParams MODifieR::build_clique_db
#' @inheritParams mcode_db
#'
#' @details
#' Builds a clique db for all maximal cliques in the PPI network and stores the location in the database,
#' wrapper for \code{\link[MODifieR]{build_clique_db}}
#' @export
build_clique_db_db <- function(ppi_name, db_folder, db_name, con){

  cur_ppi <- ppi_network_from_db(ppi_name = ppi_name, con)

  MODifieR::build_clique_db(ppi_network = cur_ppi, db_folder = db_folder, db_name = db_name)

  db_loc <- paste0(db_folder, "/", db_name, ".sqlite")

  ppi_db_to_db(ppi_db_location = db_loc, ppi_name = ppi_name, con = con)
}

#' Clique SuM function wrapper
#'
#' @inheritParams MODifieR::clique_sum_permutation
#' @inheritParams mcode_db
#'
#' @details
#' Retrieves a MODifieR input object from database by
#' input_name and runs \code{\link[MODifieR]{clique_sum_permutation}} method.
#'
#' @export
clique_sum_db <- function(input_name, ppi_name, n_iterations = 100000, to_db = TRUE, folder = NULL,
                          clique_significance = 0.01, min_clique_size = 2,
                          multiple_cores = T, n_cores = 4, module_name, con){

  validate_inference_db(module_name, con)

  MODifieR_input <- MODifieR_input_from_db(input_name = input_name, con)

  db_loc <- match_ppi_to_db_loc(ppi_name, con)

  module <- MODifieR::clique_sum_permutation(MODifieR_input = MODifieR_input,
                                             db = db_loc,
                                             n_iterations = n_iterations,
                                             clique_significance = clique_significance,
                                             min_clique_size = min_clique_size,
                                             multiple_cores = multiple_cores,
                                             n_cores = n_cores,
                                             dataset_name = input_name)

  if (to_db){
    MODifieR_object_to_db(module, object_name = module_name, con = con)
  }else{
    saveRDS(object = module, file = paste0(folder, "/", module_name, ".rds"))
  }

}
#' wgcna function wrapper
#'
#' @inheritParams MODifieR::wgcna
#' @inheritParams mcode_db
#'
#' @details
#' Retrieves a MODifieR input object from database by
#' input_name and runs \code{\link[MODifieR]{wgcna}} method.
#'
#' @export

wgcna_db <- function(input_name, group_of_interest, minModuleSize = 30, deepSplit = 2, to_db = TRUE, folder = NULL,
                     pamRespectsDendro = FALSE, mergeCutHeight = 0.1, numericLabels = TRUE,
                     pval_cutoff = 0.05, corType = "bicor", maxBlockSize = 10000, TOMType = "signed",
                     saveTOMs = TRUE, maxPOutliers = 0.1, module_name, con){

  validate_inference_db(module_name, con)

  MODifieR_input <- MODifieR_input_from_db(input_name = input_name, con)

  module <- MODifieR::wgcna(MODifieR_input = MODifieR_input,
                            group_of_interest = group_of_interest,
                            minModuleSize = minModuleSize,
                            deepSplit = deepSplit,
                            pamRespectsDendro = pamRespectsDendro,
                            mergeCutHeight = mergeCutHeight,
                            numericLabels = numericLabels,
                            pval_cutoff = pval_cutoff,
                            corType = corType,
                            maxBlockSize = maxBlockSize,
                            TOMType = TOMType,
                            saveTOMs = saveTOMs,
                            maxPOutliers = maxPOutliers,
                            dataset_name = input_name)

  if (to_db){
    MODifieR_object_to_db(module, object_name = module_name, con = con)
  }else{
    saveRDS(object = module, file = paste0(folder, "/", module_name, ".rds"))
  }

}
#' diamond function wrapper
#'
#' @inheritParams MODifieR::diamond
#' @inheritParams mcode_db
#'
#' @details
#' Retrieves a MODifieR input object from database by
#' input_name and runs \code{\link[MODifieR]{diamond}} method.
#' @export
diamond_db <- function(input_name, ppi_name, deg_cutoff = 0.05, to_db = TRUE, folder = NULL,
                       n_output_genes = 200, seed_weight = 10,
                       include_seed = F, module_name, con){


  validate_inference_db(module_name, con)

  MODifieR_input <- MODifieR_input_from_db(input_name = input_name, con)

  cur_ppi <- ppi_network_from_db(ppi_name = ppi_name, con)

  module <- MODifieR::diamond(MODifieR_input = MODifieR_input,
                             ppi_network = cur_ppi,
                             deg_cutoff = deg_cutoff,
                             n_output_genes = n_output_genes,
                             seed_weight = seed_weight,
                             include_seed = include_seed,
                             dataset_name = module_name)

  module$settings$ppi_network <- ppi_name

  if (to_db){
    MODifieR_object_to_db(module, object_name = module_name, con = con)
  }else{
    saveRDS(object = module, file = paste0(folder, "/", module_name, ".rds"))
  }
}

#' correlation_clique function wrapper
#'
#' @inheritParams MODifieR::correlation_clique
#' @inheritParams mcode_db
#'
#' @details
#' Retrieves a MODifieR input object from database by
#' input_name and runs \code{\link[MODifieR]{correlation_clique}} method.
#' @export
correlation_clique_db <- function(input_name, ppi_name, frequency_cutoff = 0.5, to_db = TRUE, folder = NULL,
                                  fraction_of_interactions = 0.4, iteration = 50,
                                  clique_significance = 0.01, deg_cutoff = 0.05,
                                  multiple_cores = FALSE, n_cores = 3, module_name, con){


  validate_inference_db(module_name, con)

  MODifieR_input <- MODifieR_input_from_db(input_name = input_name, con)

  cur_ppi <- ppi_network_from_db(ppi_name = ppi_name, con)

  module <- MODifieR::correlation_clique(MODifieR_input = MODifieR_input,
                                         ppi_network = cur_ppi,
                                         frequency_cutoff = frequency_cutoff,
                                         fraction_of_interactions = fraction_of_interactions,
                                         iteration = iteration,
                                         clique_significance = clique_significance,
                                         deg_cutoff = deg_cutoff,
                                         multiple_cores = multiple_cores,
                                         n_cores = n_cores,
                                         dataset_name = input_name)

  module$settings$ppi_network <- ppi_name

  if (to_db){
    MODifieR_object_to_db(module, object_name = module_name, con = con)
  }else{
    saveRDS(object = module, file = paste0(folder, "/", module_name, ".rds"))
  }


}
#' moda function wrapper
#'
#' @inheritParams MODifieR::moda
#' @inheritParams mcode_db
#'
#' @details
#' Retrieves a MODifieR input object from database by
#' input_name and runs \code{\link[MODifieR]{moda}} method.
#' @export
moda_db <- function(input_name, cutmethod = "Density", to_db = TRUE, folder = NULL,
                    group_of_interest, specificTheta = 0.1,
                    conservedTheta = 0.1, module_name, con){

  validate_inference_db(module_name, con)

  MODifieR_input <- MODifieR_input_from_db(input_name = input_name, con)

  module <- MODifieR::moda(MODifieR_input = MODifieR_input,
                           cutmethod = cutmethod,
                           group_of_interest = group_of_interest,
                           specificTheta = specificTheta,
                           conservedTheta = conservedTheta,
                           dataset_name = input_name)

  if (to_db){
    MODifieR_object_to_db(module, object_name = module_name, con = con)
  }else{
    saveRDS(object = module, file = paste0(folder, "/", module_name, ".rds"))
  }

}
#' DiffCoEx function wrapper
#'
#' @inheritParams MODifieR::diffcoex
#' @inheritParams mcode_db
#'
#' @details
#' Retrieves a MODifieR input object from database by
#' input_name and runs \code{\link[MODifieR]{diffcoex}} method.
#' @export
diffcoex_db <- function(input_name, beta = NULL, cor_method = "spearman", to_db = TRUE, folder = NULL,
                        cluster_method = "average", cuttree_method = "hybrid",
                        cut_height = 0.996, deepSplit = 0,
                        pamRespectsDendro = FALSE,
                        minClusterSize = 20, cutHeight = 0.2,
                        pval_cutoff = 0.05, module_name, con){

  validate_inference_db(module_name, con)

  MODifieR_input <- MODifieR_input_from_db(input_name = input_name, con)

  module <- MODifieR::diffcoex(MODifieR_input = MODifieR_input,
                               beta = beta,
                               cor_method = cor_method,
                               cluster_method = cluster_method,
                               cuttree_method = cuttree_method,
                               cut_height = cut_height,
                               deepSplit = deepSplit,
                               pamRespectsDendro = pamRespectsDendro,
                               minClusterSize = minClusterSize,
                               cutHeight = cutHeight,
                               pval_cutoff = pval_cutoff,
                               dataset_name = input_name)

  if (to_db){
    MODifieR_object_to_db(module, object_name = module_name, con = con)
  }else{
    saveRDS(object = module, file = paste0(folder, "/", module_name, ".rds"))
  }

}
#' Module Discoverer function wrapper
#'
#' @inheritParams MODifieR::modulediscoverer
#' @inheritParams mcode_db
#'
#' @details
#' Retrieves a MODifieR input object from database by
#' input_name and runs \code{\link[MODifieR]{modulediscoverer}} method.
#' @export
modulediscoverer_db <- function(input_name, ppi_name, permutations = 10000, to_db = TRUE, folder = NULL,
                                deg_cutoff =  0.05, repeats = 15, clique_cutoff = 0.01,
                                n_cores, module_name, con){

  validate_inference_db(module_name, con)

  MODifieR_input <- MODifieR_input_from_db(input_name = input_name, con)

  cur_ppi <- ppi_network_from_db(ppi_name = ppi_name, con)

  module <- modulediscoverer(MODifieR_input = MODifieR_input,
                             ppi_network = cur_ppi,
                             permutations = permutations,
                             deg_cutoff = deg_cutoff,
                             repeats = repeats,
                             clique_cutoff = clique_cutoff,
                             n_cores = n_cores,
                             dataset_name = input_name)

  module$settings$ppi_network <- ppi_name

  if (to_db){
    MODifieR_object_to_db(module, object_name = module_name, con = con)
  }else{
    saveRDS(object = module, file = paste0(folder, "/", module_name, ".rds"))
  }

}
