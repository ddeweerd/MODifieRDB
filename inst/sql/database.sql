/*Input tables*/
CREATE TABLE IF NOT EXISTS input_register(
  input_name VARCHAR(100) NOT NULL,
  n_genes INT(5),
  input_type VARCHAR(100) NOT NULL,
  PRIMARY KEY (input_name)
);

CREATE TABLE IF NOT EXISTS microarray_input_settings(
  input_name VARCHAR(100) NOT NULL,
  expression_matrix VARCHAR(100),
  annotation_table VARCHAR(100),
  group1_indici VARCHAR(5000),
  group2_indici VARCHAR(5000),
  group1_label VARCHAR(100),
  group2_label VARCHAR(100),
  method VARCHAR(100),
  use_adjusted INTEGER (1),
  PRIMARY KEY (input_name),
  FOREIGN KEY (input_name) REFERENCES microarray_input_register(input_name)
);

CREATE TABLE IF NOT EXISTS microarray_input_objects(
  input_name VARCHAR(100) NOT NULL,
  microarray_object BLOB,
  PRIMARY KEY (input_name)
);

CREATE TABLE IF NOT EXISTS rnaseq_input_settings(
  input_name VARCHAR(100) NOT NULL,
  count_matrix VARCHAR(100),
  group1_indici VARCHAR(5000),
  group2_indici VARCHAR(5000),
  group1_label VARCHAR(100),
  group2_label VARCHAR(100),
  use_adjusted INTEGER (1),
  normalize_quantiles INTEGER(1),
  PRIMARY KEY (input_name),
  FOREIGN KEY (input_name) REFERENCES rnaseq_input_register(input_name)
);

CREATE TABLE IF NOT EXISTS rnaseq_input_objects(
  input_name VARCHAR(100) NOT NULL,
  rnaseq_object BLOB,
  PRIMARY KEY (input_name)
);

/*PPI tables*/
CREATE TABLE IF NOT EXISTS ppi_network_register(
  ppi_name VARCHAR(100) NOT NULL,
  ppi_table VARCHAR(25) NOT NULL,
  PRIMARY KEY (ppi_name)
);

CREATE TABLE IF NOT EXISTS ppi_db_register(
  ppi_name VARCHAR(100) NOT NULL,
  ppi_db_location VARCHAR(500) NOT NULL,
  PRIMARY KEY (ppi_name)
  FOREIGN KEY (ppi_name) REFERENCES ppi_network_register(ppi_name)
);


/*Module tables*/

/*General module tables*/
CREATE TABLE IF NOT EXISTS module_register(
  module_name VARCHAR(100) NOT NULL,
  module_length INT(5),
  input_name VARCHAR(100) NOT NULL,
  module_type VARCHAR(100) NOT NULL,
  ppi_name VARCHAR(100),
  PRIMARY KEY (module_name)
  FOREIGN KEY (input_name) REFERENCES input_register(input_name)
  FOREIGN KEY (ppi_name) REFERENCES ppi_network_register(ppi_name)
);

CREATE TABLE IF NOT EXISTS module_genes(
  module_name VARCHAR(100) NOT NULL,
  module_type VARCHAR(100) NOT NULL,
  module_genes VARCHAR(50000000),
  PRIMARY KEY (module_name)
  FOREIGN KEY (module_name) REFERENCES module_register(module_name)
);

/*Mcode module tables*/
CREATE TABLE IF NOT EXISTS mcode_module_list(
  module_name VARCHAR(100) NOT NULL,
  module_score VARCHAR(100) NOT NULL,
  module_genes VARCHAR(50000000)
 );

CREATE TABLE IF NOT EXISTS mcode_settings(
  module_name VARCHAR(100) NOT NULL,
  hierarchy REAL,
  vwp REAL,
  haircut INTEGER(1),
  fluff INTEGER(1),
  fdt REAL,
  loops INTEGER(1),
  deg_cutoff REAL,
  module_cutoff REAL,
  dataset_name VARCHAR (500),
  PRIMARY KEY (module_name),
  FOREIGN KEY (module_name) REFERENCES mcode_module_register(module_name)
);

CREATE TABLE IF NOT EXISTS mcode_objects(
  module_name VARCHAR(100) NOT NULL,
  mcode_object BLOB,
  PRIMARY KEY (module_name)
);

/*Clique sum permutation module tables*/

CREATE TABLE IF NOT EXISTS clique_sum_permutation_settings(
  module_name VARCHAR(100) NOT NULL,
  n_iterations INTEGER (500),
  clique_significance REAL,
  min_clique_size INTEGER(10),
  multiple_cores INTEGER(1),
  n_cores INTEGER(3),
  dataset_name VARCHAR (500),
  PRIMARY KEY (module_name)
  FOREIGN KEY (module_name) REFERENCES module_register(module_name)
);

CREATE TABLE IF NOT EXISTS clique_sum_permutation_objects(
  module_name VARCHAR(100) NOT NULL,
  clique_sum_permutation_object BLOB,
  PRIMARY KEY (module_name)
);
/*DIAMOnD module tables*/

CREATE TABLE IF NOT EXISTS diamond_settings(
  module_name VARCHAR(100) NOT NULL,
  deg_cutoff REAL,
  n_output_genes INTEGER(10),
  seed_weight INTEGER(10),
  include_seed INTEGER(1),
  dataset_name VARCHAR (500),
  PRIMARY KEY (module_name)
  FOREIGN KEY (module_name) REFERENCES module_register(module_name)
);

CREATE TABLE IF NOT EXISTS diamond_objects(
  module_name VARCHAR(100) NOT NULL,
  diamond_object BLOB,
  PRIMARY KEY (module_name)
);


/*Correlation clique module tables*/

CREATE TABLE IF NOT EXISTS correlation_clique_settings(
  module_name VARCHAR(100) NOT NULL,
  frequency_cutoff REAL,
  fraction_of_interactions REAL,
  iteration INTEGER (5),
  clique_significance REAL,
  deg_cutoff REAL,
  multiple_cores INTEGER(1),
  n_cores INTEGER(3),
  dataset_name VARCHAR (500),
  PRIMARY KEY (module_name)
  FOREIGN KEY (module_name) REFERENCES module_register(module_name)
);

CREATE TABLE IF NOT EXISTS correlation_clique_objects(
  module_name VARCHAR(100) NOT NULL,
  correlation_clique_object BLOB,
  PRIMARY KEY (module_name)
);
/*Module discoverer module tables*/

CREATE TABLE IF NOT EXISTS module_discoverer_settings(
  module_name VARCHAR(100) NOT NULL,
  permutations INTEGER (5),
  deg_cutoff REAL,
  repeats INTEGER(3),
  clique_cutoff REAL,
  n_cores INTEGER(3),
  dataset_name VARCHAR (500),
  PRIMARY KEY (module_name)
  FOREIGN KEY (module_name) REFERENCES module_register(module_name)
);


CREATE TABLE IF NOT EXISTS module_discoverer_objects(
  module_name VARCHAR(100) NOT NULL,
  module_discoverer_object BLOB,
  PRIMARY KEY (module_name)
);
/*Moda module tables*/

*/
CREATE TABLE IF NOT EXISTS moda_settings(
  module_name VARCHAR(100) NOT NULL,
  cutmethod VARCHAR(100),
  group_of_interest INTEGER(3),
  specificTheta REAL,
  conservedTheta REAL,
  dataset_name VARCHAR (500),
  PRIMARY KEY (module_name)
  FOREIGN KEY (module_name) REFERENCES module_register(module_name)
);

CREATE TABLE IF NOT EXISTS moda_objects(
  module_name VARCHAR(100) NOT NULL,
  moda_object BLOB,
  PRIMARY KEY (module_name)
);
/*WGCNA module tables*/

CREATE TABLE IF NOT EXISTS wgcna_settings(
  module_name VARCHAR(100) NOT NULL,
  group_of_interest INTEGER(3),
  minModuleSize INTEGER(3),
  deepSplit REAL,
  pamRespectsDendro INTEGER(1),
  mergeCutHeight REAL,
  numericLabels INTEGER (1),
  pval_cutoff REAL,
  corType VARCHAR(50),
  maxBlockSize INTEGER (10),
  TOMType VARCHAR (100),
  saveTOMs INTEGER(1),
  maxPOutliers REAL,
  dataset_name VARCHAR (500),
  PRIMARY KEY (module_name)
  FOREIGN KEY (module_name) REFERENCES module_register(module_name)
);

CREATE TABLE IF NOT EXISTS wgcna_objects(
  module_name VARCHAR(100) NOT NULL,
  wgcna_object BLOB,
  PRIMARY KEY (module_name)
);

/*DiffCoEx module tables*/

*/
CREATE TABLE IF NOT EXISTS diffcoex_settings(
  module_name VARCHAR(100) NOT NULL,
  cor_method VARCHAR(50),
  cluster_method VARCHAR(50),
  cuttree_method VARCHAR(50),
  cut_height REAL,
  deepSplit REAL,
  pamRespectsDendro INTEGER(1),
  minClusterSize INTEGER(4),
  cutHeight REAL,
  pval_cutoff REAL,
  dataset_name VARCHAR (500),
  beta REAL,
  PRIMARY KEY (module_name)
  FOREIGN KEY (module_name) REFERENCES module_register(module_name)
);

CREATE TABLE IF NOT EXISTS diffcoex_objects(
  module_name VARCHAR(100) NOT NULL,
  diffcoex_object BLOB,
  PRIMARY KEY (module_name)
);

/*general tables*/
CREATE TABLE IF NOT EXISTS unique_genes(
  gene_name VARCHAR(100),
  PRIMARY KEY (gene_name)
);
/* enrichment tables*/
CREATE TABLE IF NOT EXISTS enrichment_register(
  enrichment_name VARCHAR(100) NOT NULL,
  module_name VARCHAR(100) NOT NULL,
  enrichment_method VARCHAR(100),
  PRIMARY KEY (enrichment_name)
);

CREATE TABLE IF NOT EXISTS enrichment_objects(
  enrichment_name VARCHAR(100) NOT NULL,
  enrichment_object BLOB
);

/*comhub tables*/

CREATE TABLE IF NOT EXISTS comhub_register(
  comhub_name VARCHAR(100) NOT NULL,
  input_name VARCHAR(100) NOT NULL,
  PRIMARY KEY (comhub_name)
);

CREATE TABLE IF NOT EXISTS comhub_objects(
  comhub_name VARCHAR(100) NOT NULL,
  comhub_object BLOB
);

/*consensus module tables*/
