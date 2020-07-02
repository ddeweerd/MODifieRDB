CREATE TEMP TABLE IF NOT EXISTS enrichment_register(
  input_name VARCHAR(100) NOT NULL,
  enrichment_method VARCHAR(100)
);

CREATE TEMP TABLE IF NOT EXISTS enrichment_objects(
  enrichment_object BLOB
);
