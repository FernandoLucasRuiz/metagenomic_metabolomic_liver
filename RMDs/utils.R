# Prepare data for TSE

 prepare_TSE_data <- function(OTU_table, metadata){
    abundance_table <- OTU_table %>%
        dplyr::group_by(random_ids, Sample) %>%
        dplyr::summarize(Count = sum(as.numeric(Count), na.rm = TRUE)) %>%
        ungroup() %>%
        pivot_wider(names_from = Sample, values_from = Count) %>%
        mutate_all(~replace_na(., 0))
    
    abundance_table <- as.data.frame(abundance_table)
    rownames(abundance_table) <- abundance_table$random_ids
    abundance_table <- abundance_table %>% dplyr::select(-random_ids)
    
    # Crear rowdata(tax)
    tax <- OTU_table %>%
        dplyr::select(Phylum:Species, random_ids) %>%
        as.data.frame()
    
    tax <- tax[!duplicated(tax$random_ids), ]
    
    rownames(tax) <- tax$random_ids
    tax <- dplyr::select(tax, -random_ids)
    
    # Filtrar metadata
    samples_metadata <- metadata %>% 
        dplyr::filter(Sample %in% colnames(abundance_table))
    
    rownames(samples_metadata) <- samples_metadata$Sample
    samples_metadata <- dplyr::select(samples_metadata, -Sample)
    
    
    # Match rows and columns
    #abundance_table <- abundance_table[rownames(tax), rownames(samples_metadata)]
    
    # Let's ensure that the data is in correct (numeric matrix) format:
    abundance_table <- as.matrix(abundance_table)
    
    return(list(abundance_table = abundance_table, tax = tax, samples_metadata = samples_metadata))
    
 }
 
 