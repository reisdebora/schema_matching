# Create function to standardized textual data and remove stopwords
# Input: dataset of raw metadata 
# Output: dataset of standardized metadata

normalizaTexto = function(metadados){
  
  ### Transform to lowercase
  metadados$no_schema = tolower(metadados$no_schema)
  metadados$no_tabela = tolower(metadados$no_tabela)
  metadados$no_coluna = tolower(metadados$no_coluna)
  metadados$tp_coluna = tolower(metadados$tp_coluna)
  
  ##############################
  ### Normalize and Standardizes
  
  ## TYPE OF COLUMNS
  sort(table(metadados$tp_coluna), decreasing = TRUE)
  sort(unique(metadados$tp_coluna))
  
  metadados$tp_coluna[metadados$tp_coluna 
                      %in% c("character","character varying","nchar")] = "char"
  metadados$tp_coluna[metadados$tp_coluna 
                      %in% c("datetime","timestamp", "timestamp with time zone", 
                             "timestamp without time zone", "timestamp(0)", 
                             "timestamp(6)", "timestamp(6) with time zone", 
                             "timestamp(9)", "smalldatetime")] = "timestamp"
  metadados$tp_coluna[metadados$tp_coluna 
                      %in% c("time with time zone", "time with97out time zone")] = "time"
  metadados$tp_coluna[metadados$tp_coluna 
                      %in% c("integer","mediumint", "bigint","tinyint", "smallint")] = "int"
  metadados$tp_coluna[metadados$tp_coluna 
                      %in% c("varchar2","long varchar", "nvarchar", "nvarchar2")] = "varchar"
  metadados$tp_coluna[metadados$tp_coluna 
                      %in% c("double precision")] = "double"
  metadados$tp_coluna[metadados$tp_coluna 
                      %in% c("blob","longblob", "mediumblob","tinyblob")] = "clob"
  metadados$tp_coluna[metadados$tp_coluna 
                      %in% c("longtext","mediumtext", "tinytext", "ntext")] = "text"
  metadados$tp_coluna[metadados$tp_coluna %in% c("xmltype")] = "xml"
  metadados$tp_coluna[metadados$tp_coluna 
                      %in% c("aq$_jms_message", "aq$_jms_object_message",
                             "aq$_jms_text_message")] = "jms"
  metadados$tp_coluna[metadados$tp_coluna %in% c("number")] = "numeric"
  metadados$tp_coluna[metadados$tp_coluna %in% c("decfloat")] = "decimal"
  metadados$tp_coluna[metadados$tp_coluna %in% c("binary_double", "varbinary")] = "binary"
  metadados$tp_coluna[metadados$tp_coluna %in% c("long raw")] = "raw"
  metadados$tp_coluna[metadados$tp_coluna 
                      %in% c("edn_event_data", "edn_oaoo_delivery")] = "edn"
  
  
  ## NAME OF SCHEMA
  length(unique(metadados$no_schema))
  sort(table(metadados$no_schema), decreasing = TRUE) 
  sort(unique(metadados$no_schema))
  
  #metadados$no_schema = metadados$no_schema
  metadados$no_schema[metadados$no_schema %in% c("test", "teste_kadu")] = "teste"
  metadados$no_schema[metadados$no_schema %in% c("sispraticas_old")] = "sispraticas"
  metadados$no_schema[metadados$no_schema %in% c("sigsuas2")] = "sigsuas"
  metadados$no_schema[metadados$no_schema %in% c("siafasv2")] = "siafas"
  metadados$no_schema[metadados$no_schema %in% c("rais_caged")] = "rais"
  metadados$no_schema[metadados$no_schema %in% c("plandem12h")] = "plandem"
  metadados$no_schema[metadados$no_schema %in% c("auditor", "auditoria_acesso")] = "auditoria"
  metadados$no_schema[metadados$no_schema %in% c("bvilela", "bart")] = "bartolomeu"
  metadados$no_schema[metadados$no_schema %in% c("dadosold", "dadosoriginais")] = "dados"
  metadados$no_schema[metadados$no_schema %in% c("gescon_old")] = "gescon"
  metadados$no_schema[metadados$no_schema %in% c("sgbfmigracao", "sgbftmp", "sgbfhst")] = "sgbf"
  metadados$no_schema[metadados$no_schema %in% c("sicnasdataprev", "sicnasdp")] = "sicnas"
  metadados$no_schema[metadados$no_schema 
                      %in% c("bpm_iau", "bpm_mds", "bpm_opss", 
                             "bpm_soainfra", "bpm_stb", "bpm_ums",
                             "bpm_wls", "bpmstd")] = "bpm"
  metadados$no_schema[metadados$no_schema 
                      %in% c("cad_v6_20080701", "cad13122014",                                                           
                      "cad15122012", "cad16092017", "cad17052014",                                                      
                      "cad17062017", "cad17122016", "cad18022017",                                                      
                      "cad18032017", "cad18042015", "cad18062016",                                                      
                      "cad19082017", "cad19112016", "cad19122015",                                                      
                      "cad20052017", "cad20082016", "cad20122013",                                                      
                      "cad21012017", "cad21032015", "cad21042017",                                                      
                      "cad21102017", "cad22072017", "cad30122011",                                                      
                      "cadastro", "cargav6", "tab_cad_16092017",
                       "tab_cad_17062017", "tab_cad_18112017",
                       "tab_cad_19082017", "tab_cad_20052017",
                       "tab_cad_20062015", "tab_cad_21042017",
                       "tab_cad_21102017", "tab_cad_22072017")] = "cadunico"
  
  ## NAME OF TABLE
  length(unique(metadados$no_tabela))
  sort(table(metadados$no_tabela), decreasing = TRUE) 
  sort(unique(metadados$no_tabela))
  
  # Remove stopwords
  metadados$no_tabela = gsub('bkp', '', metadados$no_tabela)
  metadados$no_tabela = gsub('^bp_', '', metadados$no_tabela)
  metadados$no_tabela = gsub("^cp_", '', metadados$no_tabela)
  metadados$no_tabela = gsub("backup", '', metadados$no_tabela)
  metadados$no_tabela = gsub("^old_", '', metadados$no_tabela)
  metadados$no_tabela = gsub("_old", '', metadados$no_tabela)
  metadados$no_tabela = gsub("nova", '', metadados$no_tabela)
  metadados$no_tabela = gsub("fora", '', metadados$no_tabela)
  metadados$no_tabela = gsub("_2", '', metadados$no_tabela)
  metadados$no_tabela = gsub("^tb_", '', metadados$no_tabela)
  metadados$no_tabela = gsub("^tab_", '', metadados$no_tabela)
  metadados$no_tabela = gsub("^tbd_", '', metadados$no_tabela)
  metadados$no_tabela = gsub("^tbf_", '', metadados$no_tabela)
  metadados$no_tabela = gsub("^tbl_", '', metadados$no_tabela)
  metadados$no_tabela = gsub("_tab$", '', metadados$no_tabela)
  metadados$no_tabela = gsub("_tb$", '', metadados$no_tabela)
  metadados$no_tabela = gsub("^vw_", '', metadados$no_tabela)
  metadados$no_tabela = gsub("^view_", '', metadados$no_tabela)
  metadados$no_tabela = gsub("^views_", '', metadados$no_tabela)
  metadados$no_tabela = gsub("_vw$", '', metadados$no_tabela)
  metadados$no_tabela = gsub("_view$", '', metadados$no_tabela)
  metadados$no_tabela = gsub("_views$", '', metadados$no_tabela)
  metadados$no_tabela = gsub("^mv_", '', metadados$no_tabela)
  metadados$no_tabela = gsub("^tmp_", '', metadados$no_tabela)
  metadados$no_tabela = gsub("^temp_", '', metadados$no_tabela)
  metadados$no_tabela = gsub("_tmp$", '', metadados$no_tabela)
  metadados$no_tabela = gsub("_temp$", '', metadados$no_tabela)
  metadados$no_tabela = gsub("^rl_", '', metadados$no_tabela)
  metadados$no_tabela = gsub("^ib_", '', metadados$no_tabela)
  metadados$no_tabela = gsub("_carga$", '', metadados$no_tabela)
  metadados$no_tabela = gsub("^carga_", '', metadados$no_tabela)
  metadados$no_tabela = gsub("_rmv$", '', metadados$no_tabela)
  metadados$no_tabela = gsub("_del$", '', metadados$no_tabela)
  metadados$no_tabela = gsub("^del_", '', metadados$no_tabela)
  metadados$no_tabela = gsub("^xxx_", '', metadados$no_tabela)
  metadados$no_tabela = gsub("_v$", '', metadados$no_tabela)
  
  # Remove numbers, the first and the last Underline
  metadados$no_tabela = gsub('[0-9]+', '', metadados$no_tabela)
  metadados$no_tabela = gsub('_$', '', metadados$no_tabela)
  metadados$no_tabela = gsub('^_', '', metadados$no_tabela)
  
  ## NAME OF COLUMNS
  metadados$no_coluna = gsub("^co_", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("^cd_", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("^dt_", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("^hr_", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("^dh_", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("^ds_", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("^ge_", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("^no_", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("^nu_", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("^rf_", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("^sg_", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("^st_", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("^tp_", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("^qt_", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("^vl_", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("^pc_", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("^sk_", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("^id_", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("^nr_", '', metadados$no_coluna) 
  metadados$no_coluna = gsub('bkp', '', metadados$no_coluna) 
  metadados$no_coluna = gsub("^cp_", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("backup", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("^old_", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("_old", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("nova", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("fora", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("_2", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("_1", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("^tmp_", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("^temp_", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("_temp$", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("_carga$", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("^carga_", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("_del$", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("^del_", '', metadados$no_coluna) 
  metadados$no_coluna = gsub("_v$", '', metadados$no_coluna) 
  
  # Remove numbers, the first and the last Underline
  metadados$no_coluna = gsub('[0-9]+', '', metadados$no_coluna) 
  metadados$no_coluna = gsub("_v$", '', metadados$no_coluna) 
  metadados$no_coluna = gsub('_$', '', metadados$no_coluna) 
  metadados$no_coluna = gsub('^_', '', metadados$no_coluna) 
  metadados$no_coluna = gsub('_$', '', metadados$no_coluna) 
  metadados$no_coluna = gsub('_$', '', metadados$no_coluna) 
  metadados$no_coluna = gsub('_$', '', metadados$no_coluna) 
  metadados$no_coluna = gsub('^_', '', metadados$no_coluna) 
  metadados$no_coluna = gsub('^-', '', metadados$no_coluna) 
  metadados$no_coluna = gsub('_id$', '', metadados$no_coluna) 
  
  return(metadados)
}
