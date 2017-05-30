#Table creation 
#Structure developed from Bob Horton 

IUCN_tables <- list(
  Habitat = list(
    title="Additional Food Descriptions",
    column_types=c(
      food_code="integer", # foreign key
      seq_num="integer", 
      start_date="date", 
      end_date="date", 
      additional_food_description="text"),
    sep="^"
  ),
  FNDDSNutVal = list(
    title="FNDDS Nutrient Values",
    column_types=c(
      food_code="integer",
      nutrient_code="integer",	# Nutrient Descriptions table
      start_date="date", 
      end_date="date", 
      nutrient_value="double"
    ),
    sep="^"
  ),
  FNDDSSRLinks = list(
    title="FNDDS-SR Links",	# see p34 of fndds_2011_2012_doc.pdf
    column_types=c(
      food_code="integer",
      start_date="date", 
      end_date="date", 
      seq_num="integer",
      sr_code="integer",
      sr_descripton="text",
      amount="double",
      measure="char[3]",	# lb, oz, g, mg, cup, Tsp, qt, fluid ounce, etc
      portion_code="integer",
      retention_code="integer",
      flag="integer",
      weight="double",
      change_type_to_sr_code="char[1]",	# D=data change; F=food change
      change_type_to_weight="char[1]",
      change_type_to_retn_code="char[1]"
    ),
    sep="^"
  ),
  FoodPortionDesc = list(
    title="Food Portion Descriptions",
    column_types=c(
      portion_code="integer", 	# foreign key
      start_date="date",
      end_date="date",
      portion_description="text",
      change_type="char[1]"
    ),
    sep="^"
  ),
  FoodSubcodeLinks = list(
    title="Food code-subcode links",
    column_types=c(
      food_code="integer",
      subcode="integer",
      start_date="date",
      end_date="date"
    ),
    sep="^"
  ),
  FoodWeights = list(
    title="Food Weights",
    column_types=c(
      food_code="integer",	# foreign key
      subcode="integer",
      seq_num="integer",
      portion_code="integer",	# food portion description id
      start_date="date",
      end_date="date",
      portion_weight="double",	# missing values = -9
      change_type="char[1]"	# D=data change, F=food change
    ),
    sep="^"
  ),
  MainFoodDesc = list(
    title="Main Food Descriptions",
    column_types=c(
      food_code="integer", 
      start_date="date", 
      end_date="date", 
      main_food_description="character", 
      fortification_id="integer"),
    sep="^"
  ),
  ModDesc = list(
    title="Modifications Descriptons",
    column_types=c(
      modification_code="integer",
      start_date="date", 
      end_date="date", 
      modification_description="text",
      food_code="integer"
      
    ),
    sep="^"
  ),
  ModNutVal = list(
    title="Modifications Nutrient Values",
    column_types=c(
      modification_code="integer",
      nutrient_code="integer",
      start_date="date", 
      end_date="date", 
      nutrient_value="double"
    ),
    sep="^"
  ),
  MoistNFatAdjust = list(
    title="Moisture & Fat Adjustments",	# to account for changes during cooking
    column_types=c(
      food_code="integer",
      start_date="date", 
      end_date="date", 
      moisture_change="double",
      fat_change="double",
      type_of_fat="integer"	# SR code or food code				
    ),
    sep="^"
  ),
  NutDesc = list(
    title="Nutrient Descriptions",
    column_types=c(
      nutrient_code="integer",
      nutrient_description="text",
      tagname="text",
      unit="text",
      decimals="integer"	# decimal places
    ),
    sep="^"
  ),
  SubcodeDesc = list(
    title="Subcode Descriptions",
    column_types=c(
      subcode="integer",	# key; 0=use default gram weights
      start_date="date",
      end_date="date",
      subcode_description="text"
    ),
    sep="^"
  )
)

