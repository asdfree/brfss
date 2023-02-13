#
#
#
library(haven)

zip_tf <- tempfile()

zip_url <-
	"https://www.cdc.gov/brfss/annual_data/2021/files/LLCP2021XPT.zip"
	
download.file( zip_url , zip_tf , mode = 'wb' )

brfss_tbl <- read_xpt( zip_tf )

brfss_df <- data.frame( brfss_tbl )

names( brfss_df ) <- tolower( names( brfss_df ) )

brfss_df[ , 'one' ] <- 1

options( survey.lonely.psu = "adjust" )

library(survey)

variables_to_keep <-
	c( 'one' , 'x_psu' , 'x_ststr' , 'x_llcpwt' , 'genhlth' , 'medcost1' , 
	'x_state' , 'x_age80' , 'nummen' , 'numadult' , 'x_hlthpln' )
	
brfss_df <- brfss_df[ variables_to_keep ]
	
brfss_design <-
	svydesign(
		id = ~ x_psu ,
		strata = ~ x_ststr ,
		data = brfss_df ,
		weight = ~ x_llcpwt ,
		nest = TRUE
	)
	
# note: since large linearized survey designs execute slowly,
# consider performing exploratory analysis as a replication design:
# coefficients (such as means and medians) do not change
# standard errors and confidence intervals differ slightly
# brfss_replication_design <-
# 	as.svrepdesign( brfss_design )
brfss_design <- 
	update( 
		brfss_design ,
		
		fair_or_poor_health = ifelse( genhlth %in% 1:5 , as.numeric( genhlth > 3 ) , NA ) ,
		
		no_doc_visit_due_to_cost = 
			factor( 
				medcost1 , 
				levels = c( 1 , 2 , 7 , 9 ) , 
				labels = c( "yes" , "no" , "dk" , "rf" ) 
			) ,
		
		state_name =
		
			factor(
			
				x_state ,
				
				levels = 
					c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 
					21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 
					37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 
					55, 56, 66, 72, 78) ,
					
				labels = 
					c("ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", 
					"COLORADO", "CONNECTICUT", "DELAWARE", "DISTRICT OF COLUMBIA", 
					"FLORIDA", "GEORGIA", "HAWAII", "IDAHO", "ILLINOIS", "INDIANA",
					"IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND",
					"MASSACHUSETTS", "MICHIGAN", "MINNESOTA", "MISSISSIPPI", 
					"MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE",
					"NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", 
					"NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA",
					"RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE",
					"TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON",
					"WEST VIRGINIA", "WISCONSIN", "WYOMING", "GUAM", "PUERTO RICO",
					"U.S. VIRGIN ISLANDS")
					
			)
	)
sum( weights( brfss_design , "sampling" ) != 0 )

svyby( ~ one , ~ state_name , brfss_design , unwtd.count )
svytotal( ~ one , brfss_design )

svyby( ~ one , ~ state_name , brfss_design , svytotal )
svymean( ~ x_age80 , brfss_design )

svyby( ~ x_age80 , ~ state_name , brfss_design , svymean )
svymean( ~ no_doc_visit_due_to_cost , brfss_design , na.rm = TRUE )

svyby( ~ no_doc_visit_due_to_cost , ~ state_name , brfss_design , svymean , na.rm = TRUE )
svytotal( ~ x_age80 , brfss_design )

svyby( ~ x_age80 , ~ state_name , brfss_design , svytotal )
svytotal( ~ no_doc_visit_due_to_cost , brfss_design , na.rm = TRUE )

svyby( ~ no_doc_visit_due_to_cost , ~ state_name , brfss_design , svytotal , na.rm = TRUE )
svyquantile( ~ x_age80 , brfss_design , 0.5 )

svyby( 
	~ x_age80 , 
	~ state_name , 
	brfss_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE 
)
svyratio( 
	numerator = ~ nummen , 
	denominator = ~ numadult , 
	brfss_design ,
	na.rm = TRUE
)
sub_brfss_design <- subset( brfss_design , x_hlthpln == 2 )
svymean( ~ x_age80 , sub_brfss_design )
this_result <- svymean( ~ x_age80 , brfss_design )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ x_age80 , 
		~ state_name , 
		brfss_design , 
		svymean 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( brfss_design )
svyvar( ~ x_age80 , brfss_design )
# SRS without replacement
svymean( ~ x_age80 , brfss_design , deff = TRUE )

# SRS with replacement
svymean( ~ x_age80 , brfss_design , deff = "replace" )
svyciprop( ~ fair_or_poor_health , brfss_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( x_age80 ~ fair_or_poor_health , brfss_design )
svychisq( 
	~ fair_or_poor_health + no_doc_visit_due_to_cost , 
	brfss_design 
)
glm_result <- 
	svyglm( 
		x_age80 ~ fair_or_poor_health + no_doc_visit_due_to_cost , 
		brfss_design 
	)

summary( glm_result )
library(srvyr)
brfss_srvyr_design <- as_survey( brfss_design )
brfss_srvyr_design %>%
	summarize( mean = survey_mean( x_age80 ) )

brfss_srvyr_design %>%
	group_by( state_name ) %>%
	summarize( mean = survey_mean( x_age80 ) )

result <-
	svymean(
		~ no_doc_visit_due_to_cost ,
		subset(
			brfss_design ,
			state_name %in% 'ALASKA' &
			no_doc_visit_due_to_cost %in%
				c( 'yes' , 'no' )
		) ,
		na.rm = TRUE
	)

stopifnot( round( confint( result )[ 1 , 1 ] , 3 ) == 0.094 )
stopifnot( round( confint( result )[ 1 , 2 ] , 3 ) == 0.122 )
stopifnot( round( confint( result )[ 2 , 1 ] , 3 ) == 0.878 )
stopifnot( round( confint( result )[ 2 , 2 ] , 3 ) == 0.906 )

