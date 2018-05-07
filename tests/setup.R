if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)

library(lodown)
this_sample_break <- Sys.getenv( "this_sample_break" )
brfss_cat <- get_catalog( "brfss" , output_dir = file.path( getwd() ) )
record_categories <- ceiling( seq( nrow( brfss_cat ) ) / ceiling( nrow( brfss_cat ) / 5 ) )
brfss_cat <- brfss_cat[ record_categories == this_sample_break , ]
brfss_cat <- lodown( "brfss" , brfss_cat )
if( any( brfss_cat$year == 2016 ) ){











options( survey.lonely.psu = "adjust" )

library(survey)

brfss_df <- 
	readRDS( file.path( getwd() , "2016 main.rds" ) )

variables_to_keep <-
	c( 'one' , 'xpsu' , 'xststr' , 'xllcpwt' , 'genhlth' , 'medcost' , 
	'xstate' , 'xage80' , 'nummen' , 'numadult' , 'hlthpln1' )
	
brfss_df <- brfss_df[ variables_to_keep ] ; gc()
	
brfss_design <-
	svydesign(
		id = ~ xpsu ,
		 ,
		data = brfss_df ,
		weight = ~ xllcpwt ,
		nest = TRUE
	)
brfss_design <- 
	update( 
		brfss_design ,
		
		fair_or_poor_health = ifelse( genhlth %in% 1:5 , as.numeric( genhlth > 3 ) , NA ) ,
		
		couldnt_see_doc_due_to_cost = 
			factor( 
				medcost , 
				levels = c( 1 , 2 , 7 , 9 ) , 
				labels = c( "yes" , "no" , "dk" , "rf" ) 
			) ,
		
		state_name =
		
			factor(
			
				xstate ,
				
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
svymean( ~ xage80 , brfss_design )

svyby( ~ xage80 , ~ state_name , brfss_design , svymean )
svymean( ~ couldnt_see_doc_due_to_cost , brfss_design , na.rm = TRUE )

svyby( ~ couldnt_see_doc_due_to_cost , ~ state_name , brfss_design , svymean , na.rm = TRUE )
svytotal( ~ xage80 , brfss_design )

svyby( ~ xage80 , ~ state_name , brfss_design , svytotal )
svytotal( ~ couldnt_see_doc_due_to_cost , brfss_design , na.rm = TRUE )

svyby( ~ couldnt_see_doc_due_to_cost , ~ state_name , brfss_design , svytotal , na.rm = TRUE )
svyquantile( ~ xage80 , brfss_design , 0.5 )

svyby( 
	~ xage80 , 
	~ state_name , 
	brfss_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE 
)
svyratio( 
	numerator = ~ nummen , 
	denominator = ~ numadult , 
	brfss_design ,
	na.rm = TRUE
)
sub_brfss_design <- subset( brfss_design , hlthpln1 == 2 )
svymean( ~ xage80 , sub_brfss_design )
this_result <- svymean( ~ xage80 , brfss_design )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ xage80 , 
		~ state_name , 
		brfss_design , 
		svymean 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( brfss_design )
svyvar( ~ xage80 , brfss_design )
# SRS without replacement
svymean( ~ xage80 , brfss_design , deff = TRUE )

# SRS with replacement
svymean( ~ xage80 , brfss_design , deff = "replace" )
svyciprop( ~ fair_or_poor_health , brfss_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( xage80 ~ fair_or_poor_health , brfss_design )
svychisq( 
	~ fair_or_poor_health + couldnt_see_doc_due_to_cost , 
	brfss_design 
)
glm_result <- 
	svyglm( 
		xage80 ~ fair_or_poor_health + couldnt_see_doc_due_to_cost , 
		brfss_design 
	)

summary( glm_result )

result <-
	svymean(
		~ couldnt_see_doc_due_to_cost ,
		subset(
			brfss_design ,
			couldnt_see_doc_due_to_cost %in%
				c( 'yes' , 'no' )
		) ,
		na.rm = TRUE
	)

stopifnot( round( confint( result )[ 1 , 1 ] , 3 ) == 0.128 )
stopifnot( round( confint( result )[ 1 , 2 ] , 3 ) == 0.133 )
stopifnot( round( confint( result )[ 2 , 1 ] , 3 ) == 0.867 )
stopifnot( round( confint( result )[ 2 , 2 ] , 3 ) == 0.872 )

}
