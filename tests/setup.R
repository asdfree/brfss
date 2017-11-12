if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

this_sample_break <- Sys.getenv( "this_sample_break" )

library(lodown)

brfss_cat <-
	get_catalog( "brfss" ,
		output_dir = file.path( getwd() ) )

record_categories <- ceiling( seq( nrow( brfss_cat ) ) / ceiling( nrow( brfss_cat ) / 2 ) )

brfss_cat <- unique( rbind( brfss_cat[ record_categories == this_sample_break , ] , sample_setup_required ) )

lodown( "brfss" , brfss_cat )
