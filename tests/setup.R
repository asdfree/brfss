if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)

brfss_cat <-
	get_catalog( "brfss" ,
		output_dir = file.path( getwd() ) )

# sample 75% of the records
which_records <- sample( seq( nrow( brfss_cat ) ) , round( nrow( brfss_cat ) * 0.75 ) )

# always sample year == 2016
brfss_cat <- unique( rbind( brfss_cat[ which_records , ] , subset( brfss_cat , year == 2016 ) ) )

lodown( "brfss" , brfss_cat )
