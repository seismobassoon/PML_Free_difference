F90 = gfortran
FLAGS = -std=f2003 -fimplicit-none -frange-check -O3 -fmax-errors=10 -pedantic -pedantic-errors -Waliasing -Wampersand -Wcharacter-truncation -Wline-truncation -Wsurprising -Wno-tabs -Wunderflow
MEDIUM_MEMORY = -mcmodel=medium

default: clean seismic_ADEPML_2D_viscoelastic_RK4_eighth_order_data_files_pml seismic_ADEPML_2D_viscoelastic_RK4_eighth_order_data_files_free diff_reader
all: default

clean:
	/bin/rm -f *.o xseismic_ADEPML_2D_viscoelastic_RK4_eighth_order_data_files_pml xseismic_ADEPML_2D_viscoelastic_RK4_eighth_order_data_files_free xdiff_reader
	
seismic_ADEPML_2D_viscoelastic_RK4_eighth_order_data_files_pml:
	$(F90) $(FLAGS) -o xseismic_ADEPML_2D_viscoelastic_RK4_eighth_order_data_files_pml seismic_ADEPML_2D_viscoelastic_RK4_eighth_order_data_files_pml.f90
	
seismic_ADEPML_2D_viscoelastic_RK4_eighth_order_data_files_free:
	$(F90) $(FLAGS) -o xseismic_ADEPML_2D_viscoelastic_RK4_eighth_order_data_files_free seismic_ADEPML_2D_viscoelastic_RK4_eighth_order_data_files_free.f90

diff_reader:
	$(F90) $(FLAGS) -o xdiff_reader diff_reader.f90
