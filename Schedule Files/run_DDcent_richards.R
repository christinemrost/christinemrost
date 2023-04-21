# run_DDcent_richards.R

# Description:
#   This R script runs DayCent model simulations for Hamburg, IA.
#   It is set up to run an equilibrium simulation followed by
#   a base cropping simulation, followed by continuous corn with
#   conventional tillage and no-till. 

# Reinitialize the variables in memory before rerunning the script. 
rm(list=ls())

# --------------- Step 1: Run equilibrium simulation --------------- 
# Equilibrium: 4000 years of grazed grassland 
file.copy("outfiles_eq.in", "outfiles.in", overwrite=TRUE)
unlink("richards_eq.bin")
unlink("richards_eq.lis")
system("DD15centEVI.exe -s richards_eq -n richards_eq", wait=TRUE) 
system("DD15list100.exe richards_eq richards_eq outvars.txt", wait=TRUE)

# --------------- Step 2: Run base cropping simulation --------------- 

# Base cropping schedule: 1980-2001
file.copy("outfiles_exp.in", "outfiles.in", overwrite=TRUE)
unlink("richards_base.bin")
unlink("richards_base.lis")
system("DD15centEVI.exe -s richards_base -n richards_base -e richards_eq", wait=TRUE)
system("DD15list100.exe richards_base richards_base outvars.txt", wait=TRUE)

# --------------- Step 3: Run modern cropping management practices --------------- 

file.copy("outfiles_exp.in", "outfiles.in", overwrite=TRUE)

# Corn Soybean Rotation with No Tillage (nt): (2002-2022)
unlink("richards2002_2022.bin")
unlink("richards2002_2022.lis")
system("DD15centEVI.exe -s richards2002_2022 -n richards2002_2022 -e richards_base", wait=TRUE)
system("DD15list100.exe richards2002_2022 richards2002_2022 outvars.txt", wait=TRUE)
file.rename("harvest.csv","harvest_richards2002_2022.csv")
file.rename("summary.out", "summary_richards2002_2022.out")
file.rename("nflux.out", "nflux_richards2002_2022.out")
file.rename("year_summary.out", "year_summary_richards2002_2022.out")

