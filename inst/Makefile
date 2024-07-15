
default: hw

# convenience make definitions
R = $(strip Rscript $^ $(1) $@)

# analysis directories
DATDIR ?= input
OUTDIR ?= results
FIGDIR ?= figure

# scenarios
POPSCN := GBR AFG
IFRSCN := SC2 FLU

# create all combinations of scenarios
$(foreach pop,${POPSCN},$(foreach ifr,${IFRSCN},$(call $(eval ALLSCN += ${pop}_${ifr}))))

# do some setup for the packages needed for this analysis
.install_packages: R/setup.R
	$(call R)

# input population data
${DATDIR}/popdata.rds: R/population.R | .install_packages ${DATDIR}
	$(call R)

# input scenario data - use paramix::blend to create these
${DATDIR}/param_%.json: R/param.R ${DATDIR}/popdata.rds | ${DATDIR}
	$(call R)

allparam: $(patsubst %,${DATDIR}/param_%.json,${ALLSCN})

# output scenario results
${OUTDIR}/sim_%.rds: R/simulate.R ${DATDIR}/param_%.json | ${OUTDIR}
	$(call R)

# output distilled results
${OUTDIR}/distill_%.rds: R/distill.R ${DATDIR}/param_%.json ${OUTDIR}/sim_%.rds
	$(call R)

# output computed YLLs
${OUTDIR}/yll_%.rds: R/yll.R ${OUTDIR}/distill_%.json
	$(call R)

hw:
	echo hello world
