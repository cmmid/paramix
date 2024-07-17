
# first rule is default target when invoking `make` w/o target
# redirects to another pseudo-rule below to avoid any rule
# definitions in local.makefile
default: localdef

# if you want to override directories e.g.
-include local.makefile

# convenience make definitions
R = $(strip Rscript $^ $(1) $@)

# analysis directories + build rules
DATDIR ?= input
OUTDIR ?= output
FIGDIR ?= figure

localdef: allparam

${DATDIR} ${OUTDIR} ${FIGDIR}:
	mkdir -p $@

# scenarios: iso3c codes + shorthand for pathogens
POPSCN := GBR AFG
IFRSCN := SC2 FLU

# create all combinations of scenarios
$(foreach pop,${POPSCN},$(foreach ifr,${IFRSCN},$(call $(eval ALLSCN += ${pop}_${ifr}))))

# do some setup for the packages needed for this analysis
.install_packages: install.R
	$(call R)
	touch $@

# input population data
${DATDIR}/population.rds: R/population.R | .install_packages ${DATDIR}
	$(call R)

# input life expectancy tables
${DATDIR}/lex.rds: R/lex.R
	$(call R)

# input scenario data - use paramix::blend to create these
${DATDIR}/param_%.rda: R/param.R ${DATDIR}/population.rds | ${DATDIR}
	$(call R,$(subst _, ,$*))

allparam: $(patsubst %,${DATDIR}/param_%.rda,${ALLSCN})

# output scenario results
${OUTDIR}/sim_%.rds: R/simulate.R ${DATDIR}/population.rds ${DATDIR}/param_%.rda | ${OUTDIR}
	$(call R,$(firstword $(subst _, ,$*)))

allsim: $(patsubst %,${OUTDIR}/sim_%.rds,${ALLSCN})

# output distilled results - use paramix::alembic/distill
${OUTDIR}/distill_%.rds: R/distill.R ${DATDIR}/population.rds ${DATDIR}/param_%.json ${OUTDIR}/sim_%.rds
	$(call R,$(firstword $(subst _, ,$*)))

alldistill: $(patsubst %,${OUTDIR}/distill_%.rds,${ALLSCN})

# output computed YLLs
${OUTDIR}/yll_%.rds: R/yll.R ${DATDIR}/lex.rds ${OUTDIR}/distill_%.json
	$(call R,$(firstword $(subst _, ,$*)))

# TODO what figs?
${FIGDIR}/fig1.png: R/fig1.R $(patsubst %,${OUTDIR}/yll_%.rds,${ALLSCN})
	$(call R)

hw:
	echo hello world
