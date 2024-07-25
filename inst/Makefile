
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

localdef: ${FIGDIR}/incidence.png

${DATDIR} ${OUTDIR} ${FIGDIR}:
	mkdir -p $@

# scenarios: iso3c codes + shorthand for pathogens
POPSCN := GBR AFG
IFRSCN := SC2 FLU

# create all combinations of scenarios
$(foreach pop,${POPSCN},$(foreach ifr,${IFRSCN},$(call $(eval ALLSCN += ${pop}_${ifr}))))

# do some setup for the packages needed for this analysis
.setup: install.R
	Rscript $^
	touch $@

# input population data
${DATDIR}/population.rds: R/population.R | .setup ${DATDIR}
	$(call R)

# input life expectancy tables
${DATDIR}/lex.rds: R/lex.R | .setup ${DATDIR}
	$(call R)

# input scenario data - use paramix::blend to create these
${DATDIR}/param_%.rda: R/param.R ${DATDIR}/population.rds | ${DATDIR}
	$(call R,$(subst _, ,$*))

allparam: $(patsubst %,${DATDIR}/param_%.rda,${ALLSCN})

${DATDIR}/odin.rda: R/odin.R | ${DATDIR}
	$(call R)

# output scenario results
${OUTDIR}/sim_%.rds: R/simulate.R ${DATDIR}/population.rds ${DATDIR}/param_%.rda | ${OUTDIR}
	$(call R,$(firstword $(subst _, ,$*)))

ALLSIM := $(patsubst %,${OUTDIR}/sim_%.rds,${ALLSCN})
allsim: ${ALLSIM}

# output distilled results - use paramix::alembic/distill
${OUTDIR}/distill_%.rds: R/distill.R ${DATDIR}/population.rds ${DATDIR}/param_%.rda ${OUTDIR}/sim_%.rds
	$(call R,$(firstword $(subst _, ,$*)))

alldistill: $(patsubst %,${OUTDIR}/distill_%.rds,${ALLSCN})

# output computed YLLs
${OUTDIR}/yll_%.rds: R/yll.R ${DATDIR}/lex.rds ${OUTDIR}/distill_%.rds
	$(call R,$(firstword $(subst _, ,$*)))

allylls: $(patsubst %,${OUTDIR}/yll_%.rds,${ALLSCN})

${OUTDIR}/consolidated.rds: R/consolidate.R $(patsubst %,${OUTDIR}/yll_%.rds,${ALLSCN})
	$(call R)

UTIL := ${FIGDIR}/fig_utilities.rda

${UTIL}: R/fig_utilities.R
	$(call R)

# TODO what figs?
${FIGDIR}/fig1.png: R/fig1.R ${OUTDIR}/consolidated.rds | ${FIGDIR}
	$(call R)

${FIGDIR}/incidence.png: R/fig_incidence.R ${UTIL} ${ALLSIM} | ${FIGDIR}
	$(call R)
