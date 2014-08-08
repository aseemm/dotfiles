# Getting Started Guide

# vnc
vncserver -geometry 1850x1050 -alwaysshared -nolisten local
vncserver -geometry 1440x760 -alwaysshared -nolisten local

# running a simulation
runsim.pl -build -run -seed 1 -test play_test -simv_extra_args "+sim_no_eot_check" -verbosity_level UVM_LOW
bsub -q normal 'runsim.pl -run -seed 1 -test play_test -simv_extra_args "+sim_no_eot_check"' -u amaheshwari@paloaltonetworks.com
runrand.pl -rjo -exec  'runsim.pl -build -run -seed 1 -test play_test -simv_extra_args "+sim_no_eot_check" -verbosity_level UVM_LOW'

# license usage
lmstat -c /pan-asic/tools/synopsys/scl/admin/license/synopsys.lic -f VCSRuntime_Net

# coverage reports
urg -dir */*.vdb

# hif/nif/tmi/egr
bsub -I -q short_queue evo test run -id=368 -local -bothargs \"serdes_models_on=y\" -- -vpd

# emacs untabify
emacs -batch feip_rand_test.svh -l ~/bin/emacs_format_file -f emacs-format-function

# code coverage
cd /pan-asic/projects/fe100/shared/jenkins/ipq_feip_weekend_regr/Hardware/main/asic/FE100/verif/ipq-feip/regr
find ipq_feip_wkend_regr_29341 -name "simv.log.gz" -exec gunzip '{}' \;
./covdump.sh - sh ../../../../common/bin/ccov_merge.sh /pan-asic/projects/fe100/shared/jenkins/ipq_feip_weekend_regr/Hardware/main/asic/FE100/verif/ipq-feip/sim/build.flm_beh_model /pan-asic/projects/fe100/shared/jenkins/ipq_feip_weekend_regr/Hardware/main/asic/FE100/ver$

cd /pan-asic/projects/fe100/shared/jenkins/ipq_feip_weeknight_regr/Hardware/main/asic/FE100/verif/ipq-feip/regr
find ipq_feip_wknight_regr_29293 -name "simv.log.gz" -exec gunzip '{}' \;
./covdump.sh - sh ../../../../common/bin/ccov_merge.sh /pan-asic/projects/fe100/shared/jenkins/ipq_feip_weeknight_regr/Hardware/main/asic/FE100/verif/ipq-feip/sim/build.flm_beh_model /pan-asic/projects/fe100/shared/jenkins/ipq_feip_weeknight_regr/Hardware/main/asic/FE100$

cd /home/amaheshwari/merged_coverage
dve -cov -dir ipq_feip_wkend_regr_21331.vdb/ &
