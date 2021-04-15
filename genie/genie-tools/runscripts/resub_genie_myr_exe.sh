#!/bin/bash
#$ -q long.q
#$ -cwd
#$ -j y
#$ -o ../../../genie_runlog/
pwd
. /etc/profile.d/modules.sh
module add shared sge
/bin/bash qsub_rungenie_2.sh genie_eb_go_gs_ac_bg_sg_rg rokgem_test3 18 2 6 /gpfs/env/pvp06gzu/genie/genie/../genie_output/rg.rokgem_test3
sleep 3
/bin/bash qsub_rungenie_2.sh genie_eb_go_gs_ac_bg_sg_rg rokgem_test4 18 2 6 /gpfs/env/pvp06gzu/genie/genie/../genie_output/rg.rokgem_test4
sleep 3
