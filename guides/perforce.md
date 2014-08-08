# Perforce Guide

   114  13:33   p4 status
   116  13:33   p4 login
   117  13:34   p4 fstat
   118  13:34   p4 opened
   119  13:34   p4 have
   122  13:37   p4 fstat Hardware/main/asic/FE100/verif/tlu_tdi/src/tlu_tdi_sw_api.svh
   123  13:38   p4 files
   124  13:38   p4 files Hardware/main/asic/FE100/verif/tlu_tdi/src/tlu_tdi_sw_api.svh
   131  13:41   p4 changes Hardware/main/asic/FE100/verif/tlu_tdi/src/tlu_tdi_sw_api.svh#1
   137  13:43   p4 describe 226688
   138  13:44   p4 sync
   139  13:44   p4 edit Hardware/main/asic/FE100/verif/tlu_tdi/src/tlu_tdi_sw_api.svh
   140  13:45   p4 diff Hardware/main/asic/FE100/verif/tlu_tdi/src/tlu_tdi_sw_api.svh
   141  13:47   p4 submit -d "insert newline, test"
   175  13:06   p4 diff2 ../../common/base/tb_fe100_base_test.svh#26 ../../common/base/tb_fe100_base_test.svh#30

   111  8:52    p4 workspaces -u amaheshwari
   120  8:55    p4 revert -c 234869 //...
   122  8:56    p4 change -d 234869
   123  8:57    p4 changes -c evo.amaheshwari.fe100.tlu-tdi.5
   124  8:57    p4 client -d evo.amaheshwari.fe100.tlu-tdi.5