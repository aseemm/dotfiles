# Misc Guide

### vnc
vncserver -geometry 1850x1050 -alwaysshared -nolisten local
vncserver -geometry 1440x760 -alwaysshared -nolisten local

### license usage
lmstat -c /pan-asic/tools/synopsys/scl/admin/license/synopsys.lic -f VCSRuntime_Net

### coverage reports
urg -dir */*.vdb

### emacs untabify
emacs -batch feip_rand_test.svh -l ~/bin/emacs_format_file -f emacs-format-function
