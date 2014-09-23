# Profiling (http://lanyrd.com/2013/pycon/scdywg/)

## Deterministic Profiling
python -m cProfile lcm.py

python -m cProfile -o file.prof lcm.py
runsnakerun file.prof (or anaylze using pstats)

### Targeted Profiling (to save on overhead)
https://translate.svn.sourceforge.net/svnroot/translate/src/trunk/virtaal/devsupport/profiling.py
Use profile_func as a decorator to profile the critical functions

## Statistical Profiler (Interrupt, Inquire, Collate)
StatProf
Plop, Dropbox, D3 (JS) call graphs