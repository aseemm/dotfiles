# Google Test (GTest)  Guide

### Install
sudo apt-get install libgtest-dev
sudo apt-get install cmake
cd /usr/src/gtest
sudo cmake CMakeLists.txt 
sudo make
sudo cp *.a /usr/lib # cp (or symlink) libraries to /usr/lib

### Run
cmake CMakeLists.txt 
make
./runTests

### Command-line options
--gtest_repeat=10 --gtest_break_on_failure # multiple iterations
--gtest_output="xml:report.xml" # xml output
--gtest_fileter=SquareRootTest.*-SquareRootTest.Zero* # filter, run all SquareRootTests except Zero

### Misc
Temporarily disabling tests - Add "DISABLED_" prefix to the test name to disable test