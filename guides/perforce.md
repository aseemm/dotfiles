# Perforce Guide

p4 login  
  
p4 status  
p4 fstat <filename>  
p4 opened  
p4 have  
p4 files <filename>  
p4 describe 226688  
p4 changes <filename#version>  
p4 sync  
p4 edit <filename>  
  
p4 submit -d "insert newline, test"  
p4 diff <filename>  
p4 diff2 <filename#version> <filename#version>  
  
p4 workspaces -u amaheshwari  
p4 revert -c 234869 //...  
p4 change -d 234869  
p4 changes -c <workspace>  
p4 client -d <workspace>    

## deleting a branch (from a branch workspace)
p4 delete //depot/badbranch/...
p4 submit //depot/badbranch/...