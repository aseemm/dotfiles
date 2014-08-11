# SystemC Guide

### Install
* sudo apt-get install build-essential  
* Download SystemC source from Accelera website  
* tar -xzvf systemc-2.3.1.tgz  
* cd systemc-2.3.1/  
* sudo mkdir /usr/local/systemc-2.3.1  
* mkdir objdir  
* cd objdir  
* ../configure --prefix=/usr/local/systemc-2.3.1  
* make  
* sudo make install  

### Test
* export SYSTEMC_HOME=/usr/local/systemc-2.3.1  
* create hello.cpp  
```c
#include <systemc.h>

SC_MODULE (hello_world) {
  SC_CTOR (hello_world) {
    // empty
  }

  void say_hello() {
    cout << "Hello World.\n";
  }
};


int sc_main(int argc, char* argv[]) {
  hello_world hello("HELLO");
  hello.say_hello();
  return (0);
}
```
* g++ -I. -I$SYSTEMC_HOME/include -L. -L$SYSTEMC_HOME/lib-linux64 -Wl,-rpath=$SYSTEMC_HOME/lib-linux64 -o hello hello.cpp -lsystemc -lm  
* ./hello  

