tl;dr GRIN is a first order monadic (not a contradiction; lets are turned into binds) intermediate language 
for lazy functional programs (or any program based on non-integral pointers, really). it starts off very high 
level, (fairly close to standard intermediate languages like System F[C]) and gets transformed down to 
barely-above-assembly. this is possible because so much of the higher level constructs are basically 
mneumonics - e.g. closures/thunks are easily expressible in terms of low-level GRIN. this means a shitton of 
imperialist optimisations are available to be applied, and a shitton of unboxing/deforestation etc and 
interprocedural register allocation is possible
