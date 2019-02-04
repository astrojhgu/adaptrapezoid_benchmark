# Benchmark commands and brief description about benchmark framework

## C
For c, I simply loop the integration for 100 times, and time the running time with linux command ```time```
```
cd c
make clean && make
time ./main
```

## C++
I use (google benchmark)[https://github.com/google/benchmark] lib to perform the bench
```
cd cpp
make clean && make
./main
```

## Rust
I use the Rust crate ```criterion``` to benchmark the program.
```
cd rust
cargo bench
```

## Haskell
I use ```criterion``` to benchmark the program.
```
cd hs
stack exec hs-exe
```
## CPython, pypy, and jython
I use ```timeit``` to time the running time
```
cd py
python main.py
pypy main.py
jython main.py
```
## Julia
I use ```BenchmarkTools``` to perform the benchmark
```
julia generic.jl
julia with_type_annotation.jl
julia no_type_annotation.jl
```
## C#
I use ```BenchmarkDotNet``` to perform the benchmark
```
cd cs
dotnet run -c Release
```
## Scala
I use ```scalameter``` to perform the benchmark
```
cd scala
sbt run
```
Then choose corresponding main class to run. The names of the classes are self-explained.
