# fortran_mpi_memory_monitor

dump_mem_usagesをcallした箇所のメモリ使用量が測れる


準備:
- mpi_memory_monitor.f90をコンパイルしてmpi_memory_monitor.oを生成しておく
- makefileを使っているならmpi_memory_monitor.oがリンクされるように必要に応じて書き換える
- メモリ使用量をチェックしたい箇所に対応するソースコードの先頭付近でUSE mpi_memory_monitorするのを忘れずに


使い方:  
ソースコードの適当な箇所にcall dump_mem_usages()を挿入する。  
結果はmem_usageフォルダ内に出力される。ファイル名は連番で、dump_mem_usagesが呼ばれるごとに1つファイルが生成される。


標準出力などにログを残したい場合:  
dump_mem_usagesの引数は無くて良いが、第1引数に文字列・第2引数に装置番号を指定するとその装置に文字列を出力することが出来る。  
例えばdump_mem_usages("<Breakpoint 1>", 6)のようにすると、標準出力に<Breakpoint 1>と出力ファイル名が記録されるので対応が分かりやすくなる。


VASPの場合の手順:
- mpi_memory_monitor.f90をmpi_memory_monitor.Fにリネームしてsrcフォルダ内に設置
- src/.objectsの上の方にmpi_memory_monitor.o \を追記(最下行はGPUオプションがオンのときしか読まれないので不適)
- main.F(でなくても良いが)にUSE mpi_mem_monitorとcall dump_mem_usages()を追記


Todo:  
プロセスごとではなくノードごとにまとめてメモリ使用量を出力できるように改造する？
