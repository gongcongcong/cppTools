# usage:
# ./amino_acid_stat.sh GCF_000001405.40_GRCh38.p14_protein.faa
time seqkit.exe seq -g -j 12 -s -m 100 -M 2000 $1|perl -lne 'foreach(split //){$count{$_}++ if /\w/;} END {print "$_:$count{$_}" for sort {$count{$b} <=> $count{$a}} keys %count;}'
