
mkdir prof
set -e
reset
echo "GRIN Boquist benchmark set"

for prog in nfib tsumupto sieve queens words puzzle tak exp3_8 awards sorting cichelli event clausify ida typecheck boyer2
do
stackl exec -- perf stat -e instructions:u -o prof/${prog}.prof $prog
#stackl exec -- perf stat -d -d -d -o prof/${prog}.prof $prog
done
