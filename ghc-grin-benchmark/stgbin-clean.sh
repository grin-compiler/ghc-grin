set -e -x

#cd ..
#./lambdabin-clean.sh
#cd ghc-grin-benchmark

find . -name '*.corebin' -delete
find . -name '*_stgbin' -delete
find . -name '*.lambda' -delete
find . -name '*.lambdabin' -delete
find . -name 'whole_program.out' -delete
find . -name 'whole_program.grin' -delete
