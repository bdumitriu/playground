
cd icfp2004/simulator/
make
cd ../ants
make GaneshTest.ant DoNothing.ant
./profileall.sh GaneshTest GaneshTest
./profileall.sh GaneshTest DoNothing
./profileall.sh GaneshTest DoNothing
./profileall.sh GaneshTest GaneshTest | tee o1; ./profileall.sh GaneshTest DoNothing | tee o2; ./profileall.sh GaneshTest DoNothing | tee o3 

