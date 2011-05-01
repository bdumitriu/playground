#!/bin/bash

# pack all modules into a single definition
echo "Packing modules into PLF.def..."
pack-sdf -i Forms.sdf -o PLF.def

# generate the parse table from the definition
echo "Generating parse table into PLF.tbl..."
sdf2table -i PLF.def -o PLF.tbl -m Forms

# generate the regular tree grammar from the definition
echo "Generating regular tree grammar into PLF.rtg..."
sdf2rtg -i PLF.def -o PLF.rtg -m Forms

# generate the basic pretty print table
echo "Generating pretty print table into PLF.pp..."
ppgen -i PLF.def -o PLF.pp

# generate the parenthesize tool
echo "Compiling PLF-parenthesize..."
sdf2parenthesize -i PLF.def -o PLF-parenthesize.str --lang PLF --omod PLF-parenthesize -m Forms
rtg2sig -i PLF.rtg -o PLF.str
strc -i PLF-parenthesize.str -m io-PLF-parenthesize -Cl stratego-lib --silent

# run several actions on each of the *.plf files
echo "Parsing *.plf files..."
for i in `ls *.plf`
do
	# remove the extension
	base=`echo $i | sed -e 's/\.plf$//g'`

	sglri -i $base.plf -p PLF.tbl | ./PLF-parenthesize | pp-aterm -o $base.ast
	ast2abox -i $base.ast -p PLF-pretty.pp | abox2text -o $base.txt
	sglri -i $base.txt -p PLF.tbl | ./PLF-parenthesize | pp-aterm -o $base-pretty.ast
	ast2abox -i $base.ast -p PLF-pretty.pp | abox2text -o $base-pretty.txt

	diff $base.ast $base-pretty.ast > /dev/null 2> /dev/null
	if [ "$?" -ne 0 ]
	then
		echo "$base.ast and $base-pretty.ast are not the same!"
	else
		echo "$base.ast and $base-pretty.ast are identical."
	fi
done

