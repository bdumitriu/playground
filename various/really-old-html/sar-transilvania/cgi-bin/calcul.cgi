#!/usr/bin/perl

# Author: Bogdan DUMITRIU
# Date: 04.10.2000
# Function: Reads input from a web page and calculates the monthly amount
#	    a client has to pay. The program uses a MySQL database with
#	    all the necessary records.
# Is working: yes
# Yet to be done: all done

use DBI;
use CGI;

# configure these and nothing else
$database = "costuri";			# the name of the used database
$table = "viata";			# the name of the used table
$user = "cgis";				# the MySQL user name to be used
$password = `cat ./pf`;			# the MySQL password to be used

# see how we've been called
$query = new CGI;
unless ($action = $query->param('submit'))
{
    $action = 'none';
}

# if we've been called w/ the submit button
if ($action eq "Afla suma lunara")
{
    # read data from web page
    $testa = 0;
    if ($age = $query->param('varsta'))
    {
	$testa = 1 unless ($age =~ /^[0-9]*$/);
    }
    else
    {
	$testa = 1;
    }
    
    $testl = 0;
    if ($length = $query->param('per'))
    {
	$testl = 1 unless ($length =~ /^[0-9]*$/);
    }
    else
    {
	$testl = 1;
    }
    
    $tests = 0;
    if ($sum = $query->param('suma'))
    {
	$tests = 1 unless ($sum =~ /^[0-9]*$/);
    }
    else
    {
	$tests = 1;
    }
    
    # start sending stuff back
    print("Content-type: text/html\n\n");
    print << "END";
    <HTML>
	
    <BODY BACKGROUND=../back.jpg>
    
    <IMG STYLE="position: absolute; top: 10; left: 10" SRC=../datal.jpg>
    <IMG STYLE="position: absolute; top: 10; left: 27" SRC=../datat.jpg>
    <IMG STYLE="position: absolute; top: 359; left: 27" SRC=../datat.jpg>
    <IMG STYLE="position: absolute; top: 10; left: 821" SRC=../datar.jpg>
    
    <FONT FACE=Arial SIZE=1>
    <DIV STYLE="position: absolute; top: 25; left: 25">
    <BR>
END

    if (($testa == 0) && ($testl == 0) && ($tests == 0))
    {
        # open a MySQL connection
	$dbn = "DBI:mysql:database=$database";
        $dbh = DBI->connect($dbn, $user, $password) || die("Can't connect to MySQL!\n");	
	
        # find what we're looking for
	$sth = $dbh->prepare("SELECT * FROM $table WHERE (varsta = $age) and (durata = $length) and (suma = $sum)");
        $sth->execute() || die("Can't execute statement handle!\n");
	
        $test = 0;
	if ($row = $sth->fetchrow_hashref)
        {
            $rate = $$row{rata_l};
	}
        else
	{
	    $test = 1;
        }
    
        # disconnect from the MySQL server
        $sth->finish();
	$dbh->disconnect();
	
	# send some more stuff back
	if ($test == 0)
	{
	    print << "END";
	    In conditiile date de dvs., adica pentru:<BR>
	    * varsta de $age de ani<BR>
	    * durata asiguarii de $length de ani<BR>
	    * suma totala pe care se face asigurarea de $sum de ani<BR>
	    suma lunara pe care va trebui sa o platiti este de $rate lei.<BR>
	    </DIV>
	    </FONT>
	    
	    </BODY>
	    
	    </HTML>
END
	}
	# send some more stuff back
	else
	{
	    print << "END";
	    Din pacate configuratia introdusa de dvs.<BR>
	    * varsta: $age<BR>
	    * durata: $length<BR>
	    * suma: $sum<BR>
	    nu a fost gasita in baza noastra de date.<BR>
	    Va rugam reincercati, avand grija ca toate campurile sa contina doar
	    cifre si ca:<BR>
	    * varsta sa fie intre 1 si 100 ani<BR>
	    * durata sa fie intre 1 si 100 ani<BR>
    	    * suma sa fie intre 1,000,000 si 100,000,000,000<BR>
	    Va multumim.<BR>
	    </DIV>
	    </FONT>
	    
	    </BODY>
	    
	    </HTML>
END
	}
    }
    # send some more stuff back
    else
    {
	print("Din pacate urmatoarele informatii au fost gresite:<BR>");
	if ($testa == 1)
	{
	    print("* varsta: $age<BR>");
	}
	if ($testl == 1)
	{
	    print("* durata: $length<BR>");
	}
	if ($tests == 1)
	{
	    print("* suma: $sum<BR>");
	}
	print << "END";
	Va rugam reincercati, avand grija ca toate campurile sa contina doar
	cifre si ca<BR>
	* varsta sa fie intre 1 si 100 ani<BR>
	* durata sa fie intre 1 si 100 ani<BR>
	* suma sa fie intre 1,000,000 si 100,000,000,000<BR>
	Va multumim.<BR>
	</DIV>
	</FONT>
	    
	</BODY>
	    
	</HTML>
END
    }
}
# if we've been called from the main menu
else
{
    print("Content-type: text/html\n\n");
    print << "END";
    <HTML>
	
    <BODY BACKGROUND=../back.jpg>
    
    <IMG STYLE="position: absolute; top: 10; left: 10" SRC=../datal.jpg>
    <IMG STYLE="position: absolute; top: 10; left: 27" SRC=../datat.jpg>
    <IMG STYLE="position: absolute; top: 359; left: 27" SRC=../datat.jpg>
    <IMG STYLE="position: absolute; top: 10; left: 821" SRC=../datar.jpg>
    
    <DIV STYLE="position: absolute; top: 25; left: 25">
    <BR>
    
    <FORM METHOD=post ACTION="/cgi-bin/calcul.cgi">
    
    <TABLE BORDER=0>
    <CAPTION ALIGN=top><FONT FACE=Arial SIZE=1>Aflati costul asigurarii pe care o doriti prin intermediul paginii noastre</CAPTION>
    
    <TR>
    <TD ALIGN=right><FONT FACE=Arial SIZE=1>Varsta dvs.:
    <TD ALIGN=left><FONT FACE=Arial SIZE=1><INPUT TYPE=text NAME=varsta SIZE=3 MAXLENGTH=3>
    
    <TR>
    <TD ALIGN=right><FONT FACE=Arial SIZE=1>Perioada asigurarii (in ani):
    <TD ALIGN=left><FONT FACE=Arial SIZE=1><INPUT TYPE=text NAME=per SIZE=3 MAXLENGTH=3>
    
    <TR>
    <TD ALIGN=right><FONT FACE=Arial SIZE=1>Suma asigurata:
    <TD ALIGN=left><FONT FACE=Arial SIZE=1><INPUT TYPE=text NAME=suma SIZE=10 MAXLENGTH=15>
        
    </TABLE>
    
    <SPAN STYLE="position: relative; top: 20; left: 50">
    <INPUT TYPE=submit NAME=submit VALUE="Afla suma lunara">
    <INPUT TYPE=reset>
    </SPAN>
    
    </FORM>
    
    </DIV>
    
    </BODY>
    
    </HTML>
END
}