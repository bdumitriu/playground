#!/usr/bin/perl

print("Set-Cookie: nume=Bogdan;\n");
print("Content-type: text/html\n\n");
print << "END";
<HTML>
<BODY>
Press <A HREF="/cgi-bin/print-environment-variables.cgi">here<\/A> to allow a 
check on the cookie.
</BODY>
</HTML>
END
