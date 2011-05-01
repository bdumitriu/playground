<?php

$link = mysql_connect('localhost', 'nunta', 'nunta9iuliedb')
   or die('Din pacate, a survenit o eroare in salvarea confirmarii dumeavoastra.<br /> ' .
          'Va rugam <a href="index.html">reincercati</a> si, daca primiti din nou acest mesaj, trimiteti un email la bdumitriu@bdumitriu.ro pentru a ne anunta.');

mysql_select_db('nunta')
   or die('Din pacate, a survenit o eroare in salvarea confirmarii dumeavoastra.<br /> ' .
          'Va rugam <a href="index.html">reincercati</a> si, daca primiti din nou acest mesaj, trimiteti un email la bdumitriu@bdumitriu.ro pentru a ne anunta.');

$query = "INSERT INTO Confirmare VALUES ('";
$query .= addslashes($_POST["nume"]);
$query .= "', '";
$query .= addslashes($_POST["nr_pers"]);
$query .= "', CURRENT_TIMESTAMP, '";
$query .= addslashes($_POST["obs"]);
$query .= "')";

mysql_query($query)
   or die('Din pacate, a survenit o eroare in salvarea confirmarii dumeavoastra.<br /> ' .
          'Va rugam <a href="index.html">reincercati</a> si, daca primiti din nou acest mesaj, trimiteti un email la bdumitriu@bdumitriu.ro pentru a ne anunta.');

?>

<html>

<head>
<title>Confirmare</title>
</head>

<body>
<center>

<font style="font-family: verdana; font-size: 12; letter-spacing: 1">

<br /><br />
Am primit confirmarea si va asteptam.
<br /><br />
Tot noi:
<br /><br /><br />

</font>

<img src="noi.jpg" border="1">

</center>

</body>

</html>

<?php

mysql_close($link);

?>
