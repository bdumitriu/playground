<?php

require "../Include/Config.php";
require "../Include/Functions.php";
require "../Include/ReportConfig.php";
require "../Include/ReportFunctions.php";

$iPersonID = FilterInput($_GET['PersonID'],'int');
$iYear = FilterInput($_GET['year'],'int');

if (!$_SESSION['bFinance'])
{
	Redirect("Menu.php");
	exit;
}

if (!isset($iYear)) {
	echo "<h2>ERROR: year is not set</h2>";
	exit;
}

if (!isset($iPersonID)) {
	echo "<h2>ERROR: person is not set</h2>";
	exit;
}

// Avoid a bug in FPDF..
setlocale(LC_NUMERIC,'C');

// Load the FPDF library
LoadLib_FPDF();

class PDF extends FPDF
{
	//Page header
	function Header()
	{
		global $sExemptionLetter_Letterhead;

		if (is_readable($sExemptionLetter_Letterhead))
			$this->Image($sExemptionLetter_Letterhead,10,5,190);
		$this->Ln(30);
	}

	//Page footer
	function Footer()
	{
		global $sExemptionLetter_FooterLine;

		// Position at 1.5 cm from bottom
		$this->SetY(-15);
		$this->SetFont('Arial','',9);
		$this->SetLineWidth(0.5);
		$this->Cell(0,10,$sExemptionLetter_FooterLine,'T',0,'C');
	}

	function WriteHTML($html)
	{
		//HTML parser
		$html=str_replace("\n",' ',$html);
		$a=preg_split('/<(.*)>/U',$html,-1,PREG_SPLIT_DELIM_CAPTURE);
		foreach($a as $i=>$e)
		{
			if($i%2==0)
			{
				//Text
				if($this->HREF)
					$this->PutLink($this->HREF,$e);
				else
					$this->Write(5,$e);
			}
			else
			{
				//Tag
				if($e{0}=='/')
					$this->CloseTag(strtoupper(substr($e,1)));
				else
				{
					//Extract attributes
					$a2=explode(' ',$e);
					$tag=strtoupper(array_shift($a2));
					$attr=array();
					foreach($a2 as $v)
						if(ereg('^([^=]*)=["\']?([^"\']*)["\']?$',$v,$a3))
								$attr[strtoupper($a3[1])]=$a3[2];
					$this->OpenTag($tag,$attr);
				}
			}
		}
	}

	function OpenTag($tag,$attr)
	{
		//Opening tag
		if($tag=='B' or $tag=='I' or $tag=='U')
			$this->SetStyle($tag,true);
		if($tag=='A')
			$this->HREF=$attr['HREF'];
		if($tag=='BR')
			$this->Ln(5);
	}

	function CloseTag($tag)
	{
		//Closing tag
		if($tag=='B' or $tag=='I' or $tag=='U')
			$this->SetStyle($tag,false);
		if($tag=='A')
			$this->HREF='';
	}

	function SetStyle($tag,$enable)
	{
		//Modify style and select corresponding font
		$this->$tag+=($enable ? 1 : -1);
		$style='';
		foreach(array('B','I','U') as $s)
			if($this->$s>0)
					$style.=$s;
		$this->SetFont('',$style);
	}

	function PutLink($URL,$txt)
	{
		//Put a hyperlink
		$this->SetTextColor(0,0,255);
		$this->SetStyle('U',true);
		$this->Write(5,$txt,$URL);
		$this->SetStyle('U',false);
		$this->SetTextColor(0);
	}

	// Function to draw a nicely formatted, automatically sized table
	// By: Chris Gebhardt   Note: this is a preliminary attempt! (:
	function AutoTable($header, $data, $rowHeight, $widthScalar, $footerInfo1, $footerInfo2)
	{
		$this->SetDrawColor(128);
		$this->SetLineWidth(.3);

		$numColumns = count($header);

		// Generate column widths
		for($i = 0; $i < $numColumns; $i++)
		{
			$comp = 1;
			$largest = strlen($header[$i]);

			if (count($data) > 0)
			{
				foreach($data as $row)
				{
					$dataLength = strlen($row[$i]);
					if ($dataLength > $largest)
						$largest = $dataLength;
				}
			}

			// Compensate larger strings by not scaling the column width as much
			// Ideally, we could divide row size based on an absolute table width.
			$comp = 1 - ($largest / 6 * 0.1);
			if ($comp < 0.6) $comp = 0.6;

			$width[] = round($largest * $comp * $widthScalar, 0);
		}

		// Check if sum of computed widths is enough to display any of the two
		// footer infos. If not, extend the width of the last column with enough
		// space to make sure any of the footer infos will fit.
		$f1Length = strlen($footerInfo1);
		$f2Length = strlen($footerInfo2);
		if ($f1Length <= $f2Length)
		{
			$fLength = round($f2Length * 0.5 * $widthScalar, 0);
		}
		else
		{
			$fLength = round($f1Length * 0.5 * $widthScalar, 0);
		}
		
		$tLength = array_sum($width);
		if ($tLength < $fLength)
		{
			$width[$numColumns-1] = $width[$numColumns-1] + ($fLength - $tLength);
		}

		// Set the colors and font for the header row
		$this->SetFillColor(100);
		$this->SetTextColor(255);
		$this->SetFont('','B');

		// Generate the header row
		for($i = 0; $i < $numColumns; $i++)
			$this->Cell($width[$i], $rowHeight, $header[$i], 1, 0, 'C', 1);
		$this->Ln();

		// Set the colors and font for the data rows
		$this->SetFillColor(230);
		$this->SetTextColor(0);
		$this->SetFont('');

		$fill = 0;

		// Generate the data rows
		if (count($data) > 0)
		{
			foreach($data as $row)
			{
				for($i = 0; $i < $numColumns; $i++)
				{
					$this->Cell($width[$i], $rowHeight, $row[$i], 'LR', 0, 'T', $fill);
				}
				$this->Ln();
				$fill=!$fill;
			}
		}

		// Draw the bottom of the table
		$this->Cell(array_sum($width), 0, '', 'T');
		$this->Ln();

		// Set the colors and font for the foorter info
		$this->SetFillColor(100);
		$this->SetTextColor(255);

		// Generate the footer info
		$this->Cell(array_sum($width),$rowHeight,$footerInfo1, 1, '', 'R', 1);
		$this->Ln();
		$this->Cell(array_sum($width),$rowHeight,$footerInfo2, 1, '', 'R', 1);

		// Reset the text color & the font to some decent values
		$this->SetTextColor(0);
		$this->SetFont('');
	}

	function CreateReport($iPersonID)
	{
		// Global report strings and stuff..
		global $sCommonNotes;
		global $iYear, $today, $sStartDate, $sEndDate;
		global $iChurchID;
		global $sExemptionLetter_Signature, $sExemptionLetter_Intro, $sExemptionLetter_Closing, $sExemptionLetter_Author;

		// Get the donation total of this person
		$sSQL = "SELECT sum(dna_Amount) as Total
				FROM donations_don
				LEFT JOIN donationamounts_dna ON donations_don.don_ID = donationamounts_dna.dna_don_ID
				WHERE don_DonorID = $iPersonID AND don_Date >= '$sStartDate' AND don_Date <= '$sEndDate' AND donations_don.chu_Church_ID = '$iChurchID'";
		
		$result = RunQuery($sSQL);
		$row = mysql_fetch_array($result);
		extract($row);

		// Get the tithe of this person for this year
		$sSQL = "SELECT tth_Amount AS tithe
				FROM tithe_tth
				WHERE per_person_ID=" . $iPersonID . " AND tth_Year=" . $iYear . " AND chu_Church_ID=" . $_SESSION['iChurchID'];

		$result = RunQuery($sSQL);
		if (mysql_num_rows($result) > 0)
		{
			$aRow = mysql_fetch_array($result);
			$sTithe = $aRow['tithe'];
		}
		else
		{
			$sTithe = "0";
		}

		// Get the details of this person
		$sSQL = "SELECT per_Title, per_FirstName, per_MiddleName, per_LastName, per_Suffix,
			per_Address1, per_Address2, per_City, per_State, per_Zip, per_Country,
			fam_Address1, fam_Address2, fam_City, fam_State, fam_Zip, fam_Country
			FROM person_per
			LEFT JOIN family_fam ON person_per.per_fam_ID = family_fam.fam_ID
			WHERE per_ID=" . $iPersonID;

		$rsPerson = RunQuery($sSQL);
		extract(mysql_fetch_array($rsPerson));

		$sCity = SelectWhichInfo($per_City, $fam_City, false);
		$sState = SelectWhichInfo($per_State, $fam_State, false);
		$sZip = SelectWhichInfo($per_Zip, $fam_Zip, false);
		$sCountry = SelectWhichInfo($per_Country, $fam_Country, false);

		SelectWhichAddress($sAddress1, $sAddress2, $per_Address1, $per_Address2, $fam_Address1, $fam_Address2, false);

		// Generate the page

		$this->SetFont('Times','',12);
		$this->AddPage(); // Create a new page
		$out = "<br><br>$today<br><br><br>";

		$out .= gettext("Below are the contributions for year") . " " . $iYear . " " . gettext("of") . "<br><br>";

		$out .= FormatFullName($per_Title, $per_FirstName, $per_MiddleName, $per_LastName, $per_Suffix, 0) . "<br>";
		if (strlen($sAddress1)) $out .= "$sAddress1<br>";
		if (strlen($sAddress2)) $out .= "$sAddress2<br>";
		$out .= $sCity . ", $sState $sZip";
		if ($sCountry != $sDefaultCountry) $out .= "<br>$sCountry";

		$out .= "<br><br>";

		$this->WriteHTML($out);
		$out = "";

		// Select all donations for person
		$sSQL = "SELECT DATE_FORMAT(don_Date, \"%M\") AS Month, sum(dna_Amount) as Amount
				FROM donations_don LEFT JOIN donationamounts_dna ON donations_don.don_ID = donationamounts_dna.dna_don_ID
				WHERE don_DonorID = $iPersonID AND don_Date >= '$sStartDate' AND don_Date <= '$sEndDate' AND donations_don.chu_Church_ID = '$iChurchID'
				GROUP BY month(don_Date)
				ORDER BY month(don_Date) ASC";

		$result2 = RunQuery($sSQL);

		$tableHeader=array(gettext('Month'), gettext('Amount'));

		while ($row = mysql_fetch_array($result2))
		{
			extract($row);
			$data[] = array($Month, formatNumber($Amount, 'money'));
		}

		$footerInfo1 = gettext("Total contribution: ") . formatNumber($Total, 'money');
		$footerInfo2 = gettext("Annual tithe: ") . formatNumber($sTithe, 'money');
		
		$this->AutoTable($tableHeader, $data, 5, 4, $footerInfo1, $footerInfo2);

		$out .= '<br><br>';
		$out .= $sCommonNotes;
		$this->WriteHTML($out);
	}
}

// Main
$iChurchID = $_SESSION['iChurchID'];
$sStartDate = "$iYear-1-1";
$sEndDate = "$iYear-12-31";
$today = date("F j, Y");

$pdf=new PDF('P', 'mm', $paperFormat);
$pdf->Open();
$pdf->AliasNbPages();

// If a person's ID is given, print a report just for them.
if (strlen($iPersonID) > 0)
{
	$pdf->CreateReport($iPersonID);
}
// Otherwise, print report of all donors
else
{
	// Get the IDs of everybody who donated this year.
	$sSQL = "SELECT DISTINCT per_ID FROM person_per
			RIGHT JOIN tithe_tth ON per_person_ID = per_ID
			WHERE tth_Year = '$iYear'";
	$rsTithers = RunQuery($sSQL);

	while($aTemp = mysql_fetch_array($rsTithers))
	{
		$pdf->CreateReport($aTemp['per_ID']);
	}

}

if ($iPDFOutputType == 1)
	$pdf->Output("TitheReport-" . date("Ymd-Gis") . ".pdf", true);
else
	$pdf->Output();
?>
