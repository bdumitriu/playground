function getBrowser()
{
	this.ver = navigator.appVersion;
	this.agent = navigator.userAgent;
	this.dom = document.getElementById ? 1 : 0;
	this.opera5 = this.agent.indexOf("Opera 5") > -1;
	this.ie5 = (this.ver.indexOf("MSIE 5") > -1 && this.dom && !(this.opera5)) ? 1 : 0;
	this.ie6 = (this.ver.indexOf("MSIE 6") > -1 && this.dom && !(this.opera5)) ? 1 : 0;
	this.ie4 = (document.all && !(this.dom) && !(this.opera5)) ? 1 : 0;
	this.ie = this.ie4 || this.ie5 || this.ie6;
	this.mac = this.agent.indexOf("Mac") > -1;
	this.ns6 = (this.dom && parseInt(this.ver) >= 5) ? 1 : 0;
	this.ns4 = (document.layers && !(this.dom)) ? 1 : 0;
	this.windows = (this.ie6 || this.ie5 || this.ie4 || this.ns4 || this.ns6 || this.opera5);

	return this;
}

/*
 * Changes the window status to text (a string).
 */
function windowStatus(text)
{
	window.status = text;
	return true;
}

/*
 * Changes the image source of image identified by id to imgPath.
 */
function chImg(id, imgPath)
{
	id.src = imgPath;
	return true;
}

/*
 * Enables the element identified by id.
 */
function enable(id)
{
	id.disabled = '';
	return true;
}

/*
 * Disables the element identified by id.
 */
function disable(id)
{
	id.disabled = 'false';
	return true;
}

/*
 * If checkBox is checked enables the id,
 * If checkBox is not checked disables the id
 */
function changeState(checkBoxID, id)
{
	if (checkBoxID.checked)
		id.disabled = '';
	else
		id.disabled = 'false';

	return true;
}

/*
 * Loads url in frame.
 */
function changeFrame(url, frame)
{
	parent.frames[frame].location.href = url;
}

/*
 * Checks a group of checkboxes with name as their
 * name.
 */
function checkAll(name, checkboxes)
{
	for (i = 0; i < checkboxes; i++)
	{
		 name[i].checked = "true";
	}
}

/*
 * Unchecks a group of checkboxes with name as their
 * name.
 */
function uncheckAll(name, checkboxes)
{
	for (i = 0; i < checkboxes; i++)
	{
		name[i].checked = "";
	}
}