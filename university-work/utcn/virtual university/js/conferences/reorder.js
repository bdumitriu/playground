function moveUp()
{
	for (i = 0; i < nr; i++)
	{
		if (form[i*3].checked)
		{
			idx = i;
		}
	}
	if (idx > 0)
	{
		form[(idx-1)*3].checked = true;
		previousTextField = form[(idx-1)*3+1];
		currentTextField = form[idx*3+1];
		aux = previousTextField.value;
		previousTextField.value = currentTextField.value;
		currentTextField.value = aux;
		previousHiddenField = form[(idx-1)*3+2];
		currentHiddenField = form[idx*3+2];
		aux = previousHiddenField.value;
		previousHiddenField.value = currentHiddenField.value;
		currentHiddenField.value = aux;
	}
}

function moveDown()
{
	for (i = 0; i < nr; i++)
	{
		if (form[i*3].checked)
		{
			idx = i;
		}
	}
	if (idx < nr-1)
	{
		form[(idx+1)*3].checked = true;
		nextTextField = form[(idx+1)*3+1];
		currentTextField = form[idx*3+1];
		aux = nextTextField.value;
		nextTextField.value = currentTextField.value;
		currentTextField.value = aux;
		nextHiddenField = form[(idx+1)*3+2];
		currentHiddenField = form[idx*3+2];
		aux = nextHiddenField.value;
		nextHiddenField.value = currentHiddenField.value;
		currentHiddenField.value = aux;
	}
}