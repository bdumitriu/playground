function show(id)
{
	id.style.visibility = "";
	return true;
}

function hide(id)
{
	id.style.visibility = "hidden";
	return true;
}

function changeSong()
{
	number = song.selectedIndex;
	bgsoundObject = document.all.bgsound;
	if (number == 0)
	{
		bgsoundObject.src = "nomusic.mid";
	}
	else if (number == 1)
	{
		bgsoundObject.src = "stairway.mid";
	}
	else if (number == 2)
	{
		bgsoundObject.src = "believer.mid";
	}
	else if (number == 3)
	{
		bgsoundObject.src = "truecolors.mid";
	}
	else if (number == 4)
	{
		bgsoundObject.src = "yesterday.mid";
	}
	else if (number == 5)
	{
		bgsoundObject.src = "risingsun.mid";
	}
	else if (number == 6)
	{
		bgsoundObject.src = "cradle.mid";
	}
	else if (number == 7)
	{
		bgsoundObject.src = "bridge.mid";
	}
	else if (number == 8)
	{
		bgsoundObject.src = "mygirl.mid";
	}
	else if (number == 9)
	{
		bgsoundObject.src = "onlyyou.mid";
	}
	else if (number == 10)
	{
		bgsoundObject.src = "tears.mid";
	}
	else if (number == 11)
	{
		bgsoundObject.src = "standbyme.mid";
	}
	else if (number == 12)
	{
		bgsoundObject.src = "wonderful.mid";
	}
	else if (number == 13)
	{
		bgsoundObject.src = "alllove.mid";
	}
	else if (number == 14)
	{
		bgsoundObject.src = "blowing.mid";
	}
	else if (number == 15)
	{
		bgsoundObject.src = "unchained.mid";
	}
	else if (number == 16)
	{
		bgsoundObject.src = "ghostbusters.mid";
	}
	else if (number == 17)
	{
		bgsoundObject.src = "brother.mid";
	}
	else if (number == 18)
	{
		bgsoundObject.src = "survive.mid";
	}
	else if (number == 19)
	{
		bgsoundObject.src = "submarine.mid";
	}
	else if (number == 20)
	{
		bgsoundObject.src = "whatworld.mid";
	}
	else if (number == 21)
	{
		bgsoundObject.src = "rock.mid";
	}
	else if (number == 22)
	{
		bgsoundObject.src = "cwhisper.mid";
	}
	else if (number == 23)
	{
		bgsoundObject.src = "hero.mid";
	}
	else if (number == 24)
	{
		bgsoundObject.src = "everything.mid";
	}
	else if (number == 25)
	{
		bgsoundObject.src = "fable.mid";
	}
}
