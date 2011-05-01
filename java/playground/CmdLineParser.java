public static HashMap parse(String[] args)
{
	HashMap argMap = new HashMap();

	if ((args == null) || (args.length == 0))
	{
		return argMap;
	}

	for (int i = 0; i < args.length; i++)
	{
		String arg = args [i];

		if (arg.startsWith("-"))
		{
			if (i == (args.length - 1))
			{
				argMap.put(arg.substring(1, arg.length()), null);
				continue;
			}

			String value = args [i+1];

			if (value.startsWith("-"))
			{
				argMap.put(arg.substring(1, arg.length()), null);
			}
			else
			{
				argMap.put(arg.substring(1, arg.length()), value);
			}
		}
	}

	return argMap;
}