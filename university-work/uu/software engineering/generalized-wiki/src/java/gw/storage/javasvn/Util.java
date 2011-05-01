package gw.storage.javasvn;

public class Util {
	

    /**
     * Removes any leading or trailing slash from <code>str</code> and returns the
     * result.
     */
	static public String removeSlashes(String str)
    {
        String result = str;

        // remove any leading slashes
        while (result.startsWith("/"))
        {
            result = result.substring(1);
        }

        // remove any trailing slashes
        while (result.endsWith("/"))
        {
            result = result.substring(0, result.length() - 1);
        }

        int pos;
        while ((pos = result.indexOf("//")) != -1)
        {
        	result = result.substring(0, pos) + result.substring(pos+1, result.length());
        }
        
        return result;
    }
}
