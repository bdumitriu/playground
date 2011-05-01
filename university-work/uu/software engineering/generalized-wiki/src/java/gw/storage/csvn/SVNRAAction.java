package gw.storage.csvn;

import java.io.*;

class SVNRAAction
{
    public int type;
    public boolean isDirectory = false;
    public boolean recurse = false;
    public File file;
    public String path1;
    public String path2;
    public String propname;
    public String propvalue;
    
    public static final int MOVE    = 1;
    public static final int ADD     = 2;
    public static final int DELETE  = 3;
    public static final int MODIFY  = 4;
    public static final int COPY    = 5;
    public static final int SETPROP = 6;
    public static final int ADDDIR  = 7;
}
