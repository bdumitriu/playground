class Subversion
{
    static {
        System.loadLibrary("subversion");
    }

    Subversion(String repoURL) 
    {
        openSession(repoURL);
    }

    private native void openSession(String repoURL);
    
    public native void readDirectory(String path);

    public void acceptDirEntry(String dirEntry)
    {
        System.out.println("entry: " + dirEntry);
    }
}
