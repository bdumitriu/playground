class Main 
{
    public static void main(String[] args)
    {
        Subversion svn = new Subversion("https://svn.cs.uu.nl:12443/repos/gw");
        
        System.out.println("DIR 1:");
        svn.readDirectory("trunk/src");
        
        System.out.println("DIR 2:");
        svn.readDirectory("branches/storage");
    }
}
