import java.io.IOException;

public class RunMonitor
{
    public static void main(String args[])
    {
        try
        {
            Process installProc = Runtime.getRuntime().exec("C:\\jdk1.5\\bin\\java -cp \"D:\\work\\java\\monitor\\classes\" DirectoryMonitor");
        }
        catch (IOException e)
        {}
    }
}