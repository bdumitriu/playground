package gw.query;

import gw.storage.*;
import java.util.*;
import java.util.regex.*;

public class PageInfo {
    private Storage storage;
    private String path;

    public PageInfo(Storage storage, String path) {
        this.storage = storage;
        this.path = path;
    }

    public String getTopics() {
        String temp1 = path.replace('/', ' ');
        String temp2 = temp1.replace('.', ' ');
        Pattern pattern = Pattern.compile("([A-Z][a-z0-9]+)");
        Matcher matcher = pattern.matcher(temp2);
        String words = matcher.replaceAll("$1 ");
        return words + path;
    }

    public Date getModifiedDate() {
        Date modified;
        try {
            Map status = storage.getStatus(path);
            StorageStatusMessage msg = (StorageStatusMessage) status.get(path);
            modified = msg.getLastChangedDate();
            if (modified == null)
                modified = Calendar.getInstance().getTime();
            // System.out.println(modified);
        } catch (Exception e) {
            System.out.println("Failed to get last changed date: " + e.toString());
            modified = Calendar.getInstance().getTime();
        }

        return modified;
    }

    // return all links(Wiki names) in the file
    public String[] getLinks() {
        return new String[0];
        /* FIXME: update
        TWikiParser parser = TWikiParser.getInstance();

        int lastIndex = path.lastIndexOf('/');
        String base_dir;
        if (lastIndex < 0)
            base_dir = "/";
        else
            base_dir = path.substring(0, lastIndex + 1);

        try {
            Element elem = (org.jdom.Element) parser.parse(storage.getFile(path));

            List children = elem.getChildren("link", Gwml.gwmlns);
            Iterator itr = children.iterator();
            String result[] = new String[children.size()];
            int i = 0;
            while (itr.hasNext()) {
                Element child = (Element) itr.next();
                result[i++] = base_dir + child.getAttributeValue("href").replace('.', '/');
            }

            return result;
        } catch (Exception e) {
            System.out.println("Fail to parse Wiki file: (" + path + ")" + e);
            return null;
        }
        */
    }
}
