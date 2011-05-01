package geditor.gui;

import javax.swing.*;
import javax.swing.text.html.HTMLFrameHyperlinkEvent;
import javax.swing.text.html.HTMLDocument;
import javax.swing.event.HyperlinkListener;
import javax.swing.event.HyperlinkEvent;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.net.URL;
import java.net.MalformedURLException;
import java.io.IOException;

/**
 * Created by IntelliJ IDEA.
 * User: Administrator
 * Date: May 7, 2003
 * Time: 3:14:34 PM
 * To change this template use Options | File Templates.
 */
public class HelpFrame extends JDialog implements ActionListener{
    private JEditorPane html;

    public HelpFrame() {
        super();
        setTitle("Geditor help");
        try {
            String path = null;
            URL url = null;
            try {
                path = "resources/help/index.html";
                url = null;//Application.class.getResource(path);
            } catch (Exception e) {
                System.err.println("Failed to open " + path);
                url = null;
            }
            if (url != null) {
                html = new JEditorPane(url);
                html.setEditable(false);
                html.addHyperlinkListener(createHyperLinkListener());

                JScrollPane scroller = new JScrollPane();
                JViewport vp = scroller.getViewport();
                vp.add(html);
                JPanel panel = new JPanel();
                JButton button = new JButton("Close");
                button.addActionListener(this);
                panel.add(button);
                getContentPane().setLayout(new BorderLayout());
                getContentPane().add(panel,BorderLayout.SOUTH);
                getContentPane().add(scroller, BorderLayout.CENTER);
                setSize(700,550);
                setVisible(true);
            }
        } catch (MalformedURLException e) {
            System.out.println("Malformed URL: " + e);
        } catch (IOException e) {
            System.out.println("IOException: " + e);
        }

    }

       public HyperlinkListener createHyperLinkListener() {
 	return new HyperlinkListener() {
 	    public void hyperlinkUpdate(HyperlinkEvent e) {
 		if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
 		    if (e instanceof HTMLFrameHyperlinkEvent) {
 			((HTMLDocument)html.getDocument()).processHTMLFrameHyperlinkEvent(
 			    (HTMLFrameHyperlinkEvent)e);
 		    } else {
 			try {
 			    html.setPage(e.getURL());
 			} catch (IOException ioe) {
 			    System.out.println("IOE: " + ioe);
 			}
 		    }
 		}
 	    }
 	};
     }

    public void actionPerformed(ActionEvent e) {
        dispose();
    }
}
