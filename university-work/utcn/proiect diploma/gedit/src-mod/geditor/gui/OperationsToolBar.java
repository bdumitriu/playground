package geditor.gui;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

/**
 * Created by IntelliJ IDEA.
 * User: Lorant Zeno Csaszar
 * Email: lcsaszar@inf.ethz.ch
 * Date: Mar 5, 2003
 * Time: 8:09:48 PM
 */
public class OperationsToolBar extends JToolBar implements ActionListener
{
	public static final int ACTIVATED = 10;
	public static final int DEACTIVATED = 11;

	protected JButton group;
	protected JButton ungroup;
	protected JButton delete;
	protected JButton bringToFront;
	protected JButton sendToBack;

	protected JButton commit;
	protected JButton update;

	public OperationsToolBar()
	{
		super();
		setName("Operations");
		setFloatable(false);
		//setBorder(new EmptyBorder(0, 50, 0, 0));

		group = new JButton(new ImageIcon("resources/images/group.gif"));
		group.setActionCommand("Group");
		group.setToolTipText("Group");
		group.addActionListener(this);
		group.setMnemonic(KeyEvent.VK_G);
		group.setFocusable(false);
		add(group);

		ungroup = new JButton(new ImageIcon("resources/images/ungroup.gif"));
		ungroup.setActionCommand("Ungroup");
		ungroup.setToolTipText("Ungroup");
		ungroup.setMnemonic(KeyEvent.VK_U);
		ungroup.addActionListener(this);
		ungroup.setFocusable(false);
		add(ungroup);

		addSeparator();

		bringToFront = new JButton(new ImageIcon("resources/images/bringtofront.gif"));
		bringToFront.setActionCommand("Bring To Front");
		bringToFront.setToolTipText("Bring To Front");
		bringToFront.addActionListener(this);
		bringToFront.setFocusable(false);
		add(bringToFront);

		sendToBack = new JButton(new ImageIcon("resources/images/sendtoback.gif"));
		sendToBack.setActionCommand("Send To Back");
		sendToBack.setToolTipText("Send To Back");
		sendToBack.addActionListener(this);
		sendToBack.setFocusable(false);
		add(sendToBack);

		addSeparator();

		delete = new JButton(new ImageIcon("resources/images/delete.gif"));
		delete.setActionCommand("Delete");
		delete.setToolTipText("Delete object");
		delete.addActionListener(this);
		delete.setFocusable(false);
		add(delete);

		addSeparator();

		commit = new JButton(new ImageIcon("resources/images/commit.png"));
		commit.setActionCommand("Commit");
		commit.setToolTipText("Commit changes to repository");
		commit.addActionListener(this);
		commit.setFocusable(false);
		add(commit);

		update = new JButton(new ImageIcon("resources/images/update.png"));
		update.setActionCommand("Update");
		update.setToolTipText("Update local version to the one on the repository");
		update.addActionListener(this);
		update.setFocusable(false);
		add(update);

		addSeparator();

		setStatus(DEACTIVATED);
	}

	public void setStatus(int status)
	{
		boolean s = (status == ACTIVATED);
		group.setEnabled(s);
		ungroup.setEnabled(s);
		bringToFront.setEnabled(s);
		sendToBack.setEnabled(s);
		delete.setEnabled(s);
	}

	public void actionPerformed(ActionEvent e)
	{
		String cmd = e.getActionCommand();
		if (cmd.equals("Group"))
		{
			MainFrame.getInstance().getWorkArea().doGroup();
		}
		else if (cmd.equals("Ungroup"))
		{
			MainFrame.getInstance().getWorkArea().doUnGroup();
		}
		else if (cmd.equals("Bring To Front"))
		{
			MainFrame.getInstance().getWorkArea().doBringToFront();
		}
		else if (cmd.equals("Send To Back"))
		{
			MainFrame.getInstance().getWorkArea().doSendToBack();
		}
		else if (cmd.equals("Delete"))
		{
			MainFrame.getInstance().getWorkArea().doDelete();
		}
		else if (cmd.equals("Commit"))
		{
			MainFrame.getInstance().getWorkArea().getDocument().commit();
		}
		else if (cmd.equals("Update"))
		{
			MainFrame.getInstance().getWorkArea().getDocument().update();
			MainFrame.getInstance().getWorkArea().repaint();
		}
	}
}
