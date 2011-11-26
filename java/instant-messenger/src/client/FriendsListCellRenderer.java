package client;

import javax.swing.*;
import java.awt.*;

/**
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitiru.ro
 * @version 0.1
 * @date Sep 20, 2005
 */
public class FriendsListCellRenderer extends JLabel implements ListCellRenderer
{
	final static ImageIcon onlineFriend = new ImageIcon("img/online-friend.jpg");
	final static ImageIcon offlineFriend = new ImageIcon("img/offline-friend.jpg");

	public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus)
	{
		if (value.getClass() == Friend.class)
		{
			Friend f = (Friend) value;
			setHorizontalTextPosition(SwingConstants.TRAILING);
			if (isSelected)
			{
				setBackground(list.getSelectionBackground());
				setForeground(list.getSelectionForeground());
			}
			else
			{
				setBackground(list.getBackground());
				setForeground(list.getForeground());
			}
			setEnabled(list.isEnabled());
			if (f.getStatus() == FriendStatus.ONLINE)
			{
				if (f.getStatusMessage().equals(""))
				{
					setText(f.getName());
				}
				else
				{
					setText(f.getName() + " (" + f.getStatusMessage() + ")");
				}
				setIcon(onlineFriend);
				setFont(new Font("SansSerif", Font.PLAIN, 13));
			}
			else if (f.getStatus() == FriendStatus.OFFLINE)
			{
				setText(f.getName());
				setIcon(offlineFriend);
				setFont(new Font("SansSerif", Font.ITALIC, 13));
				setForeground(Color.GRAY);
			}
			setOpaque(true);
		}
		else
		{
			setText(value.toString());
		}
		return this;
	}
}
