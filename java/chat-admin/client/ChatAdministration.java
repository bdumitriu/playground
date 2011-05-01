package chatAdmin.client;

import chatAdmin.*;
import java.rmi.*;
import java.net.*;
import java.util.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.tree.*;
import javax.swing.event.*;

/**
 *
 * @author Bogdan DUMITRIU
 * @author email: bdumitriu@email.ro
 */
public class ChatAdministration extends javax.swing.JFrame
{
	/** Creates new form ChatAdministration */
	public ChatAdministration()
	{
		initComponents();
		jMenu3.setVisible(false);
	}

	/** This method is called from within the constructor to
	 * initialize the form.
	 */
	private void initComponents()//GEN-BEGIN:initComponents
	{
		jMenuBar1 = new javax.swing.JMenuBar();
		jMenu1 = new javax.swing.JMenu();
		itemConnect = new javax.swing.JMenuItem();
		itemDisconnect = new javax.swing.JMenuItem();
		jSeparator1 = new javax.swing.JSeparator();
		itemNew = new javax.swing.JMenu();
		itemNewUser = new javax.swing.JMenuItem();
		itemNewChatRoom = new javax.swing.JMenuItem();
		itemSend = new javax.swing.JMenuItem();
		jSeparator2 = new javax.swing.JSeparator();
		itemExit = new javax.swing.JMenuItem();
		jMenu3 = new javax.swing.JMenu();
		itemLock = new javax.swing.JMenuItem();
		itemUnlock = new javax.swing.JMenuItem();
		jSeparator3 = new javax.swing.JSeparator();
		itemStart = new javax.swing.JMenuItem();
		itemStop = new javax.swing.JMenuItem();
		itemRestart = new javax.swing.JMenuItem();
		jMenu2 = new javax.swing.JMenu();
		itemHelp = new javax.swing.JMenuItem();
		itemAbout = new javax.swing.JMenuItem();
		jPanel10 = new javax.swing.JPanel();
		jPanel3 = new javax.swing.JPanel();
		jPanel4 = new javax.swing.JPanel();
		host = new javax.swing.JTextField();
		jPanel5 = new javax.swing.JPanel();
		port = new javax.swing.JTextField();
		jPanel6 = new javax.swing.JPanel();
		user = new javax.swing.JTextField();
		jPanel7 = new javax.swing.JPanel();
		pass = new javax.swing.JPasswordField();
		jPanel1 = new javax.swing.JPanel();
		jButton1 = new javax.swing.JButton();
		jButton3 = new javax.swing.JButton();
		jPanel11 = new javax.swing.JPanel();
		jTabbedPane1 = new javax.swing.JTabbedPane();
		jPanel2 = new javax.swing.JPanel();
		jSplitPane1 = new javax.swing.JSplitPane();
		jPanel9 = new javax.swing.JPanel();
		jtf1 = new javax.swing.JTextField();
		jtf2 = new javax.swing.JTextField();
		jtf3 = new javax.swing.JTextField();
		jtf4 = new javax.swing.JTextField();
		jPanel12 = new javax.swing.JPanel();
		jButton2 = new javax.swing.JButton();
		jButton7 = new javax.swing.JButton();
		jScrollPane1 = new javax.swing.JScrollPane();
		jList3 = new javax.swing.JList();
		jPanel8 = new javax.swing.JPanel();
		jSplitPane2 = new javax.swing.JSplitPane();
		jPanel13 = new javax.swing.JPanel();
		jtf10 = new javax.swing.JTextField();
		jtf11 = new javax.swing.JTextField();
		jtf12 = new javax.swing.JTextField();
		jtf13 = new javax.swing.JTextField();
		jtf14 = new javax.swing.JTextField();
		jtf15 = new javax.swing.JTextField();
		jtf16 = new javax.swing.JTextField();
		jtf17 = new javax.swing.JTextField();
		jPanel14 = new javax.swing.JPanel();
		jPanel15 = new javax.swing.JPanel();
		jButton6 = new javax.swing.JButton();
		jPanel16 = new javax.swing.JPanel();
		jButton5 = new javax.swing.JButton();
		jScrollPane2 = new javax.swing.JScrollPane();
		
		jMenu1.setText("Action");
		itemConnect.setText("Connect");
		itemConnect.addActionListener(new java.awt.event.ActionListener()
		{
			public void actionPerformed(java.awt.event.ActionEvent evt)
			{
				itemConnectActionPerformed(evt);
			}
		});
		
		jMenu1.add(itemConnect);
		itemDisconnect.setText("Disconnect");
		itemDisconnect.setEnabled(false);
		itemDisconnect.addActionListener(new java.awt.event.ActionListener()
		{
			public void actionPerformed(java.awt.event.ActionEvent evt)
			{
				itemDisconnectActionPerformed(evt);
			}
		});
		
		jMenu1.add(itemDisconnect);
		jMenu1.add(jSeparator1);
		itemNew.setText("New");
		itemNew.setEnabled(false);
		itemNewUser.setText("User");
		itemNewUser.addActionListener(new java.awt.event.ActionListener()
		{
			public void actionPerformed(java.awt.event.ActionEvent evt)
			{
				itemNewUserActionPerformed(evt);
			}
		});
		
		itemNew.add(itemNewUser);
		itemNewChatRoom.setText("ChatRoom");
		itemNewChatRoom.addActionListener(new java.awt.event.ActionListener()
		{
			public void actionPerformed(java.awt.event.ActionEvent evt)
			{
				itemNewChatRoomActionPerformed(evt);
			}
		});
		
		itemNew.add(itemNewChatRoom);
		jMenu1.add(itemNew);
		itemSend.setText("Send changes");
		itemSend.setEnabled(false);
		itemSend.addActionListener(new java.awt.event.ActionListener()
		{
			public void actionPerformed(java.awt.event.ActionEvent evt)
			{
				itemSendActionPerformed(evt);
			}
		});
		
		jMenu1.add(itemSend);
		jMenu1.add(jSeparator2);
		itemExit.setText("Exit");
		itemExit.addActionListener(new java.awt.event.ActionListener()
		{
			public void actionPerformed(java.awt.event.ActionEvent evt)
			{
				itemExitActionPerformed(evt);
			}
		});
		
		jMenu1.add(itemExit);
		jMenuBar1.add(jMenu1);
		jMenu3.setText("Server Actions");
		itemLock.setText("Lock server");
		itemLock.addActionListener(new java.awt.event.ActionListener()
		{
			public void actionPerformed(java.awt.event.ActionEvent evt)
			{
				itemLockActionPerformed(evt);
			}
		});
		
		jMenu3.add(itemLock);
		itemUnlock.setText("Unlock server");
		itemUnlock.addActionListener(new java.awt.event.ActionListener()
		{
			public void actionPerformed(java.awt.event.ActionEvent evt)
			{
				itemUnlockActionPerformed(evt);
			}
		});
		
		jMenu3.add(itemUnlock);
		jMenu3.add(jSeparator3);
		itemStart.setText("Start server");
		itemStart.addActionListener(new java.awt.event.ActionListener()
		{
			public void actionPerformed(java.awt.event.ActionEvent evt)
			{
				itemStartActionPerformed(evt);
			}
		});
		
		jMenu3.add(itemStart);
		itemStop.setText("Stop server");
		itemStop.addActionListener(new java.awt.event.ActionListener()
		{
			public void actionPerformed(java.awt.event.ActionEvent evt)
			{
				itemStopActionPerformed(evt);
			}
		});
		
		jMenu3.add(itemStop);
		itemRestart.setText("Restart server");
		itemRestart.addActionListener(new java.awt.event.ActionListener()
		{
			public void actionPerformed(java.awt.event.ActionEvent evt)
			{
				itemRestartActionPerformed(evt);
			}
		});
		
		jMenu3.add(itemRestart);
		jMenuBar1.add(jMenu3);
		jMenu2.setText("Help");
		itemHelp.setText("Help");
		itemHelp.addActionListener(new java.awt.event.ActionListener()
		{
			public void actionPerformed(java.awt.event.ActionEvent evt)
			{
				itemHelpActionPerformed(evt);
			}
		});
		
		jMenu2.add(itemHelp);
		itemAbout.setText("About");
		itemAbout.addActionListener(new java.awt.event.ActionListener()
		{
			public void actionPerformed(java.awt.event.ActionEvent evt)
			{
				itemAboutActionPerformed(evt);
			}
		});
		
		jMenu2.add(itemAbout);
		jMenuBar1.add(jMenu2);
		
		getContentPane().setLayout(new java.awt.CardLayout());
		
		setTitle("Chat Administration Client");
		setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);
		addWindowListener(new java.awt.event.WindowAdapter()
		{
			public void windowClosing(java.awt.event.WindowEvent evt)
			{
				exitForm(evt);
			}
		});
		
		jPanel10.setLayout(null);
		
		getContentPane().add(jPanel10, "empty");
		
		jPanel3.setLayout(new java.awt.GridLayout(5, 0));
		
		jPanel4.setLayout(new java.awt.BorderLayout());
		
		host.setText("blackwizard.bdumitriu.ro");
		host.setBorder(new javax.swing.border.TitledBorder("Host"));
		jPanel4.add(host, java.awt.BorderLayout.CENTER);
		
		jPanel3.add(jPanel4);
		
		jPanel5.setLayout(new java.awt.BorderLayout());
		
		port.setText("2500");
		port.setBorder(new javax.swing.border.TitledBorder("Port"));
		jPanel5.add(port, java.awt.BorderLayout.CENTER);
		
		jPanel3.add(jPanel5);
		
		jPanel6.setLayout(new java.awt.BorderLayout());
		
		user.setBorder(new javax.swing.border.TitledBorder("Username"));
		jPanel6.add(user, java.awt.BorderLayout.CENTER);
		
		jPanel3.add(jPanel6);
		
		jPanel7.setLayout(new java.awt.BorderLayout());
		
		pass.setBorder(new javax.swing.border.TitledBorder("Password"));
		jPanel7.add(pass, java.awt.BorderLayout.CENTER);
		
		jPanel3.add(jPanel7);
		
		jButton1.setText("All done");
		jButton1.addActionListener(new java.awt.event.ActionListener()
		{
			public void actionPerformed(java.awt.event.ActionEvent evt)
			{
				jButton1ActionPerformed(evt);
			}
		});
		
		jPanel1.add(jButton1);
		
		jButton3.setText("Start Server (only with admin password)");
		jButton3.addActionListener(new java.awt.event.ActionListener()
		{
			public void actionPerformed(java.awt.event.ActionEvent evt)
			{
				jButton3ActionPerformed(evt);
			}
		});
		
		jPanel1.add(jButton3);
		
		jPanel3.add(jPanel1);
		
		getContentPane().add(jPanel3, "con");
		
		jPanel11.setLayout(new java.awt.GridLayout(1, 0));
		
		jPanel2.setLayout(new java.awt.BorderLayout());
		
		jPanel9.setLayout(new java.awt.GridLayout(5, 0));
		
		jtf1.setBorder(new javax.swing.border.TitledBorder("User login name"));
		jPanel9.add(jtf1);
		
		jtf2.setBorder(new javax.swing.border.TitledBorder("User real name"));
		jPanel9.add(jtf2);
		
		jtf3.setBorder(new javax.swing.border.TitledBorder("Maximum nr. of chat rooms allowed"));
		jPanel9.add(jtf3);
		
		jtf4.setBorder(new javax.swing.border.TitledBorder("Contact info"));
		jPanel9.add(jtf4);
		
		jButton2.setText("Change password");
		jButton2.addActionListener(new java.awt.event.ActionListener()
		{
			public void actionPerformed(java.awt.event.ActionEvent evt)
			{
				jButton2ActionPerformed(evt);
			}
		});
		
		jPanel12.add(jButton2);
		
		jButton7.setText("Apply changes");
		jButton7.addActionListener(new java.awt.event.ActionListener()
		{
			public void actionPerformed(java.awt.event.ActionEvent evt)
			{
				jButton7ActionPerformed(evt);
			}
		});
		
		jPanel12.add(jButton7);
		
		jPanel9.add(jPanel12);
		
		jSplitPane1.setRightComponent(jPanel9);
		
		jList3.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
		jList3.addMouseListener(new java.awt.event.MouseAdapter()
		{
			public void mouseReleased(java.awt.event.MouseEvent evt)
			{
				jList3MouseReleased(evt);
			}
		});
		
		jScrollPane1.setViewportView(jList3);
		
		jSplitPane1.setLeftComponent(jScrollPane1);
		
		jPanel2.add(jSplitPane1, java.awt.BorderLayout.CENTER);
		
		jTabbedPane1.addTab("User(s)", jPanel2);
		
		jPanel8.setLayout(new java.awt.BorderLayout());
		
		jPanel13.setLayout(new java.awt.GridLayout(3, 3));
		
		jtf10.setBorder(new javax.swing.border.TitledBorder("Name"));
		jPanel13.add(jtf10);
		
		jtf11.setBorder(new javax.swing.border.TitledBorder("Owner"));
		jtf11.setEnabled(false);
		jPanel13.add(jtf11);
		
		jtf12.setBorder(new javax.swing.border.TitledBorder("Description"));
		jPanel13.add(jtf12);
		
		jtf13.setBorder(new javax.swing.border.TitledBorder("Max. users (0=unlimited)"));
		jPanel13.add(jtf13);
		
		jtf14.setBorder(new javax.swing.border.TitledBorder("Policy message"));
		jPanel13.add(jtf14);
		
		jtf15.setBorder(new javax.swing.border.TitledBorder("Priority (1=highest)"));
		jPanel13.add(jtf15);
		
		jtf16.setBorder(new javax.swing.border.TitledBorder("User log file"));
		jtf16.setEnabled(false);
		jPanel13.add(jtf16);
		
		jtf17.setBorder(new javax.swing.border.TitledBorder("Admin log file"));
		jtf17.setEnabled(false);
		jPanel13.add(jtf17);
		
		jPanel14.setLayout(new java.awt.GridLayout(2, 0));
		
		jButton6.setText("Apply changes");
		jButton6.addActionListener(new java.awt.event.ActionListener()
		{
			public void actionPerformed(java.awt.event.ActionEvent evt)
			{
				jButton6ActionPerformed(evt);
			}
		});
		
		jPanel15.add(jButton6);
		
		jPanel14.add(jPanel15);
		
		jButton5.setText("Change owner");
		jButton5.addActionListener(new java.awt.event.ActionListener()
		{
			public void actionPerformed(java.awt.event.ActionEvent evt)
			{
				jButton5ActionPerformed(evt);
			}
		});
		
		jPanel16.add(jButton5);
		
		jPanel14.add(jPanel16);
		
		jPanel13.add(jPanel14);
		
		jSplitPane2.setRightComponent(jPanel13);
		
		jSplitPane2.setLeftComponent(jScrollPane2);
		
		jPanel8.add(jSplitPane2, java.awt.BorderLayout.CENTER);
		
		jTabbedPane1.addTab("Chat Room(s)", jPanel8);
		
		jPanel11.add(jTabbedPane1);
		
		getContentPane().add(jPanel11, "main");
		
		setJMenuBar(jMenuBar1);
		pack();
	}//GEN-END:initComponents

	private void jButton3ActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jButton3ActionPerformed
	{//GEN-HEADEREND:event_jButton3ActionPerformed
		try
		{
			 managerServer = (ChatAdminManager) 
				Naming.lookup("//" +
				host.getText() + ":" +
				port.getText() + "/" +
				"chatAdminManagerServ");
		}
		catch (Throwable e)
		{
			new ErrorDialog(this, "Error. Server manager was not " +
				"found.").show();
			return;
		}
		try
		{
			managerServer.startServer(pass.getText());
		}
		catch (RemoteException e)
		{
			new ErrorDialog(this, "An error occured. (maybe " +
				"the manager is stopped).").show();
		}
		catch (InvalidPasswordException e)
		{
			new ErrorDialog(this, "Invalid admin password.").show();
		}
	}//GEN-LAST:event_jButton3ActionPerformed

	private void itemRestartActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_itemRestartActionPerformed
	{//GEN-HEADEREND:event_itemRestartActionPerformed
		try
		{
			managerServer.restartServer(password);
		}
		catch (RemoteException e)
		{
			new ErrorDialog(this, "An error occured. (maybe " +
				"the manager is stopped).").show();
		}
		catch (InvalidPasswordException e) {}
	}//GEN-LAST:event_itemRestartActionPerformed

	private void itemStopActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_itemStopActionPerformed
	{//GEN-HEADEREND:event_itemStopActionPerformed
		try
		{
			managerServer.stopServer(password);
		}
		catch (RemoteException e)
		{
			new ErrorDialog(this, "An error occured. (maybe " +
				"the manager is stopped).").show();
		}
		catch (InvalidPasswordException e) 
		{}
	}//GEN-LAST:event_itemStopActionPerformed

	private void itemStartActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_itemStartActionPerformed
	{//GEN-HEADEREND:event_itemStartActionPerformed
		try
		{
			managerServer.startServer(password);
		}
		catch (RemoteException e)
		{
			new ErrorDialog(this, "An error occured. (maybe " +
				"the manager is stopped).").show();
		}
		catch (InvalidPasswordException e) {}
	}//GEN-LAST:event_itemStartActionPerformed

	private void jButton5ActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jButton5ActionPerformed
	{//GEN-HEADEREND:event_jButton5ActionPerformed
		class CODialog extends javax.swing.JDialog
		{
			private java.awt.Frame parent;
			private boolean yes;
			private int idx;

			public CODialog(java.awt.Frame parent)
			{
				super(parent, true);
				this.parent = parent;
				yes = true;
				initComponents();
			}

			private void initComponents()
			{
				jPanel1 = new javax.swing.JPanel();
				jLabel2 = new javax.swing.JLabel();
				jComboBox1 = new javax.swing.JComboBox(users);
				jPanel2 = new javax.swing.JPanel();
				jButton1 = new javax.swing.JButton();
				jButton2 = new javax.swing.JButton();

				getContentPane().setLayout(
					new java.awt.GridLayout(2, 0));

				setTitle("Change owner");
				setResizable(false);
				setBounds(parent.getX()+100, parent.getY()+100, 
					370, 100);
				addWindowListener(
					new java.awt.event.WindowAdapter()
				{
					public void windowClosing(
						java.awt.event.WindowEvent evt)
					{
						closeDialog(evt);
					}
				});

				jLabel2.setText("New owner: ");
				jPanel1.add(jLabel2);

				jPanel1.add(jComboBox1);

				getContentPane().add(jPanel1);

				jButton1.setText("Change it!");
				jButton1.addActionListener(new ActionListener()
				{
					public void actionPerformed(
						ActionEvent evt)
					{
						yes = true;
						idx = jComboBox1.
							getSelectedIndex();
						setVisible(false);
						dispose();
					}
				});
				jPanel2.add(jButton1);

				jButton2.setText("I'd better not...");
				jButton2.addActionListener(new ActionListener()
				{
					public void actionPerformed(
						ActionEvent evt)
					{
						yes = false;
						idx = -1;
						setVisible(false);
						dispose();
					}
				});
				jPanel2.add(jButton2);

				getContentPane().add(jPanel2);
			}

			private void closeDialog(java.awt.event.WindowEvent evt)
			{
				setVisible(false);
				dispose();
			}

			public boolean wanted()
			{
				return yes;
			}

			public int getIndex()
			{
				return idx;
			}

			private javax.swing.JPanel jPanel1;
			private javax.swing.JLabel jLabel2;
			private javax.swing.JComboBox jComboBox1;
			private javax.swing.JPanel jPanel2;
			private javax.swing.JButton jButton1;
			private javax.swing.JButton jButton2;
		}
		DefaultMutableTreeNode node = (DefaultMutableTreeNode)
			chats.getLastSelectedPathComponent();
		if ((node == null) || (node.getLevel() != 2))
		{
			new ErrorDialog(this, 
				"Please select a chat room first.").show();
			return;
		}
		CODialog cod = new CODialog(this);
		cod.show();
		if (cod.wanted())
		{
			User u = (User) users.elementAt(cod.getIndex());
			String newOwner = u.getLoginName();
			ChatRoom room = (ChatRoom) node.getUserObject();
			room.setOwnerLoginName(newOwner);
			DefaultMutableTreeNode root = (DefaultMutableTreeNode)
				chats.getModel().getRoot();
			Enumeration rootChildren = root.children();
			while (rootChildren.hasMoreElements())
			{
				DefaultMutableTreeNode child =
					(DefaultMutableTreeNode)
					rootChildren.nextElement();
				u = (User) child.getUserObject();
				if (u.getLoginName().equals(newOwner))
					child.add(node);
			}
			chats.updateUI();
			changeChatRoom(room);
			itemSend.setEnabled(true);
		}
	}//GEN-LAST:event_jButton5ActionPerformed

	private void itemUnlockActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_itemUnlockActionPerformed
	{//GEN-HEADEREND:event_itemUnlockActionPerformed
		reconnectIfNecesarry();
		try
		{
			server.unlockAccess(password);
		}
		catch (RemoteException e)
		{
			new ErrorDialog(this, "An error occured (maybe " +
				"because the server is down).").show();
		}
		catch (Exception e)
		{}
	}//GEN-LAST:event_itemUnlockActionPerformed

	private void itemLockActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_itemLockActionPerformed
	{//GEN-HEADEREND:event_itemLockActionPerformed
		reconnectIfNecesarry();
		try
		{
			server.lockAccess(password);
		}
		catch (RemoteException e)
		{
			new ErrorDialog(this, "An error occured (maybe " +
				"because the server is down).").show();
		}
		catch (Exception e)
		{}
	}//GEN-LAST:event_itemLockActionPerformed

	private void itemNewChatRoomActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_itemNewChatRoomActionPerformed
	{//GEN-HEADEREND:event_itemNewChatRoomActionPerformed
		class CRDialog extends javax.swing.JDialog
		{
			private java.awt.Frame parent;
			private boolean yes;
			private int idx;
			private String name;

			public CRDialog(java.awt.Frame parent)
			{
				super(parent, true);
				this.parent = parent;
				yes = false;
				initComponents();
			}

			private void initComponents()
			{
				jPanel1 = new javax.swing.JPanel();
				jLabel1 = new javax.swing.JLabel();
				jTextField1 = new javax.swing.JTextField();
				jLabel2 = new javax.swing.JLabel();
				jComboBox1 = new javax.swing.JComboBox(users);
				jPanel2 = new javax.swing.JPanel();
				jButton1 = new javax.swing.JButton();
				jButton2 = new javax.swing.JButton();

				getContentPane().
					setLayout(
					new java.awt.GridLayout(2, 0));
				setTitle("New chat room");
				setResizable(false);
				setBounds(parent.getX()+100, parent.getY()+100, 
					370, 150);
				addWindowListener(
					new java.awt.event.WindowAdapter()
				{
					public void windowClosing(
						java.awt.event.WindowEvent evt)
					{
						closeDialog(evt);
					}
				});

				jLabel1.setText("Name of the new chat room: ");
				jPanel1.add(jLabel1);

				jTextField1.setPreferredSize(
					new java.awt.Dimension(150, 21));
				jPanel1.add(jTextField1);

				jLabel2.setText("Owner of the new chat room: ");
				jPanel1.add(jLabel2);

				jPanel1.add(jComboBox1);

				getContentPane().add(jPanel1);

				jButton1.setText("Create it!");
				jButton1.addActionListener(new ActionListener()
				{
					public void actionPerformed(
						ActionEvent evt)
					{
						yes = true;
						idx = jComboBox1.
							getSelectedIndex();
						name = jTextField1.getText();
						setVisible(false);
						dispose();
					}
				});
				jPanel2.add(jButton1);

				jButton2.setText("I don't want it!");
				jButton2.addActionListener(new ActionListener()
				{
					public void actionPerformed(
						ActionEvent evt)
					{
						yes = false;
						idx = -1;
						name = "";
						setVisible(false);
						dispose();
					}
				});
				jPanel2.add(jButton2);

				getContentPane().add(jPanel2);
			}

			private void closeDialog(java.awt.event.WindowEvent evt) {
				setVisible(false);
				dispose();
			}

			public boolean wanted()
			{
				return yes;
			}

			public int getIndex()
			{
				return idx;
			}

			public String getOwnerName()
			{
				return name;
			}

			private javax.swing.JPanel jPanel1;
			private javax.swing.JLabel jLabel1;
			private javax.swing.JTextField jTextField1;
			private javax.swing.JLabel jLabel2;
			private javax.swing.JComboBox jComboBox1;
			private javax.swing.JPanel jPanel2;
			private javax.swing.JButton jButton1;
			private javax.swing.JButton jButton2; 
		}

		CRDialog dg = new CRDialog(this);
		dg.show();
		if (dg.wanted())
		{
			User owner = (User) users.elementAt(dg.getIndex());
			String loginName = owner.getLoginName();
			ChatRoom room = new ChatRoom(dg.getOwnerName(), 
				loginName);
			DefaultMutableTreeNode root = (DefaultMutableTreeNode)
				chats.getModel().getRoot();
			Enumeration rootChildren = root.children();
			while (rootChildren.hasMoreElements())
			{
				DefaultMutableTreeNode node = 
					(DefaultMutableTreeNode) 
					rootChildren.nextElement();
				User u = (User) node.getUserObject();
				if (u.equals(owner))
					node.add(new DefaultMutableTreeNode(
						room));
			}
			chats.updateUI();
			itemSend.setEnabled(true);
		}
	}//GEN-LAST:event_itemNewChatRoomActionPerformed

	private void jList3MouseReleased(java.awt.event.MouseEvent evt)//GEN-FIRST:event_jList3MouseReleased
	{//GEN-HEADEREND:event_jList3MouseReleased
		if ((evt.isPopupTrigger()) && 
			(jList3.locationToIndex(evt.getPoint()) != -1) &&
			(admin))
		{
			jList3.setSelectedIndex(jList3.locationToIndex(
				evt.getPoint()));
			JPopupMenu delete = new JPopupMenu();
			JMenuItem itemDelete = new JMenuItem("Delete");
			itemDelete.addActionListener(new ActionListener()
			{
				public void actionPerformed(ActionEvent evt)
				{
					int idx = jList3.getSelectedIndex();
					User usr = (User) users.elementAt(idx);
					users.removeElementAt(idx);
					modifiyUsersList();
					DefaultMutableTreeNode root = 
						(DefaultMutableTreeNode)
						chats.getModel().getRoot();
					Enumeration rootChildren = 
						root.children();
					while (rootChildren.hasMoreElements())
					{
						DefaultMutableTreeNode node =
							(DefaultMutableTreeNode)
							rootChildren.
							nextElement();
						User u = (User) node.
							getUserObject();
						if (u.equals(usr))
							try
							{
								root.remove(
									node);
							}
							catch (IllegalArgumentException e)
							{}
					}
					chats.updateUI();
					itemSend.setEnabled(true);
				}
			});
			delete.add(itemDelete);
			delete.show(evt.getComponent(), evt.getX(), evt.getY());
			
		}
	}//GEN-LAST:event_jList3MouseReleased

	private void itemNewUserActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_itemNewUserActionPerformed
	{//GEN-HEADEREND:event_itemNewUserActionPerformed
		String name = "newUser" + new Integer(count).toString();
		User newUser = new User(name, "");
		count++;
		while (users.contains(newUser))
		{
			newUser.setLoginName("newUser" + new Integer(count).
				toString());
			count++;
		}
		users.addElement(newUser);
		modifiyUsersList();
		DefaultMutableTreeNode root = (DefaultMutableTreeNode)
			chats.getModel().getRoot();
		root.add(new DefaultMutableTreeNode(users.lastElement()));
		chats.updateUI();
		itemSend.setEnabled(true);
	}//GEN-LAST:event_itemNewUserActionPerformed

	private void itemDisconnectActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_itemDisconnectActionPerformed
	{//GEN-HEADEREND:event_itemDisconnectActionPerformed
		reconnectIfNecesarry();
		if (itemSend.isEnabled())
		{
			QuestionDialog qd = new QuestionDialog(this, 
				"Changes have not been sent yet. Are you " +
				"sure you want to disconnect without " +
				"sending the changes?");
			qd.show();
			if (!qd.answer())
				return;
		}
		try
		{
			if ((admin) && (server != null) && (server.isLocked()))
			{
				QuestionDialog qd = new QuestionDialog(this, 
					"The server is locked. This means " +
					"that users cannot submit any " +
					"changes before you log on again and " +
					"unlock the server. Are you sure you " +
					"want to leave the server locked?");
				qd.show();
				if (!qd.answer())
					return;
			}
			if ((admin) && (managerServer != null) &&
				(managerServer.isStopped()))
			{
				QuestionDialog qd = new QuestionDialog(this, 
					"The server is stopped. This means " +
					"that no one can use it in any " +
					"way before you log on again and " +
					"restart it. Are you sure you " +
					"want to leave the server stopped?");
				qd.show();
				if (!qd.answer())
					return;
			}
		}
		catch (RemoteException e)
		{
			new ErrorDialog(this, "xxxx").show();
		}
		java.awt.CardLayout cl = (java.awt.CardLayout)
			getContentPane().getLayout();
		cl.show(getContentPane(), "empty");
		itemConnect.setEnabled(true);
		itemDisconnect.setEnabled(false);
		itemSend.setEnabled(false);
		itemNew.setEnabled(false);
		jMenu3.setVisible(false);
		server = null;
		userName = "";
		password = "";
		users = null;
		chatRooms = null;
		pass.setText("");
	}//GEN-LAST:event_itemDisconnectActionPerformed

	private void jButton2ActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jButton2ActionPerformed
	{//GEN-HEADEREND:event_jButton2ActionPerformed
		int idx = jList3.getSelectedIndex();
		if (idx == -1)
			return;
		User thisUser = (User) users.elementAt(idx);
		PasswordChangeDialog pcd = new PasswordChangeDialog(this);
		pcd.show();
		if (pcd.passwordHasChanged())
			thisUser.setPassword(pcd.getNewPassword());
		itemSend.setEnabled(true);
	}//GEN-LAST:event_jButton2ActionPerformed

	private void jButton7ActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jButton7ActionPerformed
	{//GEN-HEADEREND:event_jButton7ActionPerformed
		int idx = jList3.getSelectedIndex();
		if (idx == -1)
			return;
		User thisUser = (User) users.elementAt(idx);
		String maxNr = jtf3.getText();
		byte max = 0;
		try
		{
			User tempUser = new User(jtf1.getText(), "");
			int size = users.size();
			boolean test = false;
			for (int i = 0; i < size; i++)
				if ((tempUser.equals(users.elementAt(i))) &&
					(i != idx))
					test = true;;
			if (test)
			{
				new ErrorDialog(this, "This login name " +
					"already exists.").show();
				return;
			}
			max = Byte.parseByte(maxNr);
			thisUser.setLoginName(jtf1.getText());
			thisUser.setName(jtf2.getText());
			thisUser.setMaximumNumberOfChatRooms(max);
			thisUser.setContactInfo(jtf4.getText());
			DefaultMutableTreeNode root = (DefaultMutableTreeNode)
				chats.getModel().getRoot();
			Enumeration rootChildren = root.children();
			while (rootChildren.hasMoreElements())
			{
				DefaultMutableTreeNode node = 
					(DefaultMutableTreeNode)
					rootChildren.nextElement();
				User current = (User) node.getUserObject();
				if (current.equals(thisUser))
				{
					Enumeration curChildren =
						node.children();
					while (curChildren.
						hasMoreElements())
					{
						DefaultMutableTreeNode tmpNode =
							(DefaultMutableTreeNode)
							curChildren.
							nextElement();
						ChatRoom room = (ChatRoom)
							tmpNode.getUserObject();
						room.setOwnerLoginName(
							current.getLoginName());
					}
				}
			}
			jList3.updateUI();
			chats.updateUI();
			itemSend.setEnabled(true);
		}
		catch (NumberFormatException e)
		{
			new ErrorDialog(this, "Max nr. of chat must " +
				"be in [0-127]").show();
		}
	}//GEN-LAST:event_jButton7ActionPerformed

	private void itemSendActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_itemSendActionPerformed
	{//GEN-HEADEREND:event_itemSendActionPerformed
		reconnectIfNecesarry();
		chatRooms = new Vector();
		DefaultMutableTreeNode root = (DefaultMutableTreeNode)
			chats.getModel().getRoot();
		Enumeration rootChildren = root.children();
		while (rootChildren.hasMoreElements())
		{
			DefaultMutableTreeNode node = (DefaultMutableTreeNode)
				rootChildren.nextElement();
			Enumeration nodeChildren = node.children();
			while (nodeChildren.hasMoreElements())
			{
				DefaultMutableTreeNode child = 
					(DefaultMutableTreeNode)
					nodeChildren.nextElement();
				ChatRoom room = (ChatRoom) child.
					getUserObject();
				chatRooms.addElement(room);
			}
		}
		try
		{
			server.submitChanges(userName, password, chatRooms,
				users);
			itemSend.setEnabled(false);
		}
		catch (LockException e)
		{
			new ErrorDialog(this, "The server is locked. " +
				"Try later.").show();
		}
		catch (RemoteException e)
		{
			new ErrorDialog(this, "An error occured (maybe " +
				"because the server is down).").show();
		}
		catch (Exception e) {}
	}//GEN-LAST:event_itemSendActionPerformed

	private void jButton6ActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jButton6ActionPerformed
	{//GEN-HEADEREND:event_jButton6ActionPerformed
		DefaultMutableTreeNode node = (DefaultMutableTreeNode)
			chats.getLastSelectedPathComponent();
		TreePath tp = chats.getSelectionPath();
		if ((node == null) || (node.getLevel() != 2))
			return;
		ChatRoom room = (ChatRoom) node.getUserObject();
		String maxNr = jtf13.getText();
		String priority = jtf15.getText();
		byte max = 0;
		byte pr = 0;
		try
		{
			max = Byte.parseByte(maxNr);
			try
			{
				pr = Byte.parseByte(priority);
				room.setName(jtf10.getText());
				room.setDescription(jtf12.getText());
				room.setMaxUsersAllowed(max);
				room.setPolicyMessage(jtf14.getText());
				room.setPriority(pr);
				chats.updateUI();
				chats.setSelectionPath(tp);
				itemSend.setEnabled(true);
			}
			catch (NumberFormatException e)
			{
				new ErrorDialog(this, "Priority must be in " +
					"[0-127]").show();
			}
		}
		catch (NumberFormatException e)
		{
			new ErrorDialog(this, "Max. users must be in [0-127]").
				show();
		}
	}//GEN-LAST:event_jButton6ActionPerformed

	private void jButton1ActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jButton1ActionPerformed
	{//GEN-HEADEREND:event_jButton1ActionPerformed
		java.awt.CardLayout cl = (java.awt.CardLayout)
			getContentPane().getLayout();
		try
		{
			InetAddress.getByName(host.getText());
			server = (ChatAdmin) Naming.lookup(
				"//" + host.getText() + ":" + port.getText() +
				"/" + "chatAdminServ");
			userName = user.getText();
			password = pass.getText();
			User thisUser = server.getUser(userName, password);
			connected = true;
			if (thisUser.getLoginName().equals("admin"))
				admin = true;
			else
				admin = false;
			users = new Vector();
			nodes = new Vector();
			DefaultMutableTreeNode root = 
				new DefaultMutableTreeNode("User(s)");
			chats = new JTree(root);
			if (admin)
				users = server.getUsers(password);
			else
				users.addElement(thisUser);
			int size = users.size();
			for (int i = 0; i < size; i++)
			{
				thisUser = (User) users.elementAt(i);
				nodes.addElement(thisUser.getLoginName());
				root.add(new DefaultMutableTreeNode(thisUser));
			}
			modifiyUsersList();
			jList3.setSelectedIndex(0);
			jList3.addListSelectionListener(
				new ListSelectionListener()
			{
				public void valueChanged(ListSelectionEvent e)
				{
					if (jList3.isSelectionEmpty())
					{
						changeUser(null);
						return;
					}
					else
					{
						int idx = jList3.
							getSelectedIndex();
						changeUser((User) users.
							elementAt(idx));
					}
				}
			});
			
			thisUser = (User) users.firstElement();
			changeUser(thisUser);
			itemDisconnect.setEnabled(true);
			itemConnect.setEnabled(false);
			itemNew.setEnabled(true);
			if (!admin)
			{
				jtf1.setEnabled(false);
				jtf2.setEnabled(false);
				jtf3.setEnabled(false);
				jtf4.setEnabled(false);
				jButton5.setVisible(false);
				itemNewUser.setVisible(false);
				jButton7.setVisible(false);
				jMenu3.setVisible(false);
			}
			else
			{
				jtf1.setEnabled(true);
				jtf2.setEnabled(true);
				jtf3.setEnabled(true);
				jtf4.setEnabled(true);
				jButton5.setVisible(true);
				itemNewUser.setVisible(true);
				jButton7.setVisible(true);
				jMenu3.setVisible(true);
				try
				{
					 managerServer = (ChatAdminManager) 
						Naming.lookup("//" +
						host.getText() + ":" +
						port.getText() + "/" +
						"chatAdminManagerServ");
				}
				catch (Throwable e)
				{
					itemStart.setEnabled(false);
					itemStop.setEnabled(false);
					itemRestart.setEnabled(false);
					new ErrorDialog(this, "Error! " +
						"Starting/stopping the server" +
						" is disabled.").
						show();
				}
			}
			
			chatRooms = server.getChatRooms(userName, password);
			size = chatRooms.size();
			for (int i = 0; i < size; i++)
			{
				ChatRoom room = (ChatRoom) chatRooms.
					elementAt(i);
				int idx = nodes.indexOf(
					room.getOwnerLoginName());
				if (idx != -1)
				{
					DefaultMutableTreeNode node = 
						(DefaultMutableTreeNode)
						root.getChildAt(idx);
					DefaultMutableTreeNode tmp;
					tmp = new DefaultMutableTreeNode(room);
					tmp.setAllowsChildren(false);
					node.add(tmp);
				}
			}
			chatRooms = null;
			chats.getSelectionModel().setSelectionMode
				(TreeSelectionModel.SINGLE_TREE_SELECTION);
			chats.putClientProperty("JTree.lineStyle", "Angled");
			chats.addTreeSelectionListener(
				new TreeSelectionListener()
			{
				public void valueChanged(TreeSelectionEvent e)
				{
					DefaultMutableTreeNode node = 
						(DefaultMutableTreeNode)
						chats.
						getLastSelectedPathComponent();
					if ((node == null) || 
						(node.getLevel() != 2))
					{
						changeChatRoom(null);
						return;
					}
					changeChatRoom((ChatRoom) 
						node.getUserObject());
				}
			});
			chats.addMouseListener(new MouseAdapter()
			{
				public void mouseReleased(MouseEvent evt)
				{
					if (evt.isPopupTrigger() &&
						(chats.getPathForLocation(
							evt.getX(), evt.getY())
							!= null))
					{
						chats.setSelectionPath(chats.
							getPathForLocation(
							evt.getX(),
							evt.getY()));
						DefaultMutableTreeNode node =
							(DefaultMutableTreeNode) 
							chats.
							getLastSelectedPathComponent();
						if ((node != null) && 
							(node.getLevel() == 2))
							showPopup(evt);
					}
				}
			});
			
			jScrollPane2.setViewportView(chats);
			cl.show(getContentPane(), "main");
		}
		catch (java.net.UnknownHostException e)
		{
			new ErrorDialog(this, "Unknown host.").show();
		}
		catch (NotBoundException e)
		{
			new ErrorDialog(this, "Couldn't find server object in" +
				" remote host registry.").show();
		}
		catch (java.lang.IllegalArgumentException e)
		{
			new ErrorDialog(this, "The port is incorrect.").show();
		}
		catch (MalformedURLException e)
		{
			new ErrorDialog(this, "The port is incorrect.").show();
		}
		catch (UserNotFoundException e)
		{
			new ErrorDialog(this, "Wrong user name.").show();
		}
		catch (InvalidPasswordException e)
		{
			new ErrorDialog(this, "Wrong password.").show();
		}
		catch (RemoteException e)
		{
			new ErrorDialog(this, "An error occured (maybe " +
				"because the server is down).").show();
		}
	}//GEN-LAST:event_jButton1ActionPerformed

	private void itemHelpActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_itemHelpActionPerformed
	{//GEN-HEADEREND:event_itemHelpActionPerformed
		javax.swing.JLabel text1;
		javax.swing.JPanel pan;
		javax.swing.JButton ok;
		final javax.swing.JDialog help;
		
		help = new javax.swing.JDialog(this, true);
		text1 = new javax.swing.JLabel();
		pan = new javax.swing.JPanel();
		ok = new javax.swing.JButton();

		help.getContentPane().setLayout(new java.awt.GridLayout(2, 0));
		help.setTitle("HELP");
		help.setBounds(getX()+160, getY()+100, 200, 100);
		help.setResizable(false);

		text1.setText("Sorry, help is not available yet.");
		text1.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
		help.getContentPane().add(text1);

		ok.setText("Curses!");
		ok.addActionListener(new java.awt.event.ActionListener()
		{
			public void actionPerformed(java.awt.event.ActionEvent evt)
			{
				help.setVisible(false);
				help.dispose();
			}
		});

		pan.add(ok);
		help.getContentPane().add(pan);
		help.show();
	}//GEN-LAST:event_itemHelpActionPerformed

	private void itemAboutActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_itemAboutActionPerformed
	{//GEN-HEADEREND:event_itemAboutActionPerformed
		javax.swing.JLabel text1;
		javax.swing.JLabel text2;
		javax.swing.JPanel pan;
		javax.swing.JButton ok;
		final javax.swing.JDialog about;
		
		about = new javax.swing.JDialog(this, true);
		text1 = new javax.swing.JLabel();
		text2 = new javax.swing.JLabel();
		pan = new javax.swing.JPanel();
		ok = new javax.swing.JButton();

		about.getContentPane().setLayout(new java.awt.GridLayout(3, 0));
		about.setTitle("About...");
		about.setBounds(getX()+100, getY()+100, 320, 140);
		about.setResizable(false);

		text1.setText("This software is copyrighted (C) to Bogdan DUMITRIU.");
		text1.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
		about.getContentPane().add(text1);

		text2.setText("Date of release: September, 2001");
		text2.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
		about.getContentPane().add(text2);

		ok.setText("So what do I care?!");
		ok.addActionListener(new java.awt.event.ActionListener()
		{
			public void actionPerformed(java.awt.event.ActionEvent evt)
			{
				about.setVisible(false);
				about.dispose();
			}
		});

		pan.add(ok);
		about.getContentPane().add(pan);
		about.show();
	}//GEN-LAST:event_itemAboutActionPerformed

	private void changeUser(User user)
	{
		if (user != null)
		{
			jtf1.setText(user.getLoginName());
			jtf2.setText(user.getName());
			jtf3.setText(Byte.toString(user.
				getMaximumNumberOfChatRooms()));
			jtf4.setText(user.getContactInfo());
		}
		else
		{
			jtf1.setText("");
			jtf2.setText("");
			jtf3.setText("");
			jtf4.setText("");
		}
	}
	
	private void changeChatRoom(ChatRoom chatRoom)
	{
		if (chatRoom != null)
		{
			jtf10.setText(chatRoom.getName());
			jtf11.setText(chatRoom.getOwnerLoginName());
			jtf12.setText(chatRoom.getDescription());
			jtf13.setText(new Byte(chatRoom.getMaxUsersAllowed()).toString());
			jtf14.setText(chatRoom.getPolicyMessage());
			jtf15.setText(new Byte(chatRoom.getPriority()).toString());
			if (chatRoom.getUserLog() != null)
				jtf16.setText(chatRoom.getUserLog().
					getFileName());
			else
				jtf16.setText("");
			if (chatRoom.getAdminLog() != null)
				jtf17.setText(chatRoom.getAdminLog().
					getFileName());
			else
				jtf17.setText("");
		}
		else
		{
			jtf10.setText("");
			jtf11.setText("");
			jtf12.setText("");
			jtf13.setText("");
			jtf14.setText("");
			jtf15.setText("");
			jtf16.setText("");
			jtf17.setText("");
		}
	}

	private void modifiyUsersList()
	{
		jList3.setListData(users);
		jList3.setSelectedIndex(users.size()-1);
		jList3.updateUI();
	}
	
	private void showPopup(MouseEvent evt)
	{
		JPopupMenu delete = new JPopupMenu();
		JMenuItem itemDelete = new JMenuItem("Delete");
		itemDelete.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				DefaultMutableTreeNode root = 
					(DefaultMutableTreeNode)
					chats.getModel().getRoot();
				DefaultMutableTreeNode dNode = 
					(DefaultMutableTreeNode) chats.
					getLastSelectedPathComponent();
				Enumeration rootChildren =
					root.children();
				while (rootChildren.hasMoreElements())
				{
					DefaultMutableTreeNode node =
						(DefaultMutableTreeNode)
						rootChildren.
						nextElement();
					try
					{
						node.remove(dNode);
					}
					catch (IllegalArgumentException e)
					{}
				}
				chats.updateUI();
				itemSend.setEnabled(true);
			}
		});
		delete.add(itemDelete);
		delete.show(evt.getComponent(), evt.getX(), evt.getY());
	}
	
	private void reconnectIfNecesarry()
	{
		try
		{
			if (server != null)
				server.isLocked();
			else
				try
				{
					server = (ChatAdmin) Naming.lookup(
						"//" + host.getText() + ":" +
						port.getText() +
						"/chatAdminServ");
				}
				catch (Throwable ex) {}
		}
		catch (RemoteException e)
		{
			try
			{
				server = (ChatAdmin) Naming.lookup("//" +
					host.getText() + ":" + port.getText() +
					"/chatAdminServ");
			}
			catch (Throwable ex)
			{
				server = null;
			}
		}
	}

	private void itemExitActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_itemExitActionPerformed
	{//GEN-HEADEREND:event_itemExitActionPerformed
		exitForm(null);
	}//GEN-LAST:event_itemExitActionPerformed

	private void itemConnectActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_itemConnectActionPerformed
	{//GEN-HEADEREND:event_itemConnectActionPerformed
		java.awt.CardLayout cl = (java.awt.CardLayout) 
			getContentPane().getLayout();
		cl.show(getContentPane(), "con");
	}//GEN-LAST:event_itemConnectActionPerformed

	/** Exit the Application */
    private void exitForm(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_exitForm
		reconnectIfNecesarry();
		if (itemSend.isEnabled())
		{
			QuestionDialog qd = new QuestionDialog(this, 
				"Changes have not been sent yet. Are you " +
				"sure you want to exit without " +
				"sending the changes?");
			qd.show();
			if (!qd.answer())
				return;
		}
		try
		{
			if ((admin) && (server != null) && (server.isLocked()))
			{
				QuestionDialog qd = new QuestionDialog(this, 
					"The server is locked. This means " +
					"that users cannot submit any " +
					"changes before you log on again and " +
					"unlock the server. Are you sure you " +
					"want to leave the server locked?");
				qd.show();
				if (!qd.answer())
					return;
			}
			if ((admin) && (managerServer != null) &&
				(managerServer.isStopped()))
			{
				QuestionDialog qd = new QuestionDialog(this, 
					"The server is stopped. This means " +
					"that no one can use it in any " +
					"way before you log on again and " +
					"restart it. Are you sure you " +
					"want to leave the server stopped?");
				qd.show();
				if (!qd.answer())
					return;
			}
		}
		catch (RemoteException e)
		{}
		
		System.exit(0);
    }//GEN-LAST:event_exitForm

	/**
	 * @param args the command line arguments
	 */
	public static void main(String args[])
	{
		ChatAdministration main = new ChatAdministration();
		main.setBounds(200, 200, 500, 300);
		//main.setResizable(false);
		main.show();
	}

	private ChatAdmin server;
	private ChatAdminManager managerServer;
	private boolean admin = false;
	private boolean connected = false;
	private String userName = "";
	private String password = "";
	private Vector users;
	private Vector chatRooms;
	private Vector nodes;
	private JTree chats;
	private int count = 1;
	
	// Variables declaration - do not modify//GEN-BEGIN:variables
	private javax.swing.JMenuBar jMenuBar1;
	private javax.swing.JMenu jMenu1;
	private javax.swing.JMenuItem itemConnect;
	private javax.swing.JMenuItem itemDisconnect;
	private javax.swing.JSeparator jSeparator1;
	private javax.swing.JMenu itemNew;
	private javax.swing.JMenuItem itemNewUser;
	private javax.swing.JMenuItem itemNewChatRoom;
	private javax.swing.JMenuItem itemSend;
	private javax.swing.JSeparator jSeparator2;
	private javax.swing.JMenuItem itemExit;
	private javax.swing.JMenu jMenu3;
	private javax.swing.JMenuItem itemLock;
	private javax.swing.JMenuItem itemUnlock;
	private javax.swing.JSeparator jSeparator3;
	private javax.swing.JMenuItem itemStart;
	private javax.swing.JMenuItem itemStop;
	private javax.swing.JMenuItem itemRestart;
	private javax.swing.JMenu jMenu2;
	private javax.swing.JMenuItem itemHelp;
	private javax.swing.JMenuItem itemAbout;
	private javax.swing.JPanel jPanel10;
	private javax.swing.JPanel jPanel3;
	private javax.swing.JPanel jPanel4;
	private javax.swing.JTextField host;
	private javax.swing.JPanel jPanel5;
	private javax.swing.JTextField port;
	private javax.swing.JPanel jPanel6;
	private javax.swing.JTextField user;
	private javax.swing.JPanel jPanel7;
	private javax.swing.JPasswordField pass;
	private javax.swing.JPanel jPanel1;
	private javax.swing.JButton jButton1;
	private javax.swing.JButton jButton3;
	private javax.swing.JPanel jPanel11;
	private javax.swing.JTabbedPane jTabbedPane1;
	private javax.swing.JPanel jPanel2;
	private javax.swing.JSplitPane jSplitPane1;
	private javax.swing.JPanel jPanel9;
	private javax.swing.JTextField jtf1;
	private javax.swing.JTextField jtf2;
	private javax.swing.JTextField jtf3;
	private javax.swing.JTextField jtf4;
	private javax.swing.JPanel jPanel12;
	private javax.swing.JButton jButton2;
	private javax.swing.JButton jButton7;
	private javax.swing.JScrollPane jScrollPane1;
	private javax.swing.JList jList3;
	private javax.swing.JPanel jPanel8;
	private javax.swing.JSplitPane jSplitPane2;
	private javax.swing.JPanel jPanel13;
	private javax.swing.JTextField jtf10;
	private javax.swing.JTextField jtf11;
	private javax.swing.JTextField jtf12;
	private javax.swing.JTextField jtf13;
	private javax.swing.JTextField jtf14;
	private javax.swing.JTextField jtf15;
	private javax.swing.JTextField jtf16;
	private javax.swing.JTextField jtf17;
	private javax.swing.JPanel jPanel14;
	private javax.swing.JPanel jPanel15;
	private javax.swing.JButton jButton6;
	private javax.swing.JPanel jPanel16;
	private javax.swing.JButton jButton5;
	private javax.swing.JScrollPane jScrollPane2;
	// End of variables declaration//GEN-END:variables
}