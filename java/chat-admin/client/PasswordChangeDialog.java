package chatAdmin.client;

/**
 *
 * @author Bogdan DUMITRIU
 * @author email: bdumitriu@email.ro
 */
public class PasswordChangeDialog extends javax.swing.JDialog
{
	private java.awt.Frame parent;
	private boolean passwordChanged = false;
	private String newPassword;

	public PasswordChangeDialog(java.awt.Frame parent)
	{
		super(parent, true);
		this.parent = parent;
		initComponents();
	}

	private void initComponents()
	{
		jLabel1 = new javax.swing.JLabel();
		jPasswordField2 = new javax.swing.JPasswordField();
		jLabel2 = new javax.swing.JLabel();
		jPasswordField3 = new javax.swing.JPasswordField();
		jButton2 = new javax.swing.JButton();
		jButton3 = new javax.swing.JButton();

		getContentPane().setLayout(new java.awt.FlowLayout());

		setTitle("Password change");
		setResizable(false);
		setBounds(parent.getX()+100, parent.getY()+100, 370, 120);
		addWindowListener(new java.awt.event.WindowAdapter()
		{
			public void windowClosing(java.awt.event.WindowEvent evt)
			{
				closeDialog(evt);
			}
		});

		jLabel1.setText("New password: ");
		getContentPane().add(jLabel1);

		jPasswordField2.setPreferredSize(new java.awt.Dimension(200, 21));
		getContentPane().add(jPasswordField2);

		jLabel2.setText("Confirm password: ");
		getContentPane().add(jLabel2);

		jPasswordField3.setPreferredSize(new java.awt.Dimension(200, 21));
		getContentPane().add(jPasswordField3);

		jButton2.setText("Change it");
		getContentPane().add(jButton2);

		jButton3.setText("Ups! Changed my mind...");
		getContentPane().add(jButton3);
		
		jButton2.addActionListener(new java.awt.event.ActionListener()
		{
			public void actionPerformed(java.awt.event.ActionEvent 
				evt)
			{
				if (jPasswordField2.getText().equals(
					jPasswordField3.getText()))
				{
					passwordChanged = true;
					newPassword = jPasswordField2.getText();
					setVisible(false);
					dispose();
				}
				else
				{
					new ErrorDialog(parent, "Passwords " +
						"don't match. Try again").
						show();
					jPasswordField2.setText("");
					jPasswordField3.setText("");
				}
			}
		});
		
		jButton3.addActionListener(new java.awt.event.ActionListener()
		{
			public void actionPerformed(java.awt.event.ActionEvent evt)
			{
				setVisible(false);
				dispose();
			}
		});
	}

	private void closeDialog(java.awt.event.WindowEvent evt)
	{
		setVisible(false);
		dispose();
	}

	public boolean passwordHasChanged()
	{
		return passwordChanged;
	}
	
	public String getNewPassword()
	{
		String trash = newPassword;
		newPassword = null;
		return trash;
	}

	private javax.swing.JLabel jLabel1;
	private javax.swing.JPasswordField jPasswordField2;
	private javax.swing.JLabel jLabel2;
	private javax.swing.JPasswordField jPasswordField3;
	private javax.swing.JButton jButton2;
	private javax.swing.JButton jButton3;
}
