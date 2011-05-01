package chatAdmin.client;

/**
 *
 * @author Bogdan DUMITRIU
 * @author email: bdumitriu@email.ro
 */
public class ErrorDialog extends javax.swing.JDialog
{
	
	String message = "";
	java.awt.Frame parent;
	
	public ErrorDialog(java.awt.Frame parent, String message)
	{
		super(parent, true);
		this.parent = parent;
		this.message = message;
		initComponents();
	}
	
	private void initComponents()
	{
		jLabel2 = new javax.swing.JLabel();
		jPanel1 = new javax.swing.JPanel();
		jButton1 = new javax.swing.JButton();
		
		getContentPane().setLayout(new java.awt.GridLayout(2, 0));
		
		setTitle("Error");
		setResizable(false);
		addWindowListener(new java.awt.event.WindowAdapter()
		{
			public void windowClosing(java.awt.event.WindowEvent evt)
			{
				closeDialog(evt);
			}
		});
		
		jLabel2.setText(message);
		jLabel2.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
		getContentPane().add(jLabel2);
		
		jButton1.setText("Damn!");
		jButton1.addActionListener(new java.awt.event.ActionListener()
		{
			public void actionPerformed(java.awt.event.ActionEvent evt)
			{
				jButton1ActionPerformed(evt);
			}
		});
		
		jPanel1.add(jButton1);
		
		getContentPane().add(jPanel1);
		setBounds(parent.getX()+100, parent.getY()+100, 300, 200);
		setSize(new java.awt.Dimension(330, 100));
	}

	private void jButton1ActionPerformed(java.awt.event.ActionEvent evt)
	{
		setVisible(false);
		dispose();
	}
    
	private void closeDialog(java.awt.event.WindowEvent evt)
	{
		setVisible(false);
		dispose();
	}

	private javax.swing.JLabel jLabel2;
	private javax.swing.JPanel jPanel1;
	private javax.swing.JButton jButton1;
}
