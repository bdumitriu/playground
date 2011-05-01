package chatAdmin.client;

/**
 *
 * @author Bogdan DUMITRIU
 * @author email: bdumitriu@email.ro
 */
public class QuestionDialog extends javax.swing.JDialog
{
	private String message = "";
	private java.awt.Frame parent;
	private boolean yes;

	public QuestionDialog(java.awt.Frame parent, String message)
	{
		super(parent, true);
		this.parent = parent;
		this.message = message;
		yes = true;
		initComponents();
	}

	private void initComponents()
	{
		jPanel3 = new javax.swing.JPanel();
		jTextArea1 = new javax.swing.JTextArea();
		jPanel5 = new javax.swing.JPanel();
		jPanel6 = new javax.swing.JPanel();
		jPanel7 = new javax.swing.JPanel();
		jButton3 = new javax.swing.JButton();
		jButton4 = new javax.swing.JButton();

		getContentPane().setLayout(new java.awt.GridLayout(2, 0));

		setTitle("Question");
		setResizable(false);
		setBounds(parent.getX()+100, parent.getY()+100, 370, 200);
		addWindowListener(new java.awt.event.WindowAdapter()
		{
			public void windowClosing(java.awt.event.WindowEvent evt)
			{
				closeDialog(evt);
			}
		});

		jTextArea1.setWrapStyleWord(true);
		jTextArea1.setLineWrap(true);
		jTextArea1.setEditable(false);
		jTextArea1.setColumns(20);
		jTextArea1.setRows(3);
		jTextArea1.setForeground(new java.awt.Color(102, 102, 153));
		jTextArea1.setFont(new java.awt.Font("Dialog", 1, 12));
		jTextArea1.setBackground(new java.awt.Color(204, 204, 204));
		jTextArea1.setText(message);
		jPanel3.add(jTextArea1);

		getContentPane().add(jPanel3);

		jPanel5.setLayout(new java.awt.GridLayout(2, 0));

		jPanel5.add(jPanel6);

		jButton3.setText("YES!!!");
		jButton3.addActionListener(new java.awt.event.ActionListener()
		{
			public void actionPerformed(java.awt.event.ActionEvent evt)
			{
				jButton3ActionPerformed(evt);
			}
		});

		jPanel7.add(jButton3);

		jButton4.setText("No way");
		jButton4.setPreferredSize(new java.awt.Dimension(81, 27));
		jButton4.addActionListener(new java.awt.event.ActionListener()
		{
			public void actionPerformed(java.awt.event.ActionEvent evt)
			{
				jButton4ActionPerformed(evt);
			}
		});

		jPanel7.add(jButton4);

		jPanel5.add(jPanel7);

		getContentPane().add(jPanel5);
	}

	private void jButton4ActionPerformed(java.awt.event.ActionEvent evt)
	{
		yes = false;
		setVisible(false);
		dispose();
	}

	private void jButton3ActionPerformed(java.awt.event.ActionEvent evt)
	{
		yes = true;
		setVisible(false);
		dispose();
	}

	public boolean answer()
	{
		return yes;
	}

	private void closeDialog(java.awt.event.WindowEvent evt) {
		setVisible(false);
        	dispose();
	}

	private javax.swing.JPanel jPanel3;
	private javax.swing.JTextArea jTextArea1;
	private javax.swing.JPanel jPanel5;
	private javax.swing.JPanel jPanel6;
	private javax.swing.JPanel jPanel7;
	private javax.swing.JButton jButton3;
	private javax.swing.JButton jButton4;
}
