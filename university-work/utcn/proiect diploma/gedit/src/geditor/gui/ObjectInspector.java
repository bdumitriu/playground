package geditor.gui;

import geditor.engine.tree.*;
import geditor.engine.operations.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

/**
 * Created by IntelliJ IDEA.
 * User: Lorant Zeno Csaszar
 * Email: lcsaszar@inf.ethz.ch
 * Date: Mar 4, 2003
 * Time: 7:35:08 PM
 */
public class ObjectInspector extends JPanel implements FocusListener, ActionListener
{
	public static final int ACTIVATED = 10;
	public static final int DEACTIVATED = 11;
	public final static int WIDTH = 130;
	public final static int HEIGHT = 250;
	public final static int ELEMENT_HEIGHT = 25;

	protected JLabel title;
	protected JTextField note;

	protected JTextField fgColorValueField;
	protected JTextField bgColorValueField;
	protected JTextField leftValueField;
	protected JTextField topValueField;
	protected JTextField widthValueField;
	protected JTextField heightValueField;
	protected JTextField textValueField;

	protected JButton applyButton;

	private ColorHandler fgColorHandler;
	private ColorHandler bgColorHandler;

	private Element textName;
	private Element textValue;

	protected JPanel tableContainer;
	protected JScrollPane scroll;

	private String objectId;

	private int left;
	private int top;
	private int width;
	private int height;
	private String text;

	public ObjectInspector()
	{
		super();
		setName("Object inspector");
		//setOrientation(JToolBar.VERTICAL);

		setPreferredSize(new Dimension(WIDTH + 20, HEIGHT));

		title = new JLabel("Object inspector");
		title.setForeground(Color.blue);

		note = new JTextField("Inactive");
		note.setBackground(Color.lightGray);
		note.setForeground(Color.red);
		note.setFont(new Font("times", Font.BOLD, 14));
		note.setEnabled(false);

		JPanel panel = new JPanel(new BorderLayout());
		JPanel extraPanel = new JPanel();

		tableContainer = generateTableContainer();

		applyButton = new JButton("apply");
		applyButton.addActionListener(this);
		extraPanel.add(applyButton);

		panel.add(tableContainer, BorderLayout.CENTER);
		panel.add(extraPanel, BorderLayout.SOUTH);

		scroll = new JScrollPane(panel);
		scroll.setEnabled(false);
		scroll.setVisible(false);

		JPanel panelNorth = new JPanel(new BorderLayout());
		panelNorth.add(title, BorderLayout.NORTH);
		panelNorth.add(note, BorderLayout.CENTER);

		setLayout(new BorderLayout());
		add(panelNorth, BorderLayout.NORTH);
		add(scroll, BorderLayout.CENTER);
	}

	public JPanel generateTableContainer()
	{
		JPanel tableContainer;

		// name ---> Left
		Element leftName = new Element(new JLabel("Left", JLabel.CENTER));
		leftName.setBounds(0, 0 * ELEMENT_HEIGHT, WIDTH / 2, ELEMENT_HEIGHT);
		// name --> Right
		Element topName = new Element(new JLabel("Top ", JLabel.CENTER));
		topName.setBounds(0, 1 * ELEMENT_HEIGHT, WIDTH / 2, ELEMENT_HEIGHT);
		// name --> width
		Element widthName = new Element(new JLabel("Width", JLabel.CENTER));
		widthName.setBounds(0, 2 * ELEMENT_HEIGHT, WIDTH / 2, ELEMENT_HEIGHT);
		// name --> height
		Element heightName = new Element(new JLabel("Height", JLabel.CENTER));
		heightName.setBounds(0, 3 * ELEMENT_HEIGHT, WIDTH / 2, ELEMENT_HEIGHT);
		// names ---> fgColor
		Element fgColorName = new Element(new JLabel("FgColor", JLabel.CENTER));
		fgColorName.setBounds(0, 4 * ELEMENT_HEIGHT, WIDTH / 2, ELEMENT_HEIGHT);
		// names ---> bgColor
		Element bgColorName = new Element(new JLabel("BgColor", JLabel.CENTER));
		bgColorName.setBounds(0, 5 * ELEMENT_HEIGHT, WIDTH / 2, ELEMENT_HEIGHT);
		// names ---> text field
		textName = new Element(new JLabel("Text", JLabel.CENTER));
		textName.setBounds(0, 6 * ELEMENT_HEIGHT, WIDTH / 2, ELEMENT_HEIGHT);
		textName.setVisible(false);

		// left
		leftValueField = new JTextField();
		leftValueField.addFocusListener(this);
		Element leftValue = new Element(leftValueField);
		leftValue.setBounds(WIDTH / 2, 0 * ELEMENT_HEIGHT, WIDTH / 2, ELEMENT_HEIGHT);
		// right
		topValueField = new JTextField();
		topValueField.addFocusListener(this);
		Element rightValue = new Element(topValueField);
		rightValue.setBounds(WIDTH / 2, 1 * ELEMENT_HEIGHT, WIDTH / 2, ELEMENT_HEIGHT);
		// width
		widthValueField = new JTextField();
		widthValueField.addFocusListener(this);
		Element widthValue = new Element(widthValueField);
		widthValue.setBounds(WIDTH / 2, 2 * ELEMENT_HEIGHT, WIDTH / 2, ELEMENT_HEIGHT);
		// height
		heightValueField = new JTextField();
		heightValueField.addFocusListener(this);
		Element heightValue = new Element(heightValueField);
		heightValue.setBounds(WIDTH / 2, 3 * ELEMENT_HEIGHT, WIDTH / 2, ELEMENT_HEIGHT);
		// fgColor
		fgColorValueField = new JTextField();
		fgColorValueField.setEditable(false);
		fgColorHandler = new ColorHandler((ColorHandler.FG_COLOR));
		fgColorValueField.addMouseListener(fgColorHandler);
		Element fgColorValue = new Element(fgColorValueField);
		fgColorValue.setBounds(WIDTH / 2, 4 * ELEMENT_HEIGHT, WIDTH / 2, ELEMENT_HEIGHT);
		// bgColor
		bgColorValueField = new JTextField();
		bgColorValueField.setEditable(false);
		bgColorHandler = new ColorHandler(ColorHandler.BG_COLOR);
		bgColorValueField.addMouseListener(bgColorHandler);
		Element bgColorValue = new Element(bgColorValueField);
		bgColorValue.setBounds(WIDTH / 2, 5 * ELEMENT_HEIGHT, WIDTH / 2, ELEMENT_HEIGHT);
		// text
		textValueField = new JTextField();
		textValue = new Element(textValueField);
		textValue.setBounds(WIDTH / 2, 6 * ELEMENT_HEIGHT, WIDTH / 2, ELEMENT_HEIGHT);
		textValue.setVisible(false);

		// table container
		tableContainer = new JPanel(null);
		tableContainer.setBorder(BorderFactory.createLoweredBevelBorder());

		// name
		tableContainer.add(fgColorName);
		tableContainer.add(bgColorName);
		tableContainer.add(leftName);
		tableContainer.add(topName);
		tableContainer.add(widthName);
		tableContainer.add(heightName);
		tableContainer.add(textName);
		// values
		tableContainer.add(fgColorValue);
		tableContainer.add(bgColorValue);
		tableContainer.add(leftValue);
		tableContainer.add(rightValue);
		tableContainer.add(widthValue);
		tableContainer.add(heightValue);
		tableContainer.add(textValue);

		tableContainer.setSize(WIDTH, HEIGHT);
		return tableContainer;
	}

	public void loadValues(ObjectProperties prop)
	{
		if (prop.isGroup())
		{
			note.setText("Group");
		}
		else
		{
			note.setText("Single object");
		}

		objectId = prop.getId();

		leftValueField.setText(prop.getLeft() + "");
		topValueField.setText(prop.getTop() + "");
		widthValueField.setText(prop.getWidth() + "");
		heightValueField.setText(prop.getHeight() + "");

		left = prop.getLeft();
		top = prop.getTop();
		width = prop.getWidth();
		height = prop.getHeight();
		text = prop.getText();

		if (prop.getFgColor() != null)
		{
			fgColorValueField.setBackground(prop.getFgColor());
			fgColorHandler.setEnabled(true);
		}
		else
		{
			fgColorValueField.setBackground(Color.lightGray);
			if (!prop.isGroup())
			{
				fgColorHandler.setEnabled(false);
			}
		}

		if (prop.getBgColor() != null)
		{
			bgColorValueField.setBackground(prop.getBgColor());
			bgColorHandler.setEnabled(true);
		}
		else
		{
			bgColorValueField.setBackground(Color.lightGray);
			if (!prop.isGroup())
			{
				bgColorHandler.setEnabled(false);
			}
		}

		if (prop.getText() != null)
		{
			textValue.setVisible(true);
			textName.setVisible(true);
			textValueField.setText(prop.getText());
		}
		else
		{
			textName.setVisible(false);
			textValue.setVisible(false);
		}

		scroll.setVisible(true);
		scroll.setEnabled(true);
		revalidate();
		repaint();
	}

	public void clear()
	{
		scroll.setEnabled(false);
		scroll.setVisible(false);
		note.setText("Inactive");
		repaint();
	}

	public void setStatus(int state)
	{
		if (state == ACTIVATED)
		{
		}
		else if (state == DEACTIVATED)
		{
			clear();
		}
	}

	public int getTopValue()
	{
		int tmp;
		try
		{
			tmp = Integer.parseInt(topValueField.getText());
		}
		catch (NumberFormatException e)
		{
			tmp = -1;
		}

		return tmp;
	}

	public int getLeftValue()
	{
		int tmp;
		try
		{
			tmp = Integer.parseInt(leftValueField.getText());
		}
		catch (NumberFormatException e)
		{
			tmp = -1;
		}

		return tmp;
	}

	public int getHeightValue()
	{
		int tmp;
		try
		{
			tmp = Integer.parseInt(heightValueField.getText());
		}
		catch (NumberFormatException e)
		{
			tmp = -1;
		}

		return tmp;
	}

	public int getWidthValue()
	{
		int tmp;
		try
		{
			tmp = Integer.parseInt(widthValueField.getText());
		}
		catch (NumberFormatException e)
		{
			tmp = -1;
		}

		return tmp;
	}

	public String getTextValue()
	{
		return textValueField.getText();
	}

	public JTextField getBgColorValueField()
	{
		return bgColorValueField;
	}

	public JTextField getFgColorValueField()
	{
		return fgColorValueField;
	}

	public void setLeft(int left)
	{
		leftValueField.setText(left + "");
		this.left = left;
	}

	public void setTop(int top)
	{
		topValueField.setText(top + "");
		this.top = top;
	}

	public void setWidth(int width)
	{
		widthValueField.setText(width + "");
		this.width = width;
	}

	public void setHeight(int height)
	{
		heightValueField.setText(height + "");
		this.height = height;
	}

	public void focusGained(FocusEvent e)
	{}

	public void focusLost(FocusEvent e)
	{
		JTextField tf = (JTextField) e.getSource();
		int nr = 0;
		try
		{
			nr = Integer.parseInt(tf.getText());

			if ((nr < 0))
			{
				tf.grabFocus();
				return;
			}
		}
		catch (NumberFormatException ex)
		{
			tf.grabFocus();
		}
	}

	public void actionPerformed(ActionEvent e)
	{
		if (e.getSource() == applyButton)
		{
			if (getWidthValue() < 1 || getHeightValue() < 1 || getTopValue() == -1 || getLeftValue() == -1)
			{
				return;
			}

			if (top != getTopValue() || left != getLeftValue())
			{
				TranslationOperation op = new TranslationOperation(objectId, getLeftValue() - left,
					getTopValue() - top);

				op.applyTo(MainFrame.getInstance().getWorkArea().getDocument().getRoot());

			}

			if (width != getWidthValue() || height != getHeightValue())
			{
				ScaleOperation op = new ScaleOperation(objectId, getLeftValue(), getTopValue(),
					(double) getWidthValue() / width, (double) getHeightValue() / height);

				op.applyTo(MainFrame.getInstance().getWorkArea().getDocument().getRoot());

			}

			if (textValue.isVisible())
			{	
				if (!text.equals(getTextValue()))
				{
					SetTextOperation op = new SetTextOperation(objectId, getTextValue());
					op.applyTo(MainFrame.getInstance().getWorkArea().getDocument().getRoot());

				}
			}

			MainFrame.getInstance().getWorkArea().repaint();

			top = getTopValue();
			left = getLeftValue();
			width = getWidthValue();
			height = getHeightValue();
			text = getTextValue();
		}
	}

	class Element extends JComponent
	{
		private JComponent comp;

		public Element(JComponent comp)
		{
			super();
			this.comp = comp;
			setBorder(BorderFactory.createCompoundBorder(BorderFactory.createLineBorder(Color.lightGray),
				BorderFactory.createLoweredBevelBorder()));
			setLayout(new BorderLayout());
			comp.setBackground(Color.white);
			setBackground(Color.white);
			add(comp, BorderLayout.CENTER);
		}

		public Component getComponent()
		{
			return comp;
		}
	}

	class ColorHandler extends MouseAdapter
	{
		public final static int FG_COLOR = 1;
		public final static int BG_COLOR = 2;

		private int type;

		private boolean enabled;

		public ColorHandler(int type)
		{
			this.type = type;
		}

		public boolean isEnabled()
		{
			return enabled;
		}

		public void setEnabled(boolean enabled)
		{
			this.enabled = enabled;
		}

		public void mouseClicked(MouseEvent e)
		{
			if (enabled)
			{
				ColorChooserFrame f = null;
				if (type == FG_COLOR)
				{
					//oldColor = fgColorValueField.getBackground();
					f = new ColorChooserFrame(fgColorValueField.getBackground(), ColorChooserFrame.FG_COLOR);
				}
				else if (type == BG_COLOR)
				{
					//oldColor = bgColorValueField.getBackground();
					f = new ColorChooserFrame(bgColorValueField.getBackground(), ColorChooserFrame.BG_COLOR);
				}
				f.setVisible(true);
			}
		}
	}

	class ColorChooserFrame extends JDialog implements ActionListener
	{
		public final static int FG_COLOR = 31;
		public final static int BG_COLOR = 32;

		protected JColorChooser cc;
		protected JButton ok, cancel;

		private int type;

		public ColorChooserFrame(Color initColor, int type)
		{
			super(MainFrame.getInstance(), "Color chooser", true);
			setSize(300, 300);
			this.type = type;
			cc = new JColorChooser(initColor);

			if (type == FG_COLOR)
			{
				cc.setBorder(BorderFactory.createTitledBorder("Choose foreground color"));
			}
			else
			{
				cc.setBorder(BorderFactory.createTitledBorder("Choose background color"));
			}

			ok = new JButton("Ok");
			ok.addActionListener(this);
			cancel = new JButton("Cancel");
			cancel.addActionListener(this);

			JPanel buttonPanel = new JPanel();
			buttonPanel.add(ok);
			buttonPanel.add(cancel);

			Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
			Dimension frameSize = getSize();
			if (frameSize.height > screenSize.height)
			{
				frameSize.height = screenSize.height;
			}
			if (frameSize.width > screenSize.width)
			{
				frameSize.width = screenSize.width;
			}
			setLocation((screenSize.width - frameSize.width) / 2, (screenSize.height - frameSize.height) / 2);
			getContentPane().setLayout(new BorderLayout());
			getContentPane().add(cc, BorderLayout.CENTER);
			getContentPane().add(buttonPanel, BorderLayout.SOUTH);
		}

		public void actionPerformed(ActionEvent e)
		{
			String cmd = e.getActionCommand();
			if (cmd.equals("Ok"))
			{
				if (type == FG_COLOR)
				{
					fgColorValueField.setBackground(cc.getColor());

					// generate the actual color change operation
					SetFgColorOperation op = new SetFgColorOperation(cc.getColor(), objectId);
					op.applyTo(MainFrame.getInstance().getWorkArea().getDocument().getRoot());


					MainFrame.getInstance().getWorkArea().repaint();
				}
				else if (type == BG_COLOR)
				{
					bgColorValueField.setBackground(cc.getColor());

					// generate the actual color change operation
					SetBgColorOperation op = new SetBgColorOperation(cc.getColor(), objectId);
					op.applyTo(MainFrame.getInstance().getWorkArea().getDocument().getRoot());


					MainFrame.getInstance().getWorkArea().repaint();
				}
				this.dispose();
			}
			else if (cmd.equals("Cancel"))
			{
				this.dispose();
			}
		}
	}
}