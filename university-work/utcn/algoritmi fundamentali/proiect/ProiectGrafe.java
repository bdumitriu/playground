/**
 * @author Bogdan DUMITRIU
 * @author email: bdumitriu@email.ro
 * @version 1.0
 * @date 02.01.2002
 *
 * Proiect grafe semestrul 1, anul 3.
 * Implementare algoritmii lui Kruskal si Sollin
 * in varianta grafica.
 */
import java.util.*;
import java.applet.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;

public class ProiectGrafe extends JApplet
{
	JPanel p1 = new JPanel();
	JPanel p2 = new JPanel();
	JPanel p3 = new JPanel();
	JPanel p4 = new JPanel();
	JPanel p5 = new JPanel();
	JPanel p6 = new JPanel();
	JPanel p7 = new JPanel();
	JTextField jtf1 = new JTextField(5);
	JButton b1 = new JButton("Ruleaza algoritmul lui Kruskal");
	ButtonGroup bg = new ButtonGroup();
	JRadioButton radio1 =
		new JRadioButton("  matrice de adiacenta", false);
	JRadioButton radio2 =
		new JRadioButton("  lista de vecinatati", false);
	JRadioButton radio3 =
		new JRadioButton("  reprezentare grafica", true);
	CardLayout cl = new CardLayout();
	ZonaDesen desen = new ZonaDesen(jtf1);
	ZonaRezultat kruskal;
	
	public void init()
	{
		radio1.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				Hashtable graf = desen.getGraf();
				MatriceAdiacenta ma = Reprezentari.creeazaMatrice(graf);
				JTextField temp;
				if (ma.size() != 0)
				{
					p6.removeAll();
					p6.setLayout(new GridLayout(ma.size()+1, ma.size()+1));
					temp = new JTextField(3);
					temp.setEditable(false);
					temp.setHorizontalAlignment(JTextField.CENTER);
					temp.setText("   ");
					p6.add(temp);
				}
				for (int i = 1; i <= ma.size(); i++)
				{
					temp = new JTextField(3);
					temp.setEditable(false);
					temp.setHorizontalAlignment(JTextField.CENTER);
					temp.setText(new Integer(i).toString());
					p6.add(temp);
				}
				for (int i = 1; i <= ma.size(); i++)
				{
					temp = new JTextField(3);
					temp.setEditable(false);
					temp.setHorizontalAlignment(JTextField.CENTER);
					temp.setText(new Integer(i).toString());
					p6.add(temp);
					for (int j = 1; j <= ma.size(); j++)
					{
						temp = new JTextField(3);
						temp.setEditable(false);
						temp.setHorizontalAlignment(JTextField.CENTER);
						int cost = ma.get(i, j);
						if (cost != Integer.MAX_VALUE)
						{
							temp.setText(new Integer(cost).toString());
						}
						else
						{
							temp.setText("inf");
						}
						p6.add(temp);
					}
				}
				cl.show(p5, "adiacenta");
			}
		});
		
		radio2.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				Hashtable graf = desen.getGraf();
				ListaVecinatati lv = Reprezentari.creeazaListaVecinatati(graf);
				if (lv.size() != 0)
				{
					p7.removeAll();
					p7.setLayout(new GridLayout(lv.size(), 1));
				}
				JTextField temp;
				for (int i = 1; i <= lv.size(); i++)
				{
					temp = new JTextField();
					temp.setEditable(false);
					StringBuffer text = new StringBuffer(new Integer(i).toString());
					text.append(":");
					for (int j = 1; j <= lv.size(); j++)
					{
						int cost = lv.cost(i, j);
						if (cost != Integer.MAX_VALUE)
						{
							text.append(" ");
							text.append(new Integer(j).toString());
							text.append("/");
							text.append(new Integer(cost).toString());
						}
					}
					text.append(".");
					temp.setText(text.toString());
					p7.add(temp);
				}
				cl.show(p5, "vecinatati");
			}
		});
		
		radio3.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				cl.show(p5, "grafic");
			}
		});
		
		b1.addActionListener(new ActionListener()
		{
			public boolean notContains(int array[], int size, int value)
			{
				for (int i = 0; i < size; i++)
					if (array[i] == value)
						return false;
				return true;
			}

			public void actionPerformed(ActionEvent e)
			{
				Hashtable graf = desen.getGraf();
				ListaVecinatati lv = Reprezentari.creeazaListaVecinatati(graf);
				Hashtable grafMin = new Hashtable();
				int puse[] = new int[lv.size()+1];
				Enumeration en = graf.keys();
				Vector v1, v2;
				int idx = 0;
				
				while (en.hasMoreElements())
				{
					grafMin.put(((Nod) en.nextElement()), new Vector());
				}
				
				int min = Integer.MAX_VALUE;
				int ii = 0;
				int jj = 0;
				for (int i = 1; i <= lv.size(); i++)
				{
					Vector vecini = lv.get(i);
					en = vecini.elements();
					while (en.hasMoreElements())
					{
						Vecin vec = (Vecin) en.nextElement();
						if (vec.getCost() < min)
						{
							min = vec.getCost();
							ii = i;
							jj = vec.getTo();
						}
					}
				}
				if (min != Integer.MAX_VALUE)
				{
					v1 = (Vector) graf.get(new Nod(new Ellipse2D.Float(), ii));
					Muchie m = (Muchie) v1.elementAt(
						v1.indexOf(new Muchie
						(new Nod(new Ellipse2D.Float(), jj), 1)));
					v2 = (Vector) grafMin.get(new Nod(new Ellipse2D.Float(), ii));
					v2.add(m);
					puse[idx++] = ii;
					puse[idx++] = jj;
					while (idx < lv.size())
					{
						min = Integer.MAX_VALUE;
						for (int i = 0; i < idx; i++)
						{
							Vector vecini = lv.get(puse[i]);
							en = vecini.elements();
							while (en.hasMoreElements())
							{
								Vecin vec = (Vecin) en.nextElement();
								if ((vec.getCost() < min) &&
									(notContains(puse, idx, vec.getTo())))
								{
									min = vec.getCost();
									ii = puse[i];
									jj = vec.getTo();
								}
							}
						}
						v1 = (Vector) graf.get(new Nod(new Ellipse2D.Float(), ii));
						m = (Muchie) v1.elementAt(
							v1.indexOf(new Muchie
							(new Nod(new Ellipse2D.Float(), jj), 1)));
						v2 = (Vector) grafMin.get(new Nod(new Ellipse2D.Float(), ii));
						v2.add(m);
						puse[idx++] = jj;
					}
				}
				
				kruskal = new ZonaRezultat(grafMin);
				p5.add(kruskal, "kruskal");
				cl.show(p5, "kruskal");
			}
		});

		bg.add(radio1);
		bg.add(radio2);
		bg.add(radio3);
		
		p1.setLayout(new GridLayout(1, 3));
		p2.setLayout(new FlowLayout());
		p3.setLayout(new GridLayout(2, 1));
		p4.setLayout(new GridLayout(4, 1));
		p5.setLayout(cl);
		
		jtf1.setText("1");
		p2.add(new JLabel("Costul noii muchii: "));
		p2.add(jtf1);
		
		p3.add(b1);
		
		p4.setBorder(new EmptyBorder(0, 20, 0, 0));
		p4.add(new JLabel("Vizualizare:"));
		p4.add(radio1);
		p4.add(radio2);
		p4.add(radio3);
		
		p6.setBorder(new LineBorder(Color.darkGray));
		
		p7.setBorder(new LineBorder(Color.darkGray));
		
		p5.add(p6, "adiacenta");
		p5.add(p7, "vecinatati");
		p5.add(desen, "grafic");
		cl.show(p5, "grafic");
		
		p1.add(p2);
		p1.add(p3);
		p1.add(p4);
		
		this.setSize(755, 425);
		this.getContentPane().setLayout(new FlowLayout());
		this.getContentPane().add(p1);
		this.getContentPane().add(p5);
	}
}

class ZonaDesen extends JPanel
{
	Dimension preferredSize = new Dimension(745, 300);
	Hashtable graf = new Hashtable();
	int nodIdx = 0;
	Nod st = null;
	JTextField costMuchie;

	public ZonaDesen(JTextField jtf)
	{
		costMuchie = jtf;
		setBorder(new LineBorder(Color.darkGray));
		addMouseListener(new MouseInputAdapter()
		{
			public void mousePressed(MouseEvent e)
			{
				int x = e.getX();
				int y = e.getY();
				Integer cost = new Integer(-1);
				try
				{
					cost = new Integer(
						costMuchie.getText());
					if (e.getClickCount() == 1)
					{
						Enumeration en = graf.keys();
						Nod n;
						Ellipse2D.Float el;
						int test = 1;
						while ((en.hasMoreElements()) &&
							(test == 1))
						{
							n = (Nod) en.nextElement();
							if (n.getCerc().contains(x, y))
							{
								if (st == null)
								{
									st = n;
									test = 0;
								}
								else
								{
									Vector v1 =
										(Vector)
										graf.
										get(st);
									Vector v2 = (Vector)
										graf.
										get(n);
									if (!(
										(n == st) ||
										(v1.contains(new Muchie(n, 1))) ||
										(v2.contains(new Muchie(st, 1)))))
									{
										v1.addElement(new Muchie(n, cost.intValue()));
										v2.addElement(new Muchie(st, cost.intValue()));
										st = null;
										test = 0;
									}
								}
							}
						}
						if (test == 0)
						{
							repaint();
						}
					}
				}
				catch (NumberFormatException ex)
				{}
				if (nodIdx > 98)
				{
					return;
				}
				if (e.getClickCount() == 2)
				{
					Ellipse2D.Float cerc = new Ellipse2D.Float(
						x-10, y-10, 20, 20);
					graf.put(new Nod(cerc, ++nodIdx),
						new Vector());
					repaint();
				}
			}
		});
	}

	public Dimension getPreferredSize()
	{
		return preferredSize;
	}

	public void paintComponent(Graphics g)
	{
        	super.paintComponent(g);
		Enumeration en = graf.keys();
		Nod n;
		Ellipse2D.Float el;
		Vector v;
		while (en.hasMoreElements())
		{
			n = (Nod) en.nextElement();
			v = (Vector) graf.get(n);
			Enumeration enV = v.elements();
			int x1 = (int) (n.getCerc().getX()+10);
			int y1 = (int) (n.getCerc().getY()+10);
			while (enV.hasMoreElements())
			{
				Muchie m =(Muchie) enV.nextElement();
				Nod nV = m.getNod();
				int x2 = (int) (nV.getCerc().getX()+10);
				int y2 = (int) (nV.getCerc().getY()+10);
				g.setColor(Color.blue);
				g.drawLine(x1, y1, x2, y2);
				g.setColor(Color.black);
				((Graphics2D) g).drawString(
					new Integer(m.getCost()).toString(),
					(x1+x2)/2,
					(y1+y2)/2);
			}
		}
		en = graf.keys();
		while (en.hasMoreElements())
		{
			n = (Nod) en.nextElement();
			el = n.getCerc();
			g.setColor(Color.green);
			((Graphics2D) g).fill(el);
			g.setColor(Color.black);
			((Graphics2D) g).draw(el);
			((Graphics2D) g).drawString(
				new Integer(n.getNod()).toString(),
				(float) el.getX()+ 4, (float) el.getY() + 16);
		}
	}
	
	public Hashtable getGraf()
	{
		return graf;
	}
}

class ZonaRezultat extends JPanel
{
	Dimension preferredSize = new Dimension(745, 300);
	Hashtable graf = new Hashtable();
	
	public ZonaRezultat(Hashtable graf)
	{
		this.graf = graf;
		setBorder(new LineBorder(Color.darkGray));
	}
	
	public Dimension getPreferredSize()
	{
		return preferredSize;
	}

	public void paintComponent(Graphics g)
	{
        	super.paintComponent(g);
		Enumeration en = graf.keys();
		Nod n;
		Ellipse2D.Float el;
		Vector v;
		while (en.hasMoreElements())
		{
			n = (Nod) en.nextElement();
			v = (Vector) graf.get(n);
			Enumeration enV = v.elements();
			int x1 = (int) (n.getCerc().getX()+10);
			int y1 = (int) (n.getCerc().getY()+10);
			while (enV.hasMoreElements())
			{
				Muchie m =(Muchie) enV.nextElement();
				Nod nV = m.getNod();
				int x2 = (int) (nV.getCerc().getX()+10);
				int y2 = (int) (nV.getCerc().getY()+10);
				g.setColor(Color.blue);
				g.drawLine(x1, y1, x2, y2);
				g.setColor(Color.black);
				((Graphics2D) g).drawString(
					new Integer(m.getCost()).toString(),
					(x1+x2)/2,
					(y1+y2)/2);
			}
		}
		en = graf.keys();
		while (en.hasMoreElements())
		{
			n = (Nod) en.nextElement();
			el = n.getCerc();
			g.setColor(Color.green);
			((Graphics2D) g).fill(el);
			g.setColor(Color.black);
			((Graphics2D) g).draw(el);
			((Graphics2D) g).drawString(
				new Integer(n.getNod()).toString(),
				(float) el.getX()+ 4, (float) el.getY() + 16);
		}
	}
}

class Nod
{
	private Ellipse2D.Float cerc;
	private int nod;
	
	public Nod(Ellipse2D.Float cerc, int nod)
	{
		this.cerc = cerc;
		this.nod = nod;
	}
	
	Ellipse2D.Float getCerc()
	{
		return cerc;
	}
	
	int getNod()
	{
		return nod;
	}
	
	public int hashCode()
	{
		return new Integer(nod).hashCode();
	}
	
	public boolean equals(Object obj)
	{
		if (obj == null)
			return false;
		if (!(obj instanceof Nod))
			return false;
		Nod n = (Nod) obj;
		
		return (n.nod == nod);
	}
}

class Muchie
{
	private Nod nod;
	private int cost;
	
	public Muchie(Nod nod, int cost)
	{
		this.nod = nod;
		this.cost = cost;
	}
	
	Nod getNod()
	{
		return nod;
	}
	
	int getCost()
	{
		return cost;
	}
	
	public boolean equals(Object obj)
	{
		if (obj == null)
			return false;
		if (!(obj instanceof Muchie))
			return false;
		Muchie m = (Muchie) obj;
		
		return (m.nod.equals(nod));
	}
}

class Reprezentari
{
	public static MatriceAdiacenta creeazaMatrice(Hashtable graf)
	{
		MatriceAdiacenta ma;
		Enumeration en = graf.keys();
		int nr = 0;
		while (en.hasMoreElements())
		{
			en.nextElement();
			nr++;
		}
		ma = new MatriceAdiacenta(nr);
		
		en = graf.keys();
		while (en.hasMoreElements())
		{
			Nod n = (Nod) en.nextElement();
			Vector v = (Vector) graf.get(n);

			for (int j = 1; j <= nr; j++)
			{
				Muchie m = new Muchie(new Nod(new Ellipse2D.Float(), j), 1);
				int pos = v.indexOf(m);
				if (pos != -1)
				{
					m = (Muchie) v.elementAt(pos);
					ma.set(n.getNod(), j, m.getCost());
					ma.set(j, n.getNod(), m.getCost());
				}
			}
		}

		return ma;
	}
	
	public static ListaVecinatati creeazaListaVecinatati(Hashtable graf)
	{
		ListaVecinatati lv;
		Enumeration en = graf.keys();
		int nr = 0;
		while (en.hasMoreElements())
		{
			en.nextElement();
			nr++;
		}
		lv = new ListaVecinatati(nr);
		
		en = graf.keys();
		while (en.hasMoreElements())
		{
			Nod n = (Nod) en.nextElement();
			Vector v = (Vector) graf.get(n);

			for (int j = 1; j <= nr; j++)
			{
				Muchie m = new Muchie(new Nod(new Ellipse2D.Float(), j), 1);
				int pos = v.indexOf(m);
				if (pos != -1)
				{
					m = (Muchie) v.elementAt(pos);
					lv.add(n.getNod(), j, m.getCost());
					lv.add(j, n.getNod(), m.getCost());
				}
			}
		}
		
		return lv;
	}
}

class MatriceAdiacenta
{
	private int mat[][];
	private int dim;
	
	public MatriceAdiacenta(int dim)
	{
		this.dim = dim;
		mat = new int[dim+1][dim+1];
		for (int i = 1; i <= dim; i++)
		{
			for (int j = 1; j <= dim; j++)
			{
				mat[i][j] = Integer.MAX_VALUE;
			}
		}
	}
	
	public void set(int i, int j, int value)
	{
		mat[i][j] = value;
	}
	
	public int get(int i, int j)
	{
		return mat[i][j];
	}
	
	public int size()
	{
		return dim;
	}
}

class ListaVecinatati
{
	private Vector lVec[];
	private int dim;
	
	public ListaVecinatati(int dim)
	{
		this.dim = dim;
		lVec = new Vector[dim+1];
		for (int i = 1; i <= dim; i++)
		{
			lVec[i] = new Vector();
		}
	}
	
	public void add(int from, int to, int cost)
	{
		lVec[from].add(new Vecin(to, cost));
	}
	
	public Vector get(int from)
	{
		return lVec[from];
	}
	
	public int cost(int from, int to)
	{
		Enumeration en = lVec[from].elements();
		while (en.hasMoreElements())
		{
			Vecin vec = (Vecin) en.nextElement();
			if (vec.getTo() == to)
				return vec.getCost();
		}
		return Integer.MAX_VALUE;
	}
	
	public int size()
	{
		return dim;
	}
}

class Vecin
{
	private int to;
	private int cost;

	public Vecin(int to, int cost)
	{
		this.to = to;
		this.cost = cost;
	}

	public int getTo()
	{
		return to;
	}

	public int getCost()
	{
		return cost;
	}
}
