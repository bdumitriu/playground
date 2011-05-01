package gui;

import bm.Entry;
import bm.TableEntry;
import bm.TableEntryOrder;

import javax.swing.*;
import java.util.Vector;
import java.util.ArrayList;
import java.util.Collections;
import java.awt.*;
import java.awt.event.KeyEvent;

/**
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Jan 15, 2005
 */
public class ListWordsScreen extends JPanel
{
	public ListWordsScreen(ApplicationGui parent)
	{
		this.parent = parent;

		wordsTable = new JTable(getRowData(), getColumnNames());

		setLayout(new BorderLayout());

		JPanel titlePanel = new JPanel();

		JLabel titleLabel = new JLabel("Words from dictionary");
		titleLabel.setFont(new Font("Monospaced", Font.BOLD, 18));
		titleLabel.setHorizontalAlignment(JLabel.CENTER);

		titlePanel.add(titleLabel);

		JScrollPane jsp = new JScrollPane(wordsTable);

		add(titleLabel, BorderLayout.NORTH);
		add(jsp, BorderLayout.CENTER);

	}

	public void focusDefaultComponent()
	{
		wordsTable.requestFocus();
	}

	private Vector<TableEntry> getRowData()
	{
		ArrayList<Entry> entries = parent.getEntryManager().getEntries();
		Vector<TableEntry> rowData = new Vector<TableEntry>(entries.size());

		for (Entry entry : entries)
		{
			rowData.add(new TableEntry(entry, new DefaultOrder()));
		}

		filter(rowData);
		sort(rowData);
		return rowData;
	}

	private Vector<String> getColumnNames()
	{
		Vector<String> columnNames = new Vector<String>();
		columnNames.add("Dutch word");
		columnNames.add("English word");
		columnNames.add("Romanian word");
		columnNames.add("Type");
		columnNames.add("Article");
		columnNames.add("Extra information");
		columnNames.add("Sample sentence(s)");

		return columnNames;
	}

	/**
	 * Filters the <code>rowData</code> Vector<Entry> depending of the value of the <code>filterOn</code> data
	 * memeber.
	 *
	 * @param rowData the Vector to filter
	 */
	private void filter(Vector<TableEntry> rowData)
	{
		return;
	}

	/**
	 * Sorts the <code>rowData</code> Vector<Entry> depending of the value of the <code>sortOrder</code> data
	 * memeber.
	 *
	 * @param rowData the Vector to sort
	 */
	private void sort(Vector<TableEntry> rowData)
	{
		Collections.sort(rowData);
		return;
	}



	private JTable wordsTable;

	private ApplicationGui parent;
}

class DefaultOrder implements TableEntryOrder
{
	public int elementAt(int index)
	{
		switch(index)
		{
			case 0:
			{
				return dutchWord;
			}
			case 1:
			{
				return englishWord;
			}
			case 2:
			{
				return romanianWord;
			}
			case 3:
			{
				return type;
			}
			case 4:
			{
				return article;
			}
			case 5:
			{
				return extra;
			}
			case 6:
			{
				return sampleExpression;
			}
			default:
			{
				return -1;
			}
		}
	}

	public int indexFor(int attribute)
	{
		switch(attribute)
		{
			case dutchWord:
			{
				return 0;
			}
			case englishWord:
			{
				return 1;
			}
			case romanianWord:
			{
				return 2;
			}
			case type:
			{
				return 3;
			}
			case article:
			{
				return 4;
			}
			case extra:
			{
				return 5;
			}
			case sampleExpression:
			{
				return 6;
			}
			default:
			{
				return -1;
			}
		}
	}
}