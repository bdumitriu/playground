package test;

import client.gui.SelectionList;
import client.gui.SelectionListAdapter;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

/**
 *
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 15, 2003
 */
public class TestSelectionList
{
	public static void main(String[] args)
	{
		String names[] = new String[10];

		names[0] = "Nume 1";
		names[1] = "Nume mai lung 2";
		names[2] = "Nume mult mai lung decat primele doua 3";
		names[3] = "Nume 4";
		names[4] = "Nume 5";
		names[5] = "Nume 6";
		names[6] = "Nume 7";
		names[7] = "Nume 8";
		names[8] = "Nume 9";
		names[9] = "Nume 10";

		SelectionList sl = new SelectionList(SelectionList.DEFAULT_BEHAVIOUR, names, new SelectionListAd());
		JFrame f = new JFrame("x");
		f.getContentPane().add(sl);
		f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		f.pack();
		f.show();
	}
}

class SelectionListAd extends SelectionListAdapter
{
	public String[] getListFor(String text)
	{
		String names[] = new String[10];
		//String names[] = new String[5];

		names[0] = "Nume 1";
		names[1] = "Nume mai lung 2";
		names[2] = "Nume mult mai lung decat primele doua 3";
		names[3] = "Nume 4";
		names[4] = "Nume 5";
		names[5] = "Nume 6";
		names[6] = "Nume 7";
		names[7] = "Nume 8";
		names[8] = "Nume 9";
		names[9] = "Nume 10";

		return names;
	}
}