package work.chat;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import java.net.*;
import java.io.*;

public class ChatClient
{
	public static void main(String args[])
	{
		switch(args.length)
		{
			case 1:
			{
				new SwingChatClient(args[0]);
				break;
			}
			case 2:
			{
				new SwingChatClient(args[0], args[1]);
				break;
			}
			case 3:
			{
				new SwingChatClient(args[0], args[1],
					Integer.parseInt(args[2]));
				break;
			}
			default:
			{
				System.out.println("Usage: " +
					"program_name username [host" +
					" [port]]");
				break;
			}
		}
	}
}
