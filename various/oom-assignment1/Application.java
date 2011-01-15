package ass1;

import ass1.controller.Controller;
import ass1.model.Library;
import ass1.view.ApplicationWindow;

public class Application {
	public static void main(String[] args) {
		Library library = new Library("books.lib");
		Controller controller = new Controller(library);
		ApplicationWindow applicationWindow = new ApplicationWindow(controller);
		applicationWindow.setVisible(true);
	}
}
