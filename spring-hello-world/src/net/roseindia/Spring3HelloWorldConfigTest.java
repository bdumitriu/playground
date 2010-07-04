package net.roseindia;

import org.springframework.context.annotation.AnnotationConfigApplicationContext;

public class Spring3HelloWorldConfigTest {

	public static void main(String[] args) {
		AnnotationConfigApplicationContext context =
				new AnnotationConfigApplicationContext(Spring3HelloWorldConfig.class);
		System.out.println("Calling bean method: sayHello()");
		Spring3HelloWorld bean = (Spring3HelloWorld) context.getBean("spring3HelloWorld");
		bean.sayHello();
	}
}
