package net.roseindia;

import org.springframework.beans.factory.xml.XmlBeanFactory;
import org.springframework.core.io.ClassPathResource;

public class Spring3HelloWorldTest {

	public static void main(String[] args) {
		XmlBeanFactory beanFactory = new XmlBeanFactory(new ClassPathResource("SpringHelloWorld.xml"));
		Spring3HelloWorld bean = (Spring3HelloWorld) beanFactory.getBean("Spring3HelloWorldBean");
		bean.sayHello();
	}
}
