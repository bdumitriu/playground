package net.roseindia;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class Spring3HelloWorldConfig {

	public @Bean Spring3HelloWorld spring3HelloWorld() {
		return new Spring3HelloWorld();
	}
}
