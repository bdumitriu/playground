package ro.bdumitriu.example;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;

@EnableJpaRepositories("ro.bdumitriu.example.persistence.repo")
@EntityScan("ro.bdumitriu.example.persistence.model")
@SpringBootApplication
public class Application {

    public static void main(String[] args) {
        SpringApplication.run(Application.class, args);
    }

    //    @Bean
    //    public CommandLineRunner commandLineRunner(ApplicationContext ctx) {
    //        return args -> {
    //
    //            System.out.println("Let's inspect the beans provided by Spring Boot:");
    //
    //            String[] beanNames = ctx.getBeanDefinitionNames();
    //            Arrays.sort(beanNames);
    //            for (String beanName : beanNames) {
    //                System.out.println(beanName);
    //            }
    //
    //        };
    //    }
}
