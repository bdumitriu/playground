package ro.bdumitriu.example.controller;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;

@Controller
public class SimpleController {

    @Value("${spring.application.name}")
    String applicationName;

    @GetMapping("/")
    public String homePage(Model model) {
        model.addAttribute("applicationName", applicationName);
        return "home";
    }
}
