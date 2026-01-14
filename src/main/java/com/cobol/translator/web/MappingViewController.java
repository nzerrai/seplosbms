package com.cobol.translator.web;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;

/**
 * Contrôleur pour la page de visualisation du mapping COBOL to Java
 */
@Controller
@RequestMapping("/mapping")
public class MappingViewController {

    /**
     * Affiche la page de visualisation du mapping
     */
    @GetMapping
    public String showMappingPage(Model model) {
        return "mapping";
    }
}
