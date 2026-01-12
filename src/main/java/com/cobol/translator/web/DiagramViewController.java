package com.cobol.translator.web;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;

/**
 * Contrôleur pour la page de visualisation des diagrammes algorithmiques
 * Affiche les diagrammes Mermaid.js pour les programmes COBOL convertis en Java Spring Batch
 */
@Controller
@RequestMapping("/diagrams")
public class DiagramViewController {

    /**
     * Affiche la page de visualisation des diagrammes algorithmiques
     * Les diagrammes incluent:
     * - Architecture globale (COBOL vs Java Spring Batch)
     * - Flux d'exécution (flowcharts détaillés)
     * - Flux de données (transformations de données)
     * - Diagrammes de séquence (interactions entre composants)
     * - Diagrammes d'états (machine à états)
     */
    @GetMapping
    public String showDiagramsPage(Model model) {
        return "diagrams";
    }
}
