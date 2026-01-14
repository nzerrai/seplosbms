package com.cobol.translator.redefines;

import java.util.ArrayList;
import java.util.List;

/**
 * Stocke les informations complètes sur une clause REDEFINES
 * Gère les redéfinitions multiples et chaînées
 */
public class RedefinesInfo {
    
    private final String originalFieldName;
    private final String originalFieldType;
    private final int fieldSize;
    private final List<RedefinesView> views;
    
    public RedefinesInfo(String originalFieldName, String originalFieldType, int fieldSize) {
        this.originalFieldName = originalFieldName;
        this.originalFieldType = originalFieldType;
        this.fieldSize = fieldSize;
        this.views = new ArrayList<>();
        
        // Ajouter la vue originale
        addView(new RedefinesView(originalFieldName, originalFieldType, fieldSize, ViewType.ORIGINAL, null));
    }
    
    /**
     * Ajoute une nouvelle vue (redéfinition)
     */
    public void addView(RedefinesView view) {
        views.add(view);
    }
    
    /**
     * Vérifie si une vue est compatible avec la taille du champ original
     */
    public boolean isViewCompatible(RedefinesView view) {
        return view.getSize() <= fieldSize;
    }
    
    /**
     * Analyse la compatibilité de tous les types
     */
    public List<String> analyzeCompatibility() {
        List<String> incompatibilities = new ArrayList<>();
        
        for (RedefinesView view : views) {
            if (!isViewCompatible(view)) {
                incompatibilities.add(String.format(
                    "View '%s' (size: %d) exceeds original field size: %d",
                    view.getName(), view.getSize(), fieldSize
                ));
            }
            
            // Vérifier la compatibilité des types
            if (view.getViewType() == ViewType.NUMERIC) {
                boolean hasNonNumericOriginal = originalFieldType.startsWith("X");
                if (hasNonNumericOriginal && view.getSize() > fieldSize) {
                    incompatibilities.add(String.format(
                        "Numeric view '%s' may contain non-numeric data from alphanumeric field",
                        view.getName()
                    ));
                }
            }
        }
        
        return incompatibilities;
    }
    
    /**
     * Retourne le nombre de redéfinitions (sans compter l'original)
     */
    public int getRedefinitionCount() {
        return views.size() - 1; // -1 pour exclure la vue originale
    }
    
    /**
     * Retourne toutes les vues incluant l'originale
     */
    public List<RedefinesView> getAllViews() {
        return new ArrayList<>(views);
    }
    
    /**
     * Retourne uniquement les redéfinitions (sans l'originale)
     */
    public List<RedefinesView> getRedefinitions() {
        return views.stream()
                .filter(v -> v.getViewType() != ViewType.ORIGINAL)
                .toList();
    }
    
    // Getters
    public String getOriginalFieldName() {
        return originalFieldName;
    }
    
    public String getOriginalFieldType() {
        return originalFieldType;
    }
    
    public int getFieldSize() {
        return fieldSize;
    }
    
    public List<RedefinesView> getViews() {
        return views;
    }
    
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(String.format("Field: %s (%s, %d bytes)\n", 
            originalFieldName, originalFieldType, fieldSize));
        sb.append(String.format("  Redefinitions: %d\n", getRedefinitionCount()));
        
        for (RedefinesView view : getRedefinitions()) {
            sb.append(String.format("    - %s (%s, %s, %d bytes)\n",
                view.getName(), view.getType(), view.getViewType(), view.getSize()));
            
            if (view.isStructured()) {
                for (RedefinesView.SubField sub : view.getSubFields()) {
                    sb.append(String.format("        * %s (%s, %d bytes)\n",
                        sub.getName(), sub.getType(), sub.getSize()));
                }
            }
        }
        
        List<String> issues = analyzeCompatibility();
        if (!issues.isEmpty()) {
            sb.append("  Compatibility Issues:\n");
            for (String issue : issues) {
                sb.append("    ! ").append(issue).append("\n");
            }
        }
        
        return sb.toString();
    }
}
