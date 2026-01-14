package com.cobol.translator.redefines;

/**
 * Énumération des types de vues pour les redéfinitions COBOL
 */
public enum ViewType {
    /**
     * Vue originale (champ de base)
     */
    ORIGINAL,
    
    /**
     * Vue alphanumérique (PIC X)
     */
    ALPHANUMERIC,
    
    /**
     * Vue numérique (PIC 9)
     */
    NUMERIC,
    
    /**
     * Vue structurée (contient des sous-champs)
     */
    STRUCTURED,
    
    /**
     * Vue binaire (données brutes)
     */
    BINARY
}
