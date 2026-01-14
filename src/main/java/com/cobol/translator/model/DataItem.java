package com.cobol.translator.model;

/**
 * Represents a COBOL data item (field definition).
 */
public class DataItem {

    private int level;
    private String name;
    private String pictureClause;
    private String usage;
    private String value;
    private boolean isGroup;
    private boolean isFiller;  // True if this is a FILLER field
    private DataItem parent;

    // For OCCURS clause
    private Integer occursCount;
    private String indexedBy;

    // For REDEFINES clause
    private String redefines;

    // For Level-88 condition names
    private boolean isConditionName;  // True if this is an 88-level
    private String conditionValue;    // The VALUE for 88-level
    private DataItem conditionParent; // Parent field for 88-level

    // Computed Java type
    private String javaType;
    private String javaFieldName;

    // Detected pattern (for analysis)
    private String pattern;

    // COBOL section this item belongs to
    private String section;  // "FILE", "WORKING-STORAGE", "LINKAGE", or null

    public DataItem() {
    }

    public DataItem(int level, String name) {
        this.level = level;
        this.name = name;
    }

    /**
     * Determines if this is an elementary item (has PICTURE clause).
     */
    public boolean isElementary() {
        return pictureClause != null && !pictureClause.isEmpty();
    }

    /**
     * Determines if this is a numeric field.
     */
    public boolean isNumeric() {
        if (pictureClause == null) return false;
        String pic = pictureClause.toUpperCase();
        return pic.contains("9") || pic.contains("S9");
    }

    /**
     * Determines if this field has decimal places.
     */
    public boolean hasDecimals() {
        return pictureClause != null && pictureClause.toUpperCase().contains("V");
    }

    /**
     * Determines if this is a COMP-3 (packed decimal) field.
     */
    public boolean isComp3() {
        return usage != null && usage.toUpperCase().contains("COMP-3");
    }

    /**
     * Determines if this is a date field (PIC 9(8) or PIC 9(6)).
     */
    public boolean isPotentialDateField() {
        if (pictureClause == null) return false;
        String pic = pictureClause.toUpperCase();
        return (pic.equals("9(8)") || pic.equals("9(6)")) &&
               (name.toUpperCase().contains("DATE") ||
                name.toUpperCase().contains("DT"));
    }

    // Getters and Setters

    public int getLevel() { return level; }
    public void setLevel(int level) { this.level = level; }

    public String getName() { return name; }
    public void setName(String name) { this.name = name; }

    public String getPictureClause() { return pictureClause; }
    public void setPictureClause(String pictureClause) { this.pictureClause = pictureClause; }

    public String getUsage() { return usage; }
    public void setUsage(String usage) { this.usage = usage; }

    public String getValue() { return value; }
    public void setValue(String value) { this.value = value; }

    public boolean isGroup() { return isGroup; }
    public void setGroup(boolean group) { isGroup = group; }

    public boolean isFiller() { return isFiller; }
    public void setFiller(boolean filler) { isFiller = filler; }

    public DataItem getParent() { return parent; }
    public void setParent(DataItem parent) { this.parent = parent; }

    public Integer getOccursCount() { return occursCount; }
    public void setOccursCount(Integer occursCount) { this.occursCount = occursCount; }

    public String getIndexedBy() { return indexedBy; }
    public void setIndexedBy(String indexedBy) { this.indexedBy = indexedBy; }

    public String getRedefines() { return redefines; }
    public void setRedefines(String redefines) { this.redefines = redefines; }

    public String getJavaType() { return javaType; }
    public void setJavaType(String javaType) { this.javaType = javaType; }

    public String getJavaFieldName() { return javaFieldName; }
    public void setJavaFieldName(String javaFieldName) { this.javaFieldName = javaFieldName; }

    public String getPattern() { return pattern; }
    public void setPattern(String pattern) { this.pattern = pattern; }

    public boolean isConditionName() { return isConditionName; }
    public void setConditionName(boolean conditionName) { isConditionName = conditionName; }

    public String getConditionValue() { return conditionValue; }
    public void setConditionValue(String conditionValue) { this.conditionValue = conditionValue; }

    public DataItem getConditionParent() { return conditionParent; }
    public void setConditionParent(DataItem conditionParent) { this.conditionParent = conditionParent; }

    public String getSection() { return section; }
    public void setSection(String section) { this.section = section; }

    @Override
    public String toString() {
        return "DataItem{" +
                "level=" + level +
                ", name='" + name + '\'' +
                ", picture='" + pictureClause + '\'' +
                ", usage='" + usage + '\'' +
                ", javaType='" + javaType + '\'' +
                '}';
    }
}
