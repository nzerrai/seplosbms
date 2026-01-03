package com.cobol.translator.jcl.model;

import java.util.ArrayList;
import java.util.List;

/**
 * Represents a DD (Data Definition) Statement in JCL
 */
public class DDStatement {

    private String ddName;
    private String datasetName;
    private Disposition disposition;
    private DCBInfo dcbInfo;
    private SpaceAllocation spaceAllocation;
    private String unit;
    private String volser;
    private String sysoutClass;
    private boolean isDummy;
    private List<JCLParameter> otherParameters;

    public DDStatement() {
        this.otherParameters = new ArrayList<>();
        this.dcbInfo = new DCBInfo();
    }

    public DDStatement(String ddName) {
        this();
        this.ddName = ddName;
    }

    public void addOtherParameter(JCLParameter parameter) {
        this.otherParameters.add(parameter);
    }

    // Getters and Setters

    public String getDdName() {
        return ddName;
    }

    public void setDdName(String ddName) {
        this.ddName = ddName;
    }

    public String getDatasetName() {
        return datasetName;
    }

    public void setDatasetName(String datasetName) {
        this.datasetName = datasetName;
    }

    public Disposition getDisposition() {
        return disposition;
    }

    public void setDisposition(Disposition disposition) {
        this.disposition = disposition;
    }

    public DCBInfo getDcbInfo() {
        return dcbInfo;
    }

    public void setDcbInfo(DCBInfo dcbInfo) {
        this.dcbInfo = dcbInfo;
    }

    public SpaceAllocation getSpaceAllocation() {
        return spaceAllocation;
    }

    public void setSpaceAllocation(SpaceAllocation spaceAllocation) {
        this.spaceAllocation = spaceAllocation;
    }

    public String getUnit() {
        return unit;
    }

    public void setUnit(String unit) {
        this.unit = unit;
    }

    public String getVolser() {
        return volser;
    }

    public void setVolser(String volser) {
        this.volser = volser;
    }

    public String getSysoutClass() {
        return sysoutClass;
    }

    public void setSysoutClass(String sysoutClass) {
        this.sysoutClass = sysoutClass;
    }

    public boolean isDummy() {
        return isDummy;
    }

    public void setDummy(boolean dummy) {
        isDummy = dummy;
    }

    public List<JCLParameter> getOtherParameters() {
        return otherParameters;
    }

    public void setOtherParameters(List<JCLParameter> otherParameters) {
        this.otherParameters = otherParameters;
    }

    /**
     * Check if this DD represents an input file
     */
    public boolean isInputFile() {
        return disposition != null &&
               (disposition.getStatus() == DispositionStatus.OLD ||
                disposition.getStatus() == DispositionStatus.SHR);
    }

    /**
     * Check if this DD represents an output file
     */
    public boolean isOutputFile() {
        return disposition != null &&
               (disposition.getStatus() == DispositionStatus.NEW ||
                disposition.getStatus() == DispositionStatus.MOD);
    }

    @Override
    public String toString() {
        return "DDStatement{" +
                "ddName='" + ddName + '\'' +
                ", datasetName='" + datasetName + '\'' +
                ", disposition=" + disposition +
                ", isDummy=" + isDummy +
                '}';
    }

    /**
     * Disposition information
     */
    public static class Disposition {
        private DispositionStatus status;
        private DispositionAction normalEnd;
        private DispositionAction abnormalEnd;

        public Disposition() {
        }

        public Disposition(DispositionStatus status) {
            this.status = status;
        }

        public DispositionStatus getStatus() {
            return status;
        }

        public void setStatus(DispositionStatus status) {
            this.status = status;
        }

        public DispositionAction getNormalEnd() {
            return normalEnd;
        }

        public void setNormalEnd(DispositionAction normalEnd) {
            this.normalEnd = normalEnd;
        }

        public DispositionAction getAbnormalEnd() {
            return abnormalEnd;
        }

        public void setAbnormalEnd(DispositionAction abnormalEnd) {
            this.abnormalEnd = abnormalEnd;
        }

        @Override
        public String toString() {
            return "(" + status +
                   (normalEnd != null ? "," + normalEnd : "") +
                   (abnormalEnd != null ? "," + abnormalEnd : "") + ")";
        }
    }

    public enum DispositionStatus {
        NEW, OLD, SHR, MOD
    }

    public enum DispositionAction {
        CATLG, DELETE, KEEP, PASS
    }

    /**
     * DCB (Data Control Block) information
     */
    public static class DCBInfo {
        private String recfm;  // Record format: FB, VB, etc.
        private Integer lrecl; // Logical record length
        private Integer blksize; // Block size
        private String dsorg;  // Dataset organization: PS, PO, etc.

        public String getRecfm() {
            return recfm;
        }

        public void setRecfm(String recfm) {
            this.recfm = recfm;
        }

        public Integer getLrecl() {
            return lrecl;
        }

        public void setLrecl(Integer lrecl) {
            this.lrecl = lrecl;
        }

        public Integer getBlksize() {
            return blksize;
        }

        public void setBlksize(Integer blksize) {
            this.blksize = blksize;
        }

        public String getDsorg() {
            return dsorg;
        }

        public void setDsorg(String dsorg) {
            this.dsorg = dsorg;
        }

        @Override
        public String toString() {
            return "DCBInfo{" +
                    "recfm='" + recfm + '\'' +
                    ", lrecl=" + lrecl +
                    ", blksize=" + blksize +
                    ", dsorg='" + dsorg + '\'' +
                    '}';
        }
    }

    /**
     * Space allocation information
     */
    public static class SpaceAllocation {
        private String unit; // TRK, CYL, BLKS
        private Integer primary;
        private Integer secondary;
        private Integer directory;

        public String getUnit() {
            return unit;
        }

        public void setUnit(String unit) {
            this.unit = unit;
        }

        public Integer getPrimary() {
            return primary;
        }

        public void setPrimary(Integer primary) {
            this.primary = primary;
        }

        public Integer getSecondary() {
            return secondary;
        }

        public void setSecondary(Integer secondary) {
            this.secondary = secondary;
        }

        public Integer getDirectory() {
            return directory;
        }

        public void setDirectory(Integer directory) {
            this.directory = directory;
        }

        @Override
        public String toString() {
            return "SpaceAllocation{" +
                    "unit='" + unit + '\'' +
                    ", primary=" + primary +
                    ", secondary=" + secondary +
                    ", directory=" + directory +
                    '}';
        }
    }
}
