package com.carddemo.common.copybook;

import com.carddemo.common.cobol.PicClause;
import com.carddemo.common.cobol.SignMode;
import com.carddemo.common.cobol.Usage;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * One data description entry of a copybook (group or elementary item) with its resolved storage position.
 * Offsets are absolute within the 01-level record and refer to the first occurrence of any enclosing OCCURS.
 */
public final class CopybookField {

    private final int level;
    private final String name;
    private final PicClause pic;
    private final Usage usage;
    private final SignMode signMode;
    private final int occurs;
    private final String dependingOn;
    private final String redefines;
    private final List<CopybookField> children = new ArrayList<>();
    private final List<ConditionName> conditions = new ArrayList<>();
    private CopybookField parent;
    private int offset;
    private int length;

    CopybookField(int level, String name, PicClause pic, Usage usage, SignMode signMode, int occurs,
                  String dependingOn, String redefines) {
        this.level = level;
        this.name = name;
        this.pic = pic;
        this.usage = usage;
        this.signMode = signMode;
        this.occurs = occurs;
        this.dependingOn = dependingOn;
        this.redefines = redefines;
    }

    public int level() {
        return level;
    }

    public String name() {
        return name;
    }

    public boolean isFiller() {
        return "FILLER".equals(name);
    }

    public boolean isGroup() {
        return pic == null;
    }

    public PicClause pic() {
        return pic;
    }

    public Usage usage() {
        return usage;
    }

    public SignMode signMode() {
        return signMode;
    }

    /** OCCURS count (maximum for OCCURS ... DEPENDING ON); 1 when the item does not repeat. */
    public int occurs() {
        return occurs;
    }

    public String dependingOn() {
        return dependingOn;
    }

    public String redefines() {
        return redefines;
    }

    /** True when this item, or one of its ancestors, is a REDEFINES alternate view of storage. */
    public boolean isAlternateView() {
        for (CopybookField f = this; f != null; f = f.parent) {
            if (f.redefines != null && f.parent != null) {
                return true;
            }
        }
        return false;
    }

    public List<CopybookField> children() {
        return Collections.unmodifiableList(children);
    }

    public List<ConditionName> conditions() {
        return Collections.unmodifiableList(conditions);
    }

    public CopybookField parent() {
        return parent;
    }

    public int offset() {
        return offset;
    }

    /** Storage length of one occurrence. */
    public int length() {
        return length;
    }

    /** Storage length of all occurrences. */
    public int totalLength() {
        return length * occurs;
    }

    /** Dotted path from the 01 level, e.g. {@code TRAN-CAT-BAL-RECORD.TRAN-CAT-KEY.TRANCAT-ACCT-ID}. */
    public String path() {
        return parent == null ? name : parent.path() + "." + name;
    }

    /** Enclosing OCCURS items, outermost first (including this item when it repeats). */
    public List<CopybookField> occursAncestors() {
        List<CopybookField> list = new ArrayList<>();
        for (CopybookField f = this; f != null; f = f.parent) {
            if (f.occurs > 1) {
                list.add(0, f);
            }
        }
        return list;
    }

    void addChild(CopybookField child) {
        child.parent = this;
        children.add(child);
    }

    void addCondition(ConditionName condition) {
        conditions.add(condition);
    }

    void position(int offset, int length) {
        this.offset = offset;
        this.length = length;
    }

    @Override
    public String toString() {
        return String.format("%02d %s @%d len=%d%s%s%s", level, name, offset, length,
                pic == null ? "" : " PIC " + pic.source(),
                usage == Usage.DISPLAY ? "" : " " + usage,
                occurs > 1 ? " OCCURS " + occurs : "");
    }
}
