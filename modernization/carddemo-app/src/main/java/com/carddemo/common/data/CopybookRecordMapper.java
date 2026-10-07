package com.carddemo.common.data;

import com.carddemo.common.codec.Copybook;
import com.carddemo.common.codec.Field;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.codec.RecordLayout;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.RecordComponent;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Maps a fixed-width copybook record to a Java record and back. Each record component names its copybook leaf with
 * {@link CobolField}; the binding is checked when the mapper is built: every non-FILLER leaf of the layout must be
 * bound exactly once, with the Java type the schema uses for it (ADR-0003/0004):
 *
 * <ul>
 *   <li>{@code PIC X(n)} → {@code String} (trailing spaces trimmed on read, padded on write) or a {@link CodedEnum}
 *       for a level-88 domain;</li>
 *   <li>{@code PIC 9(n)}, n ≤ 9 → {@code int}; 10 ≤ n ≤ 18 → {@code long};</li>
 *   <li>{@code PIC S9(n)V9(m)} → {@code BigDecimal}.</li>
 * </ul>
 *
 * FILLER bytes are spaces in {@link #toRecord}; {@link #writeInto} leaves them untouched, like a series of COBOL
 * {@code MOVE}s into an existing record area.
 */
public final class CopybookRecordMapper<D extends Record> {

    private enum Kind { TEXT, CODE, INT, LONG, DECIMAL }

    private record Binding(RecordComponent component, String cobolName, Field field, Kind kind) {
    }

    private final Class<D> type;
    private final String copybook;
    private final RecordLayout layout;
    private final Constructor<D> constructor;
    private final List<Binding> bindings;

    private CopybookRecordMapper(Class<D> type, String copybook) {
        this.type = type;
        this.copybook = copybook;
        this.layout = Copybook.layout(copybook);
        Map<String, Field> leaves = new LinkedHashMap<>();
        for (RecordLayout.Leaf leaf : layout.leaves()) {
            if (!isFiller(leaf)) {
                leaves.put(leaf.key(), leaf.field());
            }
        }
        RecordComponent[] components = type.getRecordComponents();
        List<Binding> bound = new ArrayList<>(components.length);
        Set<String> seen = new HashSet<>();
        Class<?>[] parameterTypes = new Class<?>[components.length];
        for (int i = 0; i < components.length; i++) {
            RecordComponent component = components[i];
            parameterTypes[i] = component.getType();
            CobolField cobol = component.getAnnotation(CobolField.class);
            if (cobol == null) {
                throw new IllegalArgumentException(type.getSimpleName() + "." + component.getName()
                        + " has no @CobolField");
            }
            Field field = leaves.get(cobol.value());
            if (field == null || !seen.add(cobol.value())) {
                throw new IllegalArgumentException(type.getSimpleName() + "." + component.getName() + ": "
                        + cobol.value() + (field == null ? " is not a stored leaf of " : " is bound twice in ")
                        + copybook);
            }
            bound.add(new Binding(component, cobol.value(), field, kind(component, field)));
        }
        if (!seen.equals(leaves.keySet())) {
            Set<String> missing = new HashSet<>(leaves.keySet());
            missing.removeAll(seen);
            throw new IllegalArgumentException(type.getSimpleName() + " does not bind " + copybook + " " + missing);
        }
        this.bindings = List.copyOf(bound);
        try {
            this.constructor = type.getDeclaredConstructor(parameterTypes);
        } catch (NoSuchMethodException e) {
            throw new IllegalStateException(type + " has no canonical constructor", e);
        }
    }

    public static <D extends Record> CopybookRecordMapper<D> of(Class<D> type, String copybook) {
        return new CopybookRecordMapper<>(type, copybook);
    }

    public Class<D> type() {
        return type;
    }

    public String copybook() {
        return copybook;
    }

    public RecordLayout layout() {
        return layout;
    }

    /** Copybook leaf name → record component name, in component order. */
    public Map<String, String> fieldToComponent() {
        Map<String, String> map = new LinkedHashMap<>();
        bindings.forEach(b -> map.put(b.cobolName(), b.component().getName()));
        return map;
    }

    public D fromRecord(FixedWidthRecord record) {
        Object[] args = new Object[bindings.size()];
        for (int i = 0; i < args.length; i++) {
            Binding b = bindings.get(i);
            args[i] = switch (b.kind()) {
                case TEXT -> record.getTrimmed(b.field());
                case CODE -> codeOf(b, record.getString(b.field()));
                case INT -> Math.toIntExact(record.getLong(b.field()));
                case LONG -> record.getLong(b.field());
                case DECIMAL -> record.getDecimal(b.field());
            };
        }
        try {
            return constructor.newInstance(args);
        } catch (InvocationTargetException e) {
            if (e.getCause() instanceof RuntimeException runtime) {
                throw runtime;
            }
            throw new IllegalStateException(e.getCause());
        } catch (ReflectiveOperationException e) {
            throw new IllegalStateException(e);
        }
    }

    public List<D> fromRecords(List<FixedWidthRecord> records) {
        return records.stream().map(this::fromRecord).toList();
    }

    public FixedWidthRecord toRecord(D data, RecordEncoding encoding) {
        FixedWidthRecord record = FixedWidthRecord.spaces(layout, encoding);
        writeInto(data, record);
        return record;
    }

    public void writeInto(D data, FixedWidthRecord record) {
        for (Binding b : bindings) {
            Object value = read(b.component(), data);
            switch (b.kind()) {
                case TEXT -> record.setString(b.field(), (String) value);
                case CODE -> record.setString(b.field(), ((CodedEnum) value).code());
                case INT, LONG -> record.setLong(b.field(), ((Number) value).longValue());
                case DECIMAL -> record.setDecimal(b.field(), (BigDecimal) value);
            }
        }
    }

    private static boolean isFiller(RecordLayout.Leaf leaf) {
        return leaf.field().isFiller() || leaf.key().endsWith("-FILLER");
    }

    private Kind kind(RecordComponent component, Field field) {
        Class<?> java = component.getType();
        Kind kind;
        if (!field.isNumeric()) {
            kind = java == String.class ? Kind.TEXT : CodedEnum.class.isAssignableFrom(java) && java.isEnum()
                    ? Kind.CODE : null;
        } else if (field.scale() > 0) {
            kind = java == BigDecimal.class ? Kind.DECIMAL : null;
        } else if (field.digits() <= 9) {
            kind = java == int.class ? Kind.INT : null;
        } else {
            kind = java == long.class ? Kind.LONG : null;
        }
        if (kind == null) {
            throw new IllegalArgumentException(type.getSimpleName() + "." + component.getName() + " (" + java
                    .getSimpleName() + ") cannot hold " + field.name() + " PIC " + field.picture().text());
        }
        return kind;
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    private static Object codeOf(Binding b, String code) {
        return CodedEnum.fromCode((Class) b.component().getType(), code);
    }

    private static Object read(RecordComponent component, Object data) {
        try {
            return component.getAccessor().invoke(data);
        } catch (ReflectiveOperationException e) {
            throw new IllegalStateException(e);
        }
    }
}
