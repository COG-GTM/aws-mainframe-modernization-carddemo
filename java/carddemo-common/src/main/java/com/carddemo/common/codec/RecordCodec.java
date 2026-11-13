package com.carddemo.common.codec;

import com.carddemo.common.cobol.CobolDataException;
import com.carddemo.common.cobol.DataEncoding;
import com.carddemo.common.copybook.CopybookField;
import com.carddemo.common.copybook.RecordLayout;
import com.carddemo.common.copybook.RecordView;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.RecordComponent;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

/**
 * Maps fixed-width records to Java {@code record} types annotated with {@link CobolField}, driven by the
 * copybook layout (the copybook stays the single source of truth for offsets and data types).
 *
 * <p>Supported component types: {@code String}, {@code BigDecimal}, {@code long/Long}, {@code int/Integer},
 * {@code short/Short}, nested records (group items) and {@code List<...>} of any of these (OCCURS items).
 * Unbound bytes (FILLER, unmapped items) are initialized to spaces/zero on encode.
 */
public final class RecordCodec<T extends Record> {

    private final RecordLayout layout;
    private final DataEncoding encoding;
    private final Binding<T> binding;

    private RecordCodec(RecordLayout layout, DataEncoding encoding, Binding<T> binding) {
        this.layout = layout;
        this.encoding = encoding;
        this.binding = binding;
    }

    public static <T extends Record> RecordCodec<T> of(Class<T> type, RecordLayout layout, DataEncoding encoding) {
        return new RecordCodec<>(layout, encoding, bind(type, layout.root()));
    }

    public RecordLayout layout() {
        return layout;
    }

    public DataEncoding encoding() {
        return encoding;
    }

    public T decode(byte[] record) {
        return decode(RecordView.wrap(layout, encoding, record));
    }

    public T decode(RecordView view) {
        return binding.decode(view, new int[0]);
    }

    public byte[] encode(T value) {
        RecordView view = RecordView.initialized(layout, encoding);
        binding.encode(value, view, new int[0]);
        return view.bytes();
    }

    private record Component(RecordComponent component, CopybookField field, boolean list, Class<?> type,
                             Binding<?> nested, boolean trim) {
    }

    private record Binding<R extends Record>(Class<R> type, Constructor<R> constructor, List<Component> components) {

        R decode(RecordView view, int[] indices) {
            Object[] args = new Object[components.size()];
            for (int i = 0; i < args.length; i++) {
                Component c = components.get(i);
                if (c.list()) {
                    List<Object> values = new ArrayList<>(c.field().occurs());
                    for (int k = 0; k < c.field().occurs(); k++) {
                        values.add(decodeOne(c, view, append(indices, k)));
                    }
                    args[i] = List.copyOf(values);
                } else {
                    args[i] = decodeOne(c, view, indices);
                }
            }
            try {
                return constructor.newInstance(args);
            } catch (InstantiationException | IllegalAccessException | InvocationTargetException e) {
                throw new CobolDataException("Cannot instantiate " + type.getName(), e);
            }
        }

        void encode(Object value, RecordView view, int[] indices) {
            for (Component c : components) {
                Object v;
                try {
                    v = c.component().getAccessor().invoke(value);
                } catch (IllegalAccessException | InvocationTargetException e) {
                    throw new CobolDataException("Cannot read " + c.component().getName(), e);
                }
                if (v == null) {
                    continue;
                }
                if (c.list()) {
                    List<?> values = (List<?>) v;
                    if (values.size() > c.field().occurs()) {
                        throw new CobolDataException(c.field().name() + " OCCURS " + c.field().occurs()
                                + " cannot hold " + values.size() + " values");
                    }
                    for (int k = 0; k < values.size(); k++) {
                        encodeOne(c, values.get(k), view, append(indices, k));
                    }
                } else {
                    encodeOne(c, v, view, indices);
                }
            }
        }

        private static Object decodeOne(Component c, RecordView view, int[] indices) {
            if (c.nested() != null) {
                return c.nested().decode(view, indices);
            }
            CopybookField f = c.field();
            Class<?> t = c.type();
            if (t == String.class) {
                String s = view.getString(f, indices);
                return c.trim() ? s.stripTrailing() : s;
            }
            BigDecimal d = view.getDecimal(f, indices);
            if (t == BigDecimal.class) {
                return d;
            }
            if (t == Long.class || t == long.class) {
                return d.longValueExact();
            }
            if (t == Integer.class || t == int.class) {
                return d.intValueExact();
            }
            if (t == Short.class || t == short.class) {
                return d.shortValueExact();
            }
            throw new CobolDataException("Unsupported component type " + t.getName());
        }

        private static void encodeOne(Component c, Object value, RecordView view, int[] indices) {
            if (value == null) {
                return;
            }
            if (c.nested() != null) {
                ((Binding<?>) c.nested()).encode(value, view, indices);
                return;
            }
            CopybookField f = c.field();
            if (value instanceof String s) {
                if (f.pic().isNumeric()) {
                    view.setDecimal(f, new BigDecimal(s.strip()), indices);
                } else {
                    view.setString(f, s, indices);
                }
            } else if (value instanceof BigDecimal d) {
                view.setDecimal(f, d, indices);
            } else if (value instanceof Number n) {
                view.setDecimal(f, BigDecimal.valueOf(n.longValue()), indices);
            } else {
                throw new CobolDataException("Unsupported value type " + value.getClass().getName());
            }
        }

        private static int[] append(int[] indices, int index) {
            int[] out = Arrays.copyOf(indices, indices.length + 1);
            out[indices.length] = index;
            return out;
        }
    }

    @SuppressWarnings("unchecked")
    private static <R extends Record> Binding<R> bind(Class<R> type, CopybookField scope) {
        if (!type.isRecord()) {
            throw new IllegalArgumentException(type.getName() + " is not a record");
        }
        RecordComponent[] rcs = type.getRecordComponents();
        List<Component> components = new ArrayList<>();
        for (RecordComponent rc : rcs) {
            CobolField ann = rc.getAnnotation(CobolField.class);
            if (ann == null) {
                throw new IllegalArgumentException(type.getSimpleName() + "." + rc.getName() + " lacks @CobolField");
            }
            CopybookField field = findIn(scope, ann.value()).orElseThrow(() -> new IllegalArgumentException(
                    "No copybook item " + ann.value() + " under " + scope.name() + " for " + type.getSimpleName()
                            + "." + rc.getName()));
            boolean list = List.class.isAssignableFrom(rc.getType());
            Class<?> valueType = list
                    ? (Class<?>) ((ParameterizedType) rc.getGenericType()).getActualTypeArguments()[0]
                    : rc.getType();
            if (list != field.occurs() > 1) {
                throw new IllegalArgumentException(type.getSimpleName() + "." + rc.getName()
                        + (list ? " is a List but " : " is not a List but ") + field.name() + " OCCURS " + field.occurs());
            }
            Binding<?> nested = null;
            if (valueType.isRecord()) {
                if (!field.isGroup()) {
                    throw new IllegalArgumentException(field.name() + " is elementary; cannot bind record "
                            + valueType.getSimpleName());
                }
                nested = bind((Class<? extends Record>) valueType, field);
            } else if (field.isGroup() && valueType != String.class) {
                throw new IllegalArgumentException("Group " + field.name() + " can only bind to a record or String");
            }
            components.add(new Component(rc, field, list, valueType, nested, ann.trim()));
        }
        Class<?>[] types = Arrays.stream(rcs).map(RecordComponent::getType).toArray(Class<?>[]::new);
        try {
            Constructor<R> constructor = type.getDeclaredConstructor(types);
            constructor.setAccessible(true);
            return new Binding<>(type, constructor, List.copyOf(components));
        } catch (NoSuchMethodException e) {
            throw new IllegalArgumentException("No canonical constructor on " + type.getName(), e);
        }
    }

    private static Optional<CopybookField> findIn(CopybookField scope, String name) {
        String wanted = name.toUpperCase(java.util.Locale.ROOT);
        for (CopybookField child : scope.children()) {
            if (child.name().equals(wanted)) {
                return Optional.of(child);
            }
            Optional<CopybookField> deeper = findIn(child, wanted);
            if (deeper.isPresent()) {
                return deeper;
            }
        }
        return Optional.empty();
    }
}
