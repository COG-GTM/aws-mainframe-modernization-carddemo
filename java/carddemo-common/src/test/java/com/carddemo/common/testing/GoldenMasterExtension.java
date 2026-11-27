package com.carddemo.common.testing;

import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.ParameterContext;
import org.junit.jupiter.api.extension.ParameterResolver;

/** Injects {@link MainframeSources} and {@link GoldenMaster} into golden-master test methods/constructors. */
public final class GoldenMasterExtension implements ParameterResolver {

    private static final ExtensionContext.Namespace NS = ExtensionContext.Namespace.create(GoldenMasterExtension.class);

    @Override
    public boolean supportsParameter(ParameterContext parameter, ExtensionContext context) {
        Class<?> type = parameter.getParameter().getType();
        return type == MainframeSources.class || type == GoldenMaster.class;
    }

    @Override
    public Object resolveParameter(ParameterContext parameter, ExtensionContext context) {
        ExtensionContext.Store store = context.getRoot().getStore(NS);
        if (parameter.getParameter().getType() == MainframeSources.class) {
            return store.getOrComputeIfAbsent(MainframeSources.class, k -> MainframeSources.locate(), MainframeSources.class);
        }
        return store.getOrComputeIfAbsent(GoldenMaster.class, k -> GoldenMaster.forCurrentModule(), GoldenMaster.class);
    }
}
