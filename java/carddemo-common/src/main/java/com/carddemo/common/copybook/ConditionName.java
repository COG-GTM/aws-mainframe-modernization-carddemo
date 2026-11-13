package com.carddemo.common.copybook;

import java.util.List;

/** A level-88 condition name, e.g. {@code 88 CDEMO-USRTYP-ADMIN VALUE 'A'}. Values keep their source form. */
public record ConditionName(String name, List<String> values) {
}
