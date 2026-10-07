package com.carddemo.common.data;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.account.AccountStatus;
import com.carddemo.account.AccountStatusConverter;
import com.carddemo.card.CardStatus;
import com.carddemo.common.InvalidRequestException;
import com.carddemo.customer.PrimaryCardHolder;
import com.carddemo.user.UserType;
import com.carddemo.user.UserTypeConverter;
import org.junit.jupiter.api.Test;

class CodedEnumTest {

    @Test
    void constantsCarryTheLevelEightyEightValues() {
        assertThat(UserType.fromCode("A")).isEqualTo(UserType.ADMIN);
        assertThat(UserType.fromCode("U")).isEqualTo(UserType.USER);
        assertThat(AccountStatus.fromCode("Y")).isEqualTo(AccountStatus.ACTIVE);
        assertThat(AccountStatus.fromCode("N")).isEqualTo(AccountStatus.INACTIVE);
        assertThat(CardStatus.fromCode("Y")).isEqualTo(CardStatus.ACTIVE);
        assertThat(PrimaryCardHolder.fromCode("N")).isEqualTo(PrimaryCardHolder.NO);
    }

    @Test
    void undefinedCodesAreInvalidRequests() {
        assertThatThrownBy(() -> UserType.fromCode("u")).isInstanceOf(InvalidRequestException.class);
        assertThatThrownBy(() -> AccountStatus.fromCode(" ")).isInstanceOf(InvalidRequestException.class);
        assertThatThrownBy(() -> CardStatus.fromCode("")).isInstanceOf(InvalidRequestException.class);
    }

    @Test
    void convertersPersistTheCodeNotTheNameOrOrdinal() {
        UserTypeConverter users = new UserTypeConverter();
        assertThat(users.convertToDatabaseColumn(UserType.ADMIN)).isEqualTo("A");
        assertThat(users.convertToEntityAttribute("U")).isEqualTo(UserType.USER);
        assertThat(users.convertToDatabaseColumn(null)).isNull();
        assertThat(new AccountStatusConverter().convertToDatabaseColumn(AccountStatus.INACTIVE)).isEqualTo("N");
        assertThatThrownBy(() -> users.convertToEntityAttribute("ADMIN")).isInstanceOf(InvalidRequestException.class);
    }
}
