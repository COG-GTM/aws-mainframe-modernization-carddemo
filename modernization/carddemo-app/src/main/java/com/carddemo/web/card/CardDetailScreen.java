package com.carddemo.web.card;

import com.carddemo.card.Card;
import com.carddemo.card.online.CardChanges;
import com.carddemo.web.NavigationContext;
import com.carddemo.web.ScreenHeader;
import io.swagger.v3.oas.annotations.media.Schema;

/**
 * Map CCRDSLA (COCRDSLC) / CCRDUPA (COCRDUPC) with a card shown. The full card number stays here because the COBOL
 * screens display it (ADR-0020).
 */
@Schema(description = "Card detail (COCRDSLC): the full card number is shown, as on CCRDSLA")
public record CardDetailScreen(
        ScreenHeader header,
        @Schema(description = "INFOMSG", example = "   Displaying requested details") String infoMessage,
        @Schema(description = "ERRMSG", example = "") String message,
        @Schema(description = "ACCTSID: the card's account, 11 digits", example = "00000000050") String accountId,
        @Schema(description = "CARDSID: full card number", example = "0500024453765740") String cardNumber,
        @Schema(description = "Opaque reference to the card (ADR-0020)") String cardRef,
        @Schema(description = "CRDNAME: CARD-EMBOSSED-NAME", example = "Aniya Von") String embossedName,
        @Schema(description = "EXPMON", example = "03") String expiryMonth,
        @Schema(description = "EXPYEAR", example = "2023") String expiryYear,
        @Schema(description = "CRDSTCD", example = "Y") String activeStatus,
        @Schema(description = "CARD version: send it back in the PUT", example = "0") long version,
        @Schema(description = "PUT body prefilled like CCRDUPA after the read (name upper-cased, COCRDUPC R-25)")
        CardUpdateRequest updateForm,
        @Schema(description = "PF3 target") NavigationContext exit) {

    static CardDetailScreen of(ScreenHeader header, String infoMessage, String message, Card card, String cardRef,
            NavigationContext exit) {
        String expiry = card.getExpirationDate();
        String accountId = String.format("%011d", card.getAcctId());
        CardChanges old = CardChanges.fetched(card);
        CardUpdateRequest form = new CardUpdateRequest(accountId, card.getVersion(), false, old.embossedName(),
                old.activeStatus(), old.expiryMonth(), old.expiryYear());
        return new CardDetailScreen(header, infoMessage, message, accountId, card.getCardNum(), cardRef,
                card.getEmbossedName(), expiry.substring(5, 7), expiry.substring(0, 4), card.getActiveStatus().code(),
                card.getVersion(), form, exit);
    }
}
