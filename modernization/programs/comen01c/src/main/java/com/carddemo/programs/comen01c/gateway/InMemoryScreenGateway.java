package com.carddemo.programs.comen01c.gateway;

import com.carddemo.programs.comen01c.dto.MenuScreenRequest;
import com.carddemo.programs.comen01c.dto.MenuScreenResponse;

import java.util.ArrayList;
import java.util.List;

/** Terminal stand-in: returns a preset input for RECEIVE and records every SEND. */
public final class InMemoryScreenGateway implements ScreenGateway {

    private final MenuScreenRequest input;
    private final List<MenuScreenResponse> sent = new ArrayList<>();

    public InMemoryScreenGateway() {
        this(MenuScreenRequest.notTransmitted());
    }

    public InMemoryScreenGateway(MenuScreenRequest input) {
        this.input = input;
    }

    @Override
    public MenuScreenRequest receive() {
        return input;
    }

    @Override
    public void send(MenuScreenResponse screen) {
        sent.add(screen);
    }

    public List<MenuScreenResponse> sent() {
        return List.copyOf(sent);
    }

    public MenuScreenResponse lastSent() {
        return sent.isEmpty() ? null : sent.get(sent.size() - 1);
    }
}
