package com.carddemo.common.web;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.common.AbendException;
import com.carddemo.common.DuplicateRecordException;
import com.carddemo.common.InvalidRequestException;
import com.carddemo.common.RecordNotFoundException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

class CicsResponseExceptionHandlerTest {

    private MockMvc mvc;

    @BeforeEach
    void setUp() {
        mvc = MockMvcBuilders.standaloneSetup(new ThrowingController())
                .setControllerAdvice(new CicsResponseExceptionHandler())
                .build();
    }

    @Test
    void notfndIs404() throws Exception {
        mvc.perform(get("/notfnd"))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.cicsResp").value("NOTFND"))
                .andExpect(jsonPath("$.detail").value("Did not find this account in account card xref file"));
    }

    @Test
    void duprecIs409() throws Exception {
        mvc.perform(get("/duprec"))
                .andExpect(status().isConflict())
                .andExpect(jsonPath("$.cicsResp").value("DUPREC"));
    }

    @Test
    void dupkeyIs409AndKeepsItsCondition() throws Exception {
        mvc.perform(get("/dupkey"))
                .andExpect(status().isConflict())
                .andExpect(jsonPath("$.cicsResp").value("DUPKEY"));
    }

    @Test
    void invreqIs400() throws Exception {
        mvc.perform(get("/invreq"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.cicsResp").value("INVREQ"));
    }

    @Test
    void optimisticLockConflictIs409WithTheCobolMessage() throws Exception {
        mvc.perform(get("/changed"))
                .andExpect(status().isConflict())
                .andExpect(jsonPath("$.cicsResp").value("CHANGED"))
                .andExpect(jsonPath("$.detail").value("Record changed by some one else. Please review"));
    }

    @Test
    void abendIs500AndKeepsTheAbendCode() throws Exception {
        mvc.perform(get("/abend"))
                .andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.cicsResp").value("ABEND"))
                .andExpect(jsonPath("$.abendCode").value("U0999"));
    }

    @RestController
    static class ThrowingController {

        @GetMapping("/notfnd")
        void notfnd() {
            throw new RecordNotFoundException("Did not find this account in account card xref file");
        }

        @GetMapping("/duprec")
        void duprec() {
            throw new DuplicateRecordException("User ID already exist...");
        }

        @GetMapping("/dupkey")
        void dupkey() {
            throw DuplicateRecordException.duplicateKey("Alternate key already exists");
        }

        @GetMapping("/invreq")
        void invreq() {
            throw new InvalidRequestException("Invalid key pressed");
        }

        @GetMapping("/changed")
        void changed() {
            throw new ObjectOptimisticLockingFailureException(Object.class, 1L);
        }

        @GetMapping("/abend")
        void abend() {
            throw AbendException.carddemo("ERROR READING ACCOUNT FILE", null);
        }
    }
}
