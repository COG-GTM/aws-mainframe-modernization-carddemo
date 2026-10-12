package com.carddemo.zerotrust.policy;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * Loads the YAML policy document into a {@link Policy}. Loading fails closed: any dangling
 * reference (role granting an undefined permission, an assignment naming an undefined role,
 * a constraint naming an undefined permission) is an error at load time, not a silent deny
 * at decision time.
 */
public final class PolicyLoader {

    private PolicyLoader() {
    }

    public static Policy load(Path path) {
        try (InputStream in = Files.newInputStream(path)) {
            return load(in);
        } catch (IOException e) {
            throw new UncheckedIOException("cannot read policy " + path, e);
        }
    }

    public static Policy load(InputStream in) {
        JsonNode root;
        try {
            root = new ObjectMapper(new YAMLFactory()).readTree(in);
        } catch (IOException e) {
            throw new PolicyException("malformed policy document: " + e.getMessage());
        }
        if (root == null) {
            throw new PolicyException("empty policy document");
        }
        Policy policy = parse(root);
        validate(policy);
        return policy;
    }

    private static Policy parse(JsonNode root) {
        Set<String> permissions = stringSet(root.required("permissions"), "permissions");

        Map<String, Set<String>> roles = new LinkedHashMap<>();
        root.path("roles").fields().forEachRemaining(e -> roles.put(e.getKey(), stringSet(e.getValue(), "roles." + e.getKey())));

        JsonNode subjects = root.path("subjects");
        Map<String, Set<String>> userTypes = new LinkedHashMap<>();
        Map<String, Set<String>> users = new LinkedHashMap<>();
        Map<String, Set<String>> workloads = new LinkedHashMap<>();
        subjects.path("userTypes").fields().forEachRemaining(e -> userTypes.put(e.getKey(), stringSet(e.getValue(), "subjects.userTypes." + e.getKey())));
        subjects.path("users").fields().forEachRemaining(e -> users.put(e.getKey(), stringSet(e.getValue(), "subjects.users." + e.getKey())));
        subjects.path("workloads").fields().forEachRemaining(e -> workloads.put(e.getKey(), stringSet(e.getValue(), "subjects.workloads." + e.getKey())));

        Map<String, Policy.Constraint> constraints = new LinkedHashMap<>();
        root.path("constraints").fields().forEachRemaining(e -> {
            JsonNode n = e.getValue();
            Optional<SubjectKind> kind = n.hasNonNull("subjectKind")
                    ? Optional.of(SubjectKind.valueOf(n.get("subjectKind").asText())) : Optional.empty();
            Optional<String> sod = n.hasNonNull("separationOfDuties")
                    ? Optional.of(n.get("separationOfDuties").asText()) : Optional.empty();
            constraints.put(e.getKey(), new Policy.Constraint(kind, sod));
        });

        return new Policy(permissions, roles, userTypes, users, workloads, constraints);
    }

    private static void validate(Policy p) {
        p.roles().forEach((role, grants) -> grants.forEach(permission -> {
            if (!p.permissions().contains(permission)) {
                throw new PolicyException("role '" + role + "' grants undefined permission '" + permission + "'");
            }
        }));
        validateAssignments("userTypes", p.userTypes(), p);
        validateAssignments("users", p.users(), p);
        validateAssignments("workloads", p.workloads(), p);
        p.constraints().keySet().forEach(permission -> {
            if (!p.permissions().contains(permission)) {
                throw new PolicyException("constraint references undefined permission '" + permission + "'");
            }
        });
    }

    private static void validateAssignments(String section, Map<String, Set<String>> assignments, Policy p) {
        assignments.forEach((subject, roles) -> roles.forEach(role -> {
            if (!p.roles().containsKey(role)) {
                throw new PolicyException(section + " assignment for '" + subject + "' names undefined role '" + role + "'");
            }
        }));
    }

    private static Set<String> stringSet(JsonNode node, String where) {
        if (!node.isArray()) {
            throw new PolicyException("expected a list at '" + where + "'");
        }
        Set<String> out = new LinkedHashSet<>();
        node.forEach(n -> out.add(n.asText()));
        return out;
    }

    /** Thrown when the policy document cannot be trusted; callers fail closed. */
    public static final class PolicyException extends RuntimeException {
        public PolicyException(String message) {
            super(message);
        }
    }
}
