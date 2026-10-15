# 04 - Security modernization

| Legacy | Modern |
|---|---|
| RACF user ID used to sign on to CICS | Cognito user pool user (local development: `POST /api/v1/auth/signon`) |
| USRSEC `SEC-USR-TYPE` = `A` | Cognito group `ADMIN`, JWT claim `userType=A`, Spring role `ROLE_ADMIN` |
| USRSEC `SEC-USR-TYPE` = `U` | Cognito group `USER`, `ROLE_USER` |
| Plain-text `SEC-USR-PWD` | BCrypt locally; Cognito-managed (SRP, MFA) in AWS |
| COADM01C only shown to admins | `@PreAuthorize("hasRole('ADMIN')")` on `/api/v1/users/**` and `/api/v1/batch/**`. The 403 body keeps the legacy text "No access - Admin Only option..." |
| CICS transaction security (RACF TCICSTRN profiles) | API Gateway / ALB authorizer + method security in each service |
| Batch job userids (RACF surrogate) | IAM task roles for ECS tasks and Step Functions |

## Implementation

- `common/security/JwtService` signs and verifies HS256 tokens with claims `sub`, `userType`, `firstName`, `lastName`.
  The secret comes from `CARDDEMO_JWT_SECRET` (at least 32 bytes) and is never committed. On AWS it is injected from
  Secrets Manager.
- `JwtAuthenticationFilter` turns the token into a `CardDemoPrincipal`. `SecurityConfig` makes the services stateless
  (no HTTP session, so no CSRF), and only sign-on, health and API docs are public.
- **Cognito:** switch the services to `spring-boot-starter-oauth2-resource-server` with
  `spring.security.oauth2.resourceserver.jwt.issuer-uri=https://cognito-idp.<region>.amazonaws.com/<pool-id>` and
  map `cognito:groups` to roles. identity-service then only keeps user administration, which calls the Cognito
  AdminCreateUser / AdminUpdateUserAttributes / AdminDeleteUser APIs.
- Migrating users: legacy passwords are plain text, so they can be imported once into Cognito with
  `AdminSetUserPassword`, or all users can be forced to reset on first login (recommended).
