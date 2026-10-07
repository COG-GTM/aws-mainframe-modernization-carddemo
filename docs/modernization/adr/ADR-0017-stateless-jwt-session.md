# ADR-0017: COMMAREA user fields → stateless HS256 JWT; web layer above the domains

- Status: Accepted (UNT51-17, 2026-10-07); supersedes the "server-side session" part of ADR-0007
- Applies to: `modernization/carddemo-app`, packages `com.carddemo.web..`, `com.carddemo.user.signon`, `com.carddemo.user.menu`

## Context
COSGN00C puts `CDEMO-USER-ID` / `CDEMO-USER-TYPE` into the COMMAREA, and every later program trusts them. ADR-0007
moves them into an authenticated principal but assumed a server-side session. The application is a single,
horizontally scalable Spring Boot service whose online API is consumed by a separate web UI (`d-ui`); sticky or
replicated HTTP sessions add infrastructure for no functional gain.

## Decision
- **Sign-on** `POST /api/v1/auth/login` returns a bearer JWT. HS256, signed with `carddemo.security.jwt.secret`
  = env `CARDDEMO_JWT_SECRET` (at least 32 bytes). Claims: `iss=carddemo`, `sub` = user id, `role` = `ADMIN`/`USER`
  (the `UserType` enum, ADR-0006), `usrType` = level-88 code, `iat`, `exp` (`CARDDEMO_JWT_TTL`, default 1 hour).
- **No default secret** except the `local` and `test` profiles (same pattern as `CARDDEMO_DB_PASSWORD`); in any
  other profile the web application refuses to start without a key. The security configuration is only active in a
  servlet web application, so batch CLI runs need no key.
- **Stateless**: `SessionCreationPolicy.STATELESS`, no cookies, CSRF off (no ambient credentials), no form/basic
  login. Sign-off (PF3) is client-side (discard the token); there is no server-side revocation list.
- **Authorization**: anonymous = `/api/v1/auth/**`, `GET /actuator/health/**` + `/actuator/info` (compose gate),
  OpenAPI/Swagger UI. Admin programs (`/api/v1/menu/admin/**`, later `COUSR0*C`) need role `ADMIN`; everything else
  needs a valid token. User id and type always come from the verified token, never from the request.
- **Failures** use the ADR-0019 body: no/invalid/expired token → 401 `SIGNON_REQUIRED` with `toProgram=COSGN00C`
  (a program entered without COMMAREA returns to sign-on); wrong role → 403 `NOTAUTH` with the COMEN01C text
  `No access - Admin Only option...`.
- **Layering**: controllers, security, request/response DTOs and `NavigationContext` live in `com.carddemo.web..`.
  Program logic (edits, messages, routing, option tables) stays in the domain (`com.carddemo.user.signon`,
  `com.carddemo.user.menu`) and CICS estate facts in `com.carddemo.common.online`. `web` may depend on `common` and
  every domain; no domain and not `common` may depend on `web` (ArchUnit `noDomainDependsOnWeb`).

## Consequences
- Tokens cannot be revoked before `exp`; keep the TTL short. Rotating `CARDDEMO_JWT_SECRET` signs everyone off.
- Changing a user's type (COUSR02C) takes effect at the next sign-on.
