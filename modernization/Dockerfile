# syntax=docker/dockerfile:1.7
# CardDemo application image. Build context is the repository root, because the codec loads the legacy
# copybooks from app/cpy (see carddemo-app/pom.xml <resources>):
#   docker build -f modernization/Dockerfile .        (docker compose does this for you)

FROM maven:3.9-eclipse-temurin-21 AS build
# Optional Maven Central mirror (e.g. when repo.maven.apache.org rate-limits with 429); empty = Central.
ARG MAVEN_MIRROR_URL=""
RUN if [ -n "$MAVEN_MIRROR_URL" ]; then \
      printf '<settings><mirrors><mirror><id>mirror</id><mirrorOf>central</mirrorOf><url>%s</url></mirror></mirrors></settings>' \
        "$MAVEN_MIRROR_URL" > /usr/share/maven/conf/settings.xml; \
    fi
WORKDIR /src/modernization
COPY modernization/pom.xml ./pom.xml
COPY modernization/carddemo-app/pom.xml ./carddemo-app/pom.xml
RUN --mount=type=cache,target=/root/.m2 mvn -B -q -ntp dependency:go-offline -DexcludeReactor=true || true
COPY app/cpy /src/app/cpy
COPY modernization/carddemo-app/src ./carddemo-app/src
RUN --mount=type=cache,target=/root/.m2 mvn -B -ntp -DskipTests package \
    && cp carddemo-app/target/carddemo-app.jar /tmp/app.jar

FROM eclipse-temurin:21-jre
RUN groupadd --system carddemo && useradd --system --gid carddemo --home-dir /app carddemo
WORKDIR /app
COPY --from=build /tmp/app.jar /app/carddemo-app.jar
# Sample VSAM images for the initial-load job (local profile); read-only, owned by root.
COPY app/data/EBCDIC /app/data/EBCDIC
RUN mkdir -p /app/batch-output && chown carddemo:carddemo /app/batch-output
ENV CARDDEMO_INITIAL_LOAD_SOURCE_DIR=/app/data/EBCDIC \
    CARDDEMO_BATCH_OUTPUT_DIR=/app/batch-output
USER carddemo
EXPOSE 8080
ENV JAVA_TOOL_OPTIONS="-XX:MaxRAMPercentage=75"
HEALTHCHECK --interval=10s --timeout=3s --start-period=60s --retries=6 \
    CMD curl -fsS http://localhost:8080/actuator/health/readiness || exit 1
ENTRYPOINT ["java", "-jar", "/app/carddemo-app.jar"]
